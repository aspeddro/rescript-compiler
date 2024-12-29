open Node

module Docgen = RescriptTools.Docgen

type example = {
  id: string,
  kind: string,
  name: string,
  docstrings: array<string>,
}

// Only major version
let nodeVersion =
  Process.version
  ->String.replace("v", "")
  ->String.split(".")
  ->Array.get(0)
  ->Option.getExn(~message="Failed to find major version of Node")
  ->Int.fromString
  ->Option.getExn(~message="Failed to convert node version to Int")

let ignoreRuntimeTests = [
  (
    // Ignore some tests not supported by node v18
    18,
    [
      "Array.toReversed",
      "Array.toSorted",
      "Promise.withResolvers",
      "Set.union",
      "Set.isSupersetOf",
      "Set.isSubsetOf",
      "Set.isDisjointFrom",
      "Set.intersection",
      "Set.symmetricDifference",
      "Set.difference",
    ],
  ),
]

let getOutput = buffer =>
  buffer
  ->Array.map(e => e->Buffer.toString)
  ->Array.join("")

let extractDocFromFile = async file => {
  let toolsBin = Path.join([Process.cwd(), "cli", "rescript-tools"])

  let {stdout} = await SpawnAsync.run(~command=toolsBin, ~args=["doc", file])

  try {
    stdout
    ->getOutput
    ->JSON.parseExn
    ->Docgen.decodeFromJson
  } catch {
  | Exn.Error(_) => Error.panic(`Failed to generate docstrings from ${file}`)
  | _ => assert(false)
  }
}

let getExamples = ({items}: Docgen.doc) => {
  let rec loop = (items: list<Docgen.item>, acc: list<example>) => {
    switch items {
    | list{Value({docstrings, id, name}), ...rest} =>
      loop(rest, list{{id, name, docstrings, kind: "value"}, ...acc})
    | list{Type({docstrings, id, name}), ...rest} =>
      loop(rest, list{{id, name, docstrings, kind: "type"}, ...acc})
    | list{Module({id, name, docstrings, items}), ...rest} =>
      loop(
        list{...rest, ...List.fromArray(items)},
        list{{id, name, docstrings, kind: "module"}, ...acc},
      )
    | list{ModuleType({id, name, docstrings, items}), ...rest} =>
      loop(
        list{...rest, ...List.fromArray(items)},
        list{{id, name, docstrings, kind: "moduleType"}, ...acc},
      )
    | list{ModuleAlias({id, name, docstrings, items}), ...rest} =>
      loop(
        list{...rest, ...List.fromArray(items)},
        list{{id, name, docstrings, kind: "moduleAlias"}, ...acc},
      )
    | list{} => acc
    }
  }

  items
  ->List.fromArray
  ->loop(list{})
  ->List.toArray
  ->Array.filter(({docstrings}) => Array.length(docstrings) > 0)
}

let getCodeBlocks = example => {
  let rec loopEndCodeBlock = (lines, acc) => {
    switch lines {
    | list{hd, ...rest} =>
      if (
        hd
        ->String.trim
        ->String.endsWith("```")
      ) {
        acc
      } else {
        loopEndCodeBlock(rest, list{hd, ...acc})
      }
    | list{} => panic(`Failed to find end of code block for ${example.kind}: ${example.id}`)
    }
  }

  let rec loop = (lines: list<string>, acc: list<string>) => {
    switch lines {
    | list{hd, ...rest} =>
      switch hd
      ->String.trim
      ->String.startsWith("```res") {
      | true =>
        let code = loopEndCodeBlock(rest, list{})
        loop(
          rest,
          list{
            code
            ->List.reverse
            ->List.toArray
            ->Array.join("\n"),
            ...acc,
          },
        )
      | false => loop(rest, acc)
      }
    | list{} => acc
    }
  }

  example.docstrings
  ->Array.reduce([], (acc, docstring) => acc->Array.concat(docstring->String.split("\n")))
  ->List.fromArray
  ->loop(list{})
  ->List.toArray
  ->Belt.Array.reverse
  ->Array.join("\n\n")
}

let batchSize = OS.cpus()->Array.length

let extractExamples = async () => {
  let files = Fs.readdirSync("runtime")

  let docFiles = files->Array.filter(f =>
    switch f {
    // Ignore Js modules and RescriptTools for now
    | f if f->String.startsWith("Js") || f->String.startsWith("RescriptTools") => false
    | f if f->String.endsWith(".resi") => true
    | f if f->String.endsWith(".res") && !(files->Array.includes(f ++ "i")) => true
    | _ => false
    }
  )

  Console.log(`Extracting examples from ${docFiles->Array.length->Int.toString} runtime files...`)

  let examples = []
  await docFiles->ArrayUtils.forEachAsyncInBatches(~batchSize, async f => {
    let doc = await extractDocFromFile(Path.join(["runtime", f]))
    examples->Array.pushMany(doc->getExamples)
  })

  examples
}

let main = async () => {
  let examples = await extractExamples()

  examples->Array.sort((a, b) => String.compare(a.id, b.id))

  let dict = dict{}

  examples->Array.forEach(cur => {
    let modulePath = cur.id->String.split(".")

    let id =
      modulePath
      ->Array.slice(~start=0, ~end=Array.length(modulePath) - 1)
      ->Array.join(".")

    let previous = switch dict->Dict.get(id) {
    | Some(p) => p
    | None => []
    }

    dict->Dict.set(id, Array.concat([cur], previous))
  })

  let output = []

  dict->Dict.forEachWithKey((examples, key) => {
    let codeExamples = examples->Array.filterMap(example => {
      let ignoreExample = Array.find(
        ignoreRuntimeTests,
        ((version, tests)) => {
          nodeVersion === version && Array.includes(tests, example.id)
        },
      )->Option.isSome

      switch ignoreExample {
      | true =>
        Console.warn(
          `Ignoring ${example.id} tests. Not supported by Node ${nodeVersion->Int.toString}`,
        )
        None
      | false =>
        let code = getCodeBlocks(example)

        switch String.length(code) === 0 {
        | true => None
        | false =>
          // Let's add the examples inside a Test module because some examples
          // have type definitions that are not supported inside a block.
          // Also add unit type `()`
          Some(
            `test("${example.id->String.split(".")->Array.at(-1)->Option.getExn}", () => {
  module Test = {
    ${code}
  }
  ()
})`,
          )
        }
      }
    })

    switch Array.length(codeExamples) > 0 {
    | true =>
      let content = `describe("${key}", () => {
${codeExamples->Array.join("\n")}
 })`
      output->Array.push(content)
    | false => ()
    }
  })

  let dirname = url->URL.fileURLToPath->Path.dirname
  let filepath = Path.join([dirname, "generated_mocha_test.res"])
  let fileContent = `open Mocha
@@warning("-32-34-60-37-109-3-44")

${output->Array.join("\n")}`

  await Fs.writeFile(filepath, fileContent)
}

let () = await main()
