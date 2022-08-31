@@jsxConfig({version: 4})

module CompV4 = {
  @genType @react.component
  let make = (~x, ~y) => React.string(x ++ y)
}

@@jsxConfig({version: 3})

module CompV3 = {
  @genType @react.component
  let make = (~x, ~y) => React.string(x ++ y)
}

@genType
type person = {
  name: string,
  age: int,
}

@@jsxConfig({version: 4})

@genType.import("./hookExample") external foo4: (~person: person) => string = "foo"