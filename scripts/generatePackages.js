#!/usr/bin/env node

const { platforms } = require("./platforms");
const fs = require("fs");
const { duneBinDir } = require("./dune");
const ninjaDir = "ninja";

const dryrun = process.argv.includes("--dry-run");
const folder = "npm-releases";

const owner = "@rescript";
const repository = "https://github.com/rescript-lang/rescript-compiler";
const homepage = "https://rescript-lang.org";
const bugs = "https://github.com/rescript-lang/rescript-compiler/issues";
const license = "MIT";
const version = JSON.parse(fs.readFileSync("./package.json", "utf8")).version;

// const files = [
//   "CHANGELOG.md",
//   "CREDITS.md",
//   "ninja.COPYING",
//   "bsc",
//   "rescript",
//   "docs/docson/build-schema.json",
//   "lib",
//   "scripts",
// ];

const binaries = ["rescript", "bsc", "bsb_helper"];

function generatePackage(platform) {
  const description = `This is the ${platform.os} ${platform.arch} binary for ReScript.`;
  const name = `${owner}/rescript-${platform.os}-${platform.arch}`;
  const pkg = {
    name,
    version,
    preferUnplugged: true,
    description,
    homepage,
    bugs,
    license,
    repository,
    os: [platform.os],
    cpu: [platform.arch],
  };

  const readme = `# ReScript

${description}`;

  const dir_pkg = `${folder}/${platform.os}-${platform.arch}`;

  fs.mkdirSync(dir_pkg);
  fs.writeFileSync(`${dir_pkg}/package.json`, JSON.stringify(pkg, null, 2));
  fs.writeFileSync(`${dir_pkg}/README.md`, readme);

  binaries.forEach(bin =>
    fs.copyFileSync(`${duneBinDir}/${bin}`, `${dir_pkg}/${bin}`)
  );
  fs.copyFileSync(`${ninjaDir}/ninja`, `${dir_pkg}/ninja`);
  // fs.cpSync("lib", `${dir_pkg}/lib`, { recursive: true });

  console.log(`Package ${name} created`);
}

function main() {
  if (fs.existsSync(folder)) {
    fs.rmSync(folder, { recursive: true, force: true });
  }
  fs.mkdirSync(folder);
  for (const platform of platforms) {
    generatePackage(platform);
  }
}

main();
