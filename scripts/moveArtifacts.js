#!/usr/bin/env node

const fs = require("fs");
const { platforms } = require("./platforms");

for (const platform of platforms) {
  fs.chmodSync(`binaries-${platform.os}-${platform.arch}`, 0o755);
}

const cache_files = ["cmi_cache.bin", "cmj_cache.bin"];
for (const file of cache_files) {
  fs.renameSync(`cmij-cache/${file}`, `lib/${file}`);
}

fs.renameSync("lib-ocaml", "lib/ocaml");
fs.renameSync("ninja/COPYING", "ninja.COPYING");
