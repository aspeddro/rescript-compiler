#!/usr/bin/env node

const fs = require('fs')
const { platforms } = require('./platforms')

for (const platform of platforms) {
  fs.chmodSync(`binaries-${platform.os}-${platform.arch}`, 0o755);
}

const other_files = ["cmi_cache.bin", "cmj_cache.bin"];
for (const other_file of other_files) {
  fs.renameSync(`cmij-cache/${other_file}`, 'lib')
}

fs.renameSync('lib-ocaml', 'lib')
fs.renameSync('ninja/COPYING', 'ninja.COPYING')
