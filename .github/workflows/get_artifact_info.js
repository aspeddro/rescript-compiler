const fs = require("fs");
const os = require("os");
// const platforms = require("../../scripts/platform");

// const artifactPath = require("../../scripts/bin_path").dirName;
const artifactPath = `${process.platform}-${process.arch}`
const artifactName = "binaries-" + artifactPath;

console.log("Artifact path:", artifactPath);

// Pass artifactPath and artifactName to subsequent GitHub actions
fs.appendFileSync(
  process.env.GITHUB_ENV,
  `artifact_path=${artifactPath}${os.EOL}artifact_name=${artifactName}${os.EOL}`
);
