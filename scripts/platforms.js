const name = `${process.platform}-${process.arch}`;
const platforms = [
  {
    os: "darwin",
    arch: "arm64",
    exe: "rescript",
  },
  {
    os: "darwin",
    arch: "x64",
    exe: "rescript",
  },
  {
    os: "linux",
    arch: "arm64",
    exe: "rescript",
  },
  {
    os: "linux",
    arch: "x64",
    exe: "rescript",
  },
  {
    os: "win32",
    arch: "x64",
    exe: "rescript",
  },
];

exports.platforms = platforms;
exports.name = name;
