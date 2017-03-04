{ mkDerivation, base, process, stdenv }:
mkDerivation {
  pname = "smallDockerImagesTool";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base process ];
  homepage = "https://github.com/dyrnade/smallDockerImagesTool";
  license = stdenv.lib.licenses.gpl3;
}
