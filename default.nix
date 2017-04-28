{ mkDerivation, aeson, base, bytestring, directory, filepath, lens
, process, stdenv, yaml
}:
mkDerivation {
  pname = "smallDockerImagesTool";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring directory filepath lens process yaml
  ];
  homepage = "https://github.com/dyrnade/smallDockerImagesTool";
  license = stdenv.lib.licenses.gpl3;
}
