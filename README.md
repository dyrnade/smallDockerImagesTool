# smallDockerImagesTool
Smaller Docker Images Tool with the power of Nix Package Manager.

### This project is intended to have simpler usage of Nix language for new comers like me. ###

- This tool expects a configuration-file(docker.yml) from user to read and builds image as follow.

**IMPORTANT**: This tools is in very early stage. I have some ideas to make things simple as possible.

Menu looks like below

```
Usage: sdit [OPTION]
Create DOCKER IMAGE(s) from configuration-file.

    help              Displays this help menu.
    build             Builds the docker image.
    init              Creates initial configuration-file(docker.yml)
    print             Displays created Nix from configuration-file(docker.yml)

Homepage and help: https://github.com/dyrnade/smallDockerImagesTool

```

**Important**: Indentation is important so Please! follow the rules.

Note: I am new at Haskell and Nix, so please let me know about any improvements.
      The code looks awful somehow to follow error free indentation as possible as it can and some Nix language syntax.
