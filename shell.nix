{ system ? null }:
let this = import ./. { inherit system; };
in this.pinBuildInputs "shell" (this.tryReflexPackages ++ [this.nixpkgs.inotify-tools]) []
