((import ./.).mqtt-clean-retain.components.exes.mqtt-clean-retain // {
  env = (import ./.).shellFor {
    packages = p: [ p.mqtt-clean-retain ];
    exactDeps = true;
    tools = {
      cabal = "3.2.0.0";
      hie = "unstable";
    };
    shellHook = ''
      export HIE_HOOGLE_DATABASE=$(realpath "$(dirname "$(realpath "$(which hoogle)")")/../share/doc/hoogle/default.hoo")
    '';
  };
}).env
