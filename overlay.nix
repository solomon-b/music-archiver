final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {
      music-archiver = hfinal.callCabal2nix "music-archiver" (./.) { };
    });
  });
}
