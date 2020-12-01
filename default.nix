{ compiler ? "ghc8102", pkgs ? import <nixpkgs> {} }:

let
	haskellPackages = pkgs.haskell.packages.${compiler};
	drv = haskellPackages.callCabal2nix "adventOfCode2020" ./. {};
in {
	adventOfCode2020 = drv;
	adventOfCode2020-shell = haskellPackages.shellFor {
		packages = p: [drv];
		buildInputs = with pkgs; [
		];
	};
}
