let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  base = nixpkgs.latest.rustChannels.stable;
  rust = base.rust.override {
    extensions = [
      "rls-preview"
      "rust-analysis"
      "rust-src"
      "rust-std"
      "rustfmt-preview"
    ];
  };
in
  with nixpkgs;
  stdenv.mkDerivation {
    name = "moz_overlay_shell";
    buildInputs = [
      rust
      gnuplot
    ];
    RUST_SRC_PATH="${rust}/lib/rustlib/src/rust/src";
  }
