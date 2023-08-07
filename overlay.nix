final: prev:
{
  mill = prev.mill.override { jre = final.openjdk20; };
  espresso = final.callPackage ./nix/espresso.nix { };
  jextract = prev.callPackage ./nix/jextract.nix { };
  circt = prev.circt.overrideAttrs (old: {
    version = "for-binder";
    cmakeFlags = [
      "-DBUILD_SHARED_LIBS=ON"
      "-DLLVM_ENABLE_BINDINGS=OFF"
      "-DLLVM_ENABLE_OCAMLDOC=OFF"
      "-DLLVM_BUILD_EXAMPLES=OFF"
      "-DLLVM_OPTIMIZED_TABLEGEN=ON"
      "-DLLVM_ENABLE_PROJECTS=mlir"
      "-DLLVM_EXTERNAL_PROJECTS=circt"
      "-DLLVM_EXTERNAL_CIRCT_SOURCE_DIR=.."
      "-DCIRCT_LLHD_SIM_ENABLED=OFF"
    ];
    src = final.fetchFromGitHub {
      owner = "llvm";
      repo = "circt";
      rev = "c44803e680639fa46a3badf33a0f773911ec8f8a";
      sha256 = "sha256-Jh+NQXmbApGPqdpgsSFY0Ytu8K1FXk3vV/XOCfQtzAU=";
      fetchSubmodules = true;
    };
    patches = [
      (prev.fetchpatch {
        url = "https://github.com/llvm/circt/commit/cf94c95d4af8fd77ca44e889a240ed4b6f5efe61.patch";
        hash = "sha256-G8l8Fo+PG8jLEs8CmjTRHC/+3Xukoxbw1o5NvDKEU4M=";
      })
    ];
    installPhase = ''
      runHook preInstall
      mkdir -p $out
      CMAKE_INSTALL_PREFIX=$out cmake --build . --target install --config Release
      runHook postInstall
    '';
  });
}
