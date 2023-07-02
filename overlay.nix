final: prev:
{
  mill = prev.mill.override { jre = final.openjdk20; };
  circt = prev.circt.overrideAttrs (old: {
    version = "unstable-2023-06-30";
    src = final.fetchFromGitHub {
      owner = "llvm";
      repo = "circt";
      rev = "c44803e680639fa46a3badf33a0f773911ec8f8a";
      sha256 = "sha256-HsXwh98RZuXvK/KkZ2NAGwWNLxUAQVj+WKzZXd4C4Is=";
      fetchSubmodules = true;
    };
  });
  jextract = prev.callPackage ./jextract { };
}
