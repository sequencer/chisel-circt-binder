final: prev:
{
  mill = prev.mill.override { jre = final.openjdk20; };
  espresso = final.callPackage ./nix/espresso.nix { };
  jextract = final.callPackage ./nix/jextract.nix { };
  circt = prev.circt;
}
