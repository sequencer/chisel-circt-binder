import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import mill.modules.Util
import $file.dependencies.chisel3.build
import $file.common

object v {
  val scala = "2.13.10"
  val utest = ivy"com.lihaoyi::utest:0.8.1"
}

object mychisel extends dependencies.chisel3.build.Chisel(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "chisel3"
}

object `chisel-circt-binder`
  extends ChiselCIRCTPanamaBinderModule {
  // TODO: find path from nix+env
  def includePath = os.Path("/usr/local/include")
  def libraryPath = os.Path("/usr/local/lib")
  // TODO: provide from jextract or hardcoded
  def jextractPath = os.Path("/home/asuna/dev/jextract-20/bin/jextract")

  def scalaVersion = T(v.scala)
  def chiselModule: Option[ScalaModule] = Some(mychisel)
  def chiselPluginJar: T[Option[PathRef]] = T(Some(mychisel.pluginModule.jar()))

  object tests extends Tests with TestModule.Utest with HasChiselCIRCTPanamaBinderModule {
    def chiselCIRCTPanamaBinderModule: ChiselCIRCTPanamaBinderModule = m 
    def ivyDeps = Agg(v.utest)
  }
}
