import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import mill.modules.Util
import $file.dependencies.chisel.build
import $file.common

object v {
  val scala = "2.13.11"
  val utest = ivy"com.lihaoyi::utest:0.8.1"
}

object mychisel extends Cross[Chisel](v.scala)

trait Chisel extends dependencies.chisel.build.Chisel {
  override def millSourcePath = os.pwd / "dependencies" / "chisel"
}

object `chisel-circt-binder` extends common.ChiselCIRCTPanamaBinderModule { m =>
  def circtInstallPath = T(os.Path(sys.env.get("CIRCT_INSTALL_PATH").getOrElse("/usr/local")))
  def includePaths = T(Seq(PathRef(circtInstallPath() / "include")))
  def libraryPaths = T(Seq(PathRef(circtInstallPath() / "lib")))

  def scalaVersion = T(v.scala)
  def chiselModule:    Option[ScalaModule] = Some(mychisel(v.scala))
  def chiselPluginJar: T[Option[PathRef]] = T(Some(mychisel(v.scala).pluginModule.jar()))
  def chiselIvy:       Option[mill.scalalib.Dep] = None
  def chiselPluginIvy: Option[mill.scalalib.Dep] = None

  object tests
      extends TestModule
      with TestModule.Utest
      with common.HasChiselCIRCTPanamaBinderModule
      with common.HasChisel {
    def chiselModule:    Option[ScalaModule] = Some(mychisel(v.scala))
    def chiselPluginJar: T[Option[PathRef]] = T(Some(mychisel(v.scala).pluginModule.jar()))
    def chiselIvy:       Option[mill.scalalib.Dep] = None
    def chiselPluginIvy: Option[mill.scalalib.Dep] = None
    def scalaVersion = v.scala
    def chiselCIRCTPanamaBinderModule = m
    def ivyDeps = Agg(v.utest)
    def defaultCommandName = "test"
  }
}
