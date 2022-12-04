import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import mill.modules.Util
import $file.dependencies.chisel3.build
import $file.dependencies.firrtl.build
import $file.dependencies.treadle.build
import $file.dependencies.chiseltest.build
import $file.common

object v {
  val scala = "2.13.10"
}

object myfirrtl extends dependencies.firrtl.build.firrtlCrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "firrtl"
  override val checkSystemAntlr4Version = false
  override val checkSystemProtocVersion = false
  override val protocVersion = os.proc("protoc", "--version").call().out.text.dropRight(1).split(' ').last
  override val antlr4Version = os.proc("antlr4").call().out.text.split('\n').head.split(' ').last
}
object mytreadle extends dependencies.treadle.build.treadleCrossModule(v.scala) {
  override def millSourcePath = os.pwd /  "dependencies" / "treadle"
  def firrtlModule: Option[PublishModule] = Some(myfirrtl)
}
object mychisel3 extends dependencies.chisel3.build.chisel3CrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "chisel3"
  def firrtlModule: Option[PublishModule] = Some(myfirrtl)
  def treadleModule: Option[PublishModule] = Some(mytreadle)
  def chiseltestModule: Option[PublishModule] = Some(mychiseltest)
}
object mychiseltest extends dependencies.chiseltest.build.chiseltestCrossModule(v.scala) {
  override def millSourcePath = os.pwd /  "dependencies" / "chiseltest"
  def chisel3Module: Option[PublishModule] = Some(mychisel3)
  def treadleModule: Option[PublishModule] = Some(mytreadle)
}
object `chisel-circt-binder` extends common.ChiselCIRCTBinderModule with ScalafmtModule { m =>
  def scalaVersion = T { v.scala }
  def chisel3Module = Some(mychisel3)
  def chisel3PluginJar = T { Some(mychisel3.plugin.jar()) }
  def jextractTarGz = T.persistent {
    Util.download(s"https://download.java.net/java/early_access/jextract/2/openjdk-19-jextract+2-3_linux-x64_bin.tar.gz", os.rel / "jextract.tar.gz")
    PathRef(T.dest / "jextract.tar.gz")
  }

  def jextract = T.persistent {
    os.proc("tar", "xvf", jextractTarGz().path).call(T.dest)
    PathRef(T.dest / "jextract-19" / "bin" / "jextract")
  }

  object tests extends Tests with TestModule.Utest {
    def scalacOptions = { m.scalacOptions() }
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.8.1")
  }
}
