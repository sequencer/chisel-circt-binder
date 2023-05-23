import mill._
import mill.scalalib._
import mill.scalalib.publish._

import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.1.4`
import de.tobiasroeser.mill.vcs.version.VcsVersion

trait ChiselCIRCTBinderPublishModule extends PublishModule {m =>
  def publishVersion = de.tobiasroeser.mill.vcs.version.VcsVersion.vcsState().format()
  def pomSettings = T {
    PomSettings(
      description = artifactName(),
      organization = "me.jiuyang",
      url = "https://github.com/sequencer/chisel-circt-binder",
      licenses = Seq(License.`Apache-2.0`),
      versionControl = VersionControl.github("seqeuncer", "chisel-circt-binder"),
      developers = Seq(
        Developer("sequencer", "Jiuyang Liu", "https://github.com/sequencer")
      )
    )
  }
}

trait ChiselCIRCTBinderModule extends ChiselCIRCTBinderPublishModule with ScalaModule {
  def circtJextractModule: PublishModule

  def chisel3Module: Option[PublishModule] = None

  def chisel3PluginJar: T[Option[PathRef]] = T {
    None
  }

  // Use SNAPSHOT chisel by default, downstream users should override this for their own project versions.
  def chisel3IvyDep: T[Option[Dep]] = None

  def chisel3PluginIvyDep: T[Option[Dep]] = None

  // User should not override lines below
  override def moduleDeps = Seq(circtJextractModule) ++ chisel3Module

  override def scalacPluginClasspath = T {
    super.scalacPluginClasspath() ++ chisel3PluginJar()
  }

  override def scalacPluginIvyDeps = T {
    Agg() ++ chisel3PluginIvyDep()
  }

  override def scalacOptions = T {
    super.scalacOptions() ++ chisel3PluginJar().map(path => s"-Xplugin:${path.path}") ++ Some("-Ymacro-annotations")
  }

  override def javacOptions = T {
    super.javacOptions() ++ Seq("--enable-preview", "--release", "20")
  }

  override def ivyDeps = T {
    Agg() ++ chisel3IvyDep()
  }
}
