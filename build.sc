import mill.define.Task
import mill.scalalib.DepSyntax
import mill.scalalib.scalafmt.ScalafmtModule
import mill.{ Agg, T }

import $ivy.`com.lihaoyi::mill-contrib-scoverage:$MILL_VERSION`
import mill.contrib.scoverage.ScoverageModule

/* * * * * * * * *
 * Configuration *
 * * * * * * * * */

private object Versions {
  // General
  val Scala = "2.13.2"
  val ScoverageVersion = "1.4.1"
  // Dependencies
  val ScalaTest = "3.1.1"
  val ScalaMock = "4.4.0"
}

private object Settings {

  private def prefix(p: String)(args: String*) = args.map(p + _)

  val ScalacOptions = Seq(
    Seq(
      "-deprecation",
      "-encoding", "utf-8",
      "-explaintypes",
      "-feature",
      "-unchecked"
    ),

    prefix("-W")(
      //"error",
      "extra-implicit",
      "dead-code",
      "numeric-widen",
      "octal-literal",
      "value-discard",
      "self-implicit"
    ),

    prefix("-Wunused:")(
      "imports",
      "patvars",
      "privates",
      "locals",
      "explicits",
      "implicits",
      "params",
      "linted"
    ),

    prefix("-X")(
      //"fatal-warnings",
      "checkinit"
    ),

    prefix("-Xlint:")(
      "adapted-args",
      "constant",
      "delayedinit-select",
      "doc-detached",
      "inaccessible",
      "infer-any",
      "missing-interpolator",
      "nullary-override",
      "nullary-unit",
      "option-implicit",
      "package-object-classes",
      "poly-implicit-overload",
      "private-shadow",
      "nonlocal-return",
      "implicit-not-found",
      "serial",
      "valpattern",
      "eta-zero",
      "eta-sam",
      "deprecation",
      "unused",
      "stars-align",
      "type-parameter-shadow"
    ),

    prefix("-Ywarn")(
      "-dead-code",
      "-extra-implicit",
      "-numeric-widen",
      "-value-discard"
    ),

    prefix("-Ywarn-unused:")(
      "implicits",
      "imports",
      "locals",
      "params",
      "patvars",
      "privates"
    )
  ).flatten
}

/* * * * * * * *
 * Common code *
 * * * * * * * */

private implicit class SeqTaskOps[A](seq: Seq[A]) {
  def |> [B](f: A => Task[B]) = Task.sequence(seq.map(f))
}

trait ScalaModule extends ScoverageModule with ScalafmtModule { outer =>

  override def scalaVersion = Versions.Scala
  override def scoverageVersion = Versions.ScoverageVersion
  override def scalacOptions = Settings.ScalacOptions
  override def millSourcePath = {
    val segments = super.millSourcePath.segments.toList
    os.root / (segments.init :+ "modules" :+ segments.last)
  }
  override def intellijModulePath = millSourcePath / 'main
  override def sources = T.sources(millSourcePath / 'main / 'scala)
  override def resources = T.sources(millSourcePath / 'main / 'resources)

  class ScalaTests(scope: Symbol, ctx: mill.define.Ctx) extends mill.Module()(ctx)
    with ScalafmtModule with ScoverageTests {

    override def millSourcePath = outer.millSourcePath
    override def intellijModulePath = millSourcePath / scope
    override def sources = T.sources(millSourcePath / scope / 'scala)
    override def resources = T.sources(millSourcePath / scope / 'resources)
    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:${Versions.ScalaTest}",
      ivy"org.scalamock::scalamock:${Versions.ScalaMock}"
    )
    override def testFrameworks = Seq(
      "org.scalatest.tools.Framework"
    )
  }

  val unit = new ScalaTests('unit, implicitly)
  val integration = new ScalaTests('integration, implicitly)

  def test() = T.command {
    Seq(unit, integration) |> { _.test() }
  }

  def coverage() = T.command {
    T.sequence(
      Seq(test().map(_ => ()), scoverage.htmlReport(), scoverage.xmlReport())
    )
  }
}

/* * * * * *
 * Modules *
 * * * * * */

object util extends ScalaModule

object preprocessing extends ScalaModule {
  override def moduleDeps = Seq(util)
}

object `neural-network` extends ScalaModule {
  override def moduleDeps = Seq(util)
}

object `gradient-descent` extends ScalaModule {
  override def moduleDeps = Seq(`neural-network`)
}

object training extends ScalaModule {
  override def moduleDeps = Seq(preprocessing, `gradient-descent`)
}

private val allModules = Seq[ScalaModule](
  util,
  preprocessing,
  `neural-network`,
  `gradient-descent`,
  training
)

/* * * * *
 * Tasks *
 * * * * */

def init(ev: mill.eval.Evaluator) = mill.scalalib.GenIdea.idea(ev)

def compile() = T.command {
  allModules |> { _.compile }
}

def reformat() = T.command {
  allModules |> { _.reformat() }
}

def unit() = T.command {
  allModules |> { _.unit.test() }
}

def integration() = T.command {
  allModules |> { _.integration.test() }
}

def test() = T.command {
  allModules |> { _.test() }
}

def coverage() = T.command {
  allModules |> { _.coverage() }
}
