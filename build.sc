import mill._, scalalib._

object api extends ScalaModule {
  def scalaVersion = "2.13.2"
}

object impl extends ScalaModule {
  def scalaVersion = "2.13.2"
  def moduleDeps = Seq(api)

  object test extends Tests {
    def ivyDeps = Agg(
       ivy"com.lihaoyi::utest:0.7.4",
       ivy"org.scalacheck::scalacheck:1.15.4"
     )
    def testFrameworks = Seq("utest.runner.Framework")
  }
}