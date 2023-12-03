import mill._
import scalalib._
import scalanativelib._
import scalafmt._

object aoc extends ScalaNativeModule with ScalafmtModule {
  override def scalaVersion = "3.2.2"
  override def scalaNativeVersion = "0.4.16"

  override def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib::0.9.1",
  )

  object test extends ScalaTests with TestModule.Munit {
    override def ivyDeps = Agg(
      ivy"org.scalameta::munit:1.0.0-M10"
    )
  }
}
