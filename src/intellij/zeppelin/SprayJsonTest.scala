package intellij.zeppelin

import java.net.Proxy

import spray.json.{JsString, _}
import spray.json.DefaultJsonProtocol._
import scalaj.http.Http

import scala.util.{Failure, Success}

object SprayJsonTest extends App {

  val str = "//paragraph 4\n    println(\"this is a test\")"

  println(str.replaceAll("\n\\s+", "\n"))
}
