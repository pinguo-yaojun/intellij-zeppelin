package intellij.zeppelin

import java.net.Proxy

import spray.json.{JsString, _}
import scalaj.http.Http

import scala.util.{Failure, Success}

object SprayJsonTest extends App {

  val req = Http("http://ip-10-0-146-250.ec2.internal:8890/api/notebook/2DQ93RDRK").proxy("127.0.0.1", 8157, Proxy.Type.SOCKS)

  val response = req.asString.body

  println(response.parseJson.asJsObject().fields("body"))

  val result = response.parseJson.asJsObject().fields("body") match {
    case JsObject(s) => s.asInstanceOf[Notebook]
    case _ => "2322"
  }

  println(result)
}
