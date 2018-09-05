package intellij.zeppelin

import java.net.{HttpCookie, Proxy}

import spray.json.{JsString, _}
import spray.json.DefaultJsonProtocol._

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scalaj.http.Http

case class Notebook(id: String, name: String, size: Int) {
  def notebookHeader(url: String): String = Seq(markerText, s"//$url/#/notebook/$id").mkString("\n")

  def markerText: String = s"//Notebook:$name,$id,$size"
}

case class Paragraph(id: String, text:String, index: Int) {
  def paragraphHeader: String = s"//paragraph:$id,$index"
  def markerText: String = s"$text\n${Paragraph.paragraphFooter}".split("\n").map(x => s"$x\n").mkString("")
}

case class ParagraphResult(paragraph: Paragraph, results: Seq[String]) {
  def markerText: String = {
    "/*\nOutput:\n" + results.flatMap(_.split("\n").map(x => s"$x\n")).mkString("") + "*/"
  }
}

object Notebook {
  private val NoteId: Regex = """.*//Notebook:([\w/-]+),(\w+),(\d+).*""".r
  private val NoteName: Regex = """.*//Notebook:([\w/-]+).*""".r

  def parse(text: String): Option[Notebook] = text match {
    case NoteId(name, id, size) => Some(Notebook(id, name, size.toInt))
    case NoteName(name) => Some(Notebook("", name, 0))
    case _ => None
  }
}

object Paragraph {
  val ParagraphFlag: Regex = """.*//paragraph (\d+).*""".r
  val paragraphFooter: String = "//paragraph end"

  def parse(text: String): Option[Paragraph] = text match {
    case ParagraphFlag(index) => Some(Paragraph("", "", index.toInt))
    case _ => None
  }

}

case class Credentials(username: String, password: String)

class ZeppelinApi(val url: String, credentials: Option[Credentials]) {

  lazy val sessionToken: Option[HttpCookie] = credentials.flatMap { c =>
    val r = Http(s"$url/api/login").proxy("127.0.0.1", 8157, Proxy.Type.SOCKS).postForm(Seq(
      ("username", c.username),
      ("password", c.password)
    ))
    r.asString.cookies.headOption
  }

  private def request(path: String) = {
    val r = Http(s"$url$path").proxy("127.0.0.1", 8157, Proxy.Type.SOCKS)
    sessionToken.fold(r)(cookie => r.header("Cookie", s"${cookie.getName}=${cookie.getValue}"))
  }


  def createNotebook(name: String): Try[Notebook] = {
    val req = request("/api/notebook").postData(
      s"""
         |{"name": "$name"}
      """.stripMargin)

    val response = req.asString.body
    response.parseJson.asJsObject().fields("body") match {
      case JsString(s) => Success(Notebook(s, name, 0))
      case _ => Failure(new RuntimeException("Error creating new Zeppelin notebook"))
    }
  }

  def getNotebook(name: String): Try[Notebook] = {
    val req = request("/api/notebook")

    val response = req.asString.body

    response.parseJson.asJsObject().fields("body") match {
      case JsArray(arr) => Success(arr.map(n => Notebook(n.asJsObject().fields("id").asInstanceOf[JsString].value, n.asJsObject().fields("name").asInstanceOf[JsString].value, 0)).filter(_.name == name).head)
      case _ => Failure(new RuntimeException(s"Error get the Zeppelin notebook[$name]"))
    }
  }

  def createParagraph(notebook: Notebook, text: String, atIndex: Option[Int] = None): Try[Paragraph] = {
    val escaped = text.replaceAll("\\\"", "\\\\\"")

    val body = atIndex match {
      case Some(index) => s"""{"title":"new note", "text": "$escaped", "index": $index}"""
      case None => s"""{"title":"new note", "text": "$escaped"}"""
    }
    val req = request(s"/api/notebook/${notebook.id}/paragraph").postData(body)

    req.asString.body.parseJson.asJsObject.fields("body") match {
      case JsString(paragraphId) => Success(Paragraph(paragraphId, text, notebook.size))
      case _ => Failure(new RuntimeException("Error creating new Zeppelin paragraph"))
    }
  }

  def deleteParagraph(notebook: Notebook, paragraph: Paragraph): Try[Paragraph] = {
    val result = request(s"/api/notebook/${notebook.id}/paragraph/${paragraph.id}").method("DELETE")
      .asString
      .body.parseJson.asJsObject

    result.fields("status") match {
      case JsString(status) if status == "OK" => Success(paragraph)
      case unknown => Failure(new RuntimeException(s"Unrecognized response $unknown"))
    }
  }

  def updateParagraph(notebook: Notebook, paragraph: Paragraph, text: String): Try[Paragraph] = {
    val escaped = text.replaceAll("\\\"", "\\\\\"")
    val result = request(s"/api/notebook/${notebook.id}/paragraph/${paragraph.id}").put(s"""{"text": "$escaped"}""")
      .asString
      .body.parseJson.asJsObject

    result.fields("status") match {
      case JsString(status) if status == "OK" => Success(paragraph)
      case unknown => Failure(new RuntimeException(s"Unrecognized response $unknown"))
    }
  }

  def listParagraphs(notebook: Notebook): Try[List[Paragraph]] = {
    val req = request(s"/api/notebook/${notebook.id}")

    val response = req.asString.body

    case class ParagraphResponse(id: String, text:Option[String], index: Option[Int])

    implicit val paragraphFormat = jsonFormat(ParagraphResponse, "id", "text", "index")

    response.parseJson.asJsObject().fields("body").asJsObject().fields("paragraphs") match {
      case JsArray(arr) => Success(arr.zipWithIndex.map{ p =>
        val paragraphResp = p._1.convertTo[ParagraphResponse]

        Paragraph(paragraphResp.id, paragraphResp.text.getOrElse(""), p._2)
      }.toList)
      case _ => Failure(new RuntimeException(s"Error list paragraphs from Zeppelin notebook[${notebook.id}]"))
    }
  }

  def getParagraph(notebook: Notebook, paragraph: Paragraph): Try[Paragraph] = {
    val req = request(s"/api/notebook/${notebook.id}/paragraph/${paragraph.id}")

    val response = req.asString.body.parseJson.asJsObject()

    case class ParagraphResponse(id: String, text:Option[String], index: Option[Int])

    implicit val paragraphFormat = jsonFormat(ParagraphResponse, "id", "text", "index")

    response.fields("status") match {
      case JsString(status) if status == "OK" =>
        val paragraphResp = response.fields("body").convertTo[ParagraphResponse]
        Success(Paragraph(paragraphResp.id, paragraphResp.text.getOrElse(""), paragraph.index))
      case unknown => Failure(new RuntimeException(s"Unrecognized response $unknown"))
    }
  }

  def getParagraphByFlag(notebook: Notebook, paragraphFlag: String): Try[Paragraph] = {
    val paragraphList = listParagraphs(notebook)
    Success(paragraphList.get.filter(p => p.text.contains(paragraphFlag)).head)
  }

  def runParagraph(notebook: Notebook, paragraph: Paragraph): Try[ParagraphResult] = {
    val result = request(s"/api/notebook/run/${notebook.id}/${paragraph.id}").timeout(10000, 10*60*1000).postData("")
      .asString
      .body.parseJson.asJsObject

    result.fields("status") match {
      case JsString(status) if status == "OK" =>
        result.fields("body").asJsObject.fields("msg") match {
          case JsArray(arr) => Success(arr.map(_.asJsObject.fields("data"))
            .collect { case JsString(s) => s })
            .map(ParagraphResult(paragraph, _))
          case other => Failure(new RuntimeException(s"Unrecognized result $other"))
        }
      case unknown => Failure(new RuntimeException(s"Unrecognized response $unknown"))
    }
  }
}

