package com.example

import unfiltered.request._
import unfiltered.response._

import org.clapper.avsl.Logger

import com.codahale.jerkson.Json
import com.fasterxml.jackson.core.{Version, JsonGenerator}
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.{JsonDeserializer, SerializerProvider, JsonSerializer}
//import org.joda.time.DateTime
//import org.joda.time.format.ISODateTimeFormat
//import java.text.DateFormat
//import java.util.Locale

case class RSS(name: String, url: String)

/** unfiltered plan */
class App extends unfiltered.filter.Plan {
  import QParams._

  val logger = Logger(classOf[App])
  def jsonResponse[T](t:T)= JsonContent ~> ResponseString(Json.generate(t))
  def intent = {
    case GET(Path(Seg("rss"::Nil)))=> Ok ~> jsonResponse(Seq("test","de","null"))
    case GET(Path(Seg("rss"::"new"::Nil)))=> Ok ~> HtmlContent ~> Html(
        <html><body><form action="/rss" method="POST">
          <input type="text" name="name"/>
          <input type="text" name="url"/>
          <input type="submit"/>
        </form></body></html>
      )
    case POST(Path("/rss") & Params(params))=> {
      import unfiltered.request.QParams._
      val expected = for (
        name <- lookup("name") is required("missing");
        url <- lookup("url") is required("missing")
      ) yield {
        Ok ~> JsContent ~> jsonResponse(new RSS(name.get,url.get))
      }
      expected(params) orFail (failures => {
        BadRequest ~> JsContent ~> jsonResponse(failures)
      })
    }
    case GET(Path(p)) =>
      logger.debug("GET %s" format p)
      Ok ~> view(Map.empty)(<p> What say you? </p>)
    case POST(Path(p) & Params(params)) =>
      logger.debug("POST %s" format p)
      val vw = view(params)_
      val expected = for {
        int <- lookup("int") is
          int { _ + " is not an integer" } is
          required("missing int")
        word <- lookup("palindrome") is
          trimmed is
          nonempty("Palindrome is empty") is
          pred(palindrome, { _ + " is not a palindrome" }) is
          required("missing palindrome")
      } yield vw(<p>Yup. { int.get } is an integer and { word.get } is a palindrome. </p>)
      expected(params) orFail { fails =>
        vw(<ul> { fails.map { f => <li>{f.error} </li> } } </ul>)
      }
  }
  def palindrome(s: String) = s.toLowerCase.reverse == s.toLowerCase
  def view(params: Map[String, Seq[String]])(body: scala.xml.NodeSeq) = {
    def p(k: String) = params.get(k).flatMap { _.headOption } getOrElse("")
    Html(
     <html>
      <head>
        <title>uf example</title>
        <link rel="stylesheet" type="text/css" href="/assets/css/app.css"/>
      </head>
      <body>
       <div id="container">
       { body }
       <form method="POST">
         <div>Integer <input type="text" name="int" value={ p("int") } /></div>
         <div>Palindrome <input type="text" name="palindrome" value={ p("palindrome") } /></div>
         <input type="submit" />
       </form>
       </div>
     </body>
    </html>
   )
  }
}

/** embedded server */
object Server {
  val logger = Logger(Server.getClass)
  def main(args: Array[String]) {
    val http = unfiltered.jetty.Http(8080) // this will not be necessary in 0.4.0
    http.context("/assets") { _.resources(new java.net.URL(getClass().getResource("/www/css"), ".")) }
      .filter(new App).run({ svr =>
        unfiltered.util.Browser.open(http.url+"rss")
      }, { svr =>
        logger.info("shutting down server")
      })
  }
}
