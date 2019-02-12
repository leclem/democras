package controllers

import play.api._

import play.api.mvc._
import play.api.Play.current
import play.api.mvc.Results._
import slick.driver.MySQLDriver.simple._
import play.api.i18n._
import java.nio.charset.Charset
import java.nio.file.Files
import java.text.SimpleDateFormat
import java.util.Locale
import java.net.SocketTimeoutException
import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import slick.jdbc.{GetResult, StaticQuery => Q}
import slick.jdbc.JdbcBackend.Database
import Q.interpolation
import scalaj.http.Http
import com.mysql.jdbc.exceptions.MySQLIntegrityConstraintViolationException

object Scraping  extends Controller {
  
  def scrapFacebookActions() = Action {
    while (true){
      val charset = Charset.forName("utf-8");
      val reader = Files.newBufferedReader(Play.getFile("conf/fbLinks").toPath(), charset)
      var line = convertIntoGraphAdress(reader.readLine());
      while (line != null) {
          parseFBAddress(line)
          Thread.sleep(100)
          line = convertIntoGraphAdress(reader.readLine());
      }
      Thread.sleep(3600000)
    }
    Ok
  }
  
  def convertIntoGraphAdress(line: String): String = {
    if (line == null)
      null
    else
      return "https://graph.facebook.com/v2.8/" + line + "/events?access_token=514964885289852|IPTe1W191kHRnQbs3DLGiy1YOxY&limit=2"
  }
  
  def parseFBAddress(address: String): Unit = {
    var lastDate = -1l
    try {
      val result = Http(address).asString;
      val response = Json.parse(result.body);
      val data = (response \ "data").as[JsArray].value;
      lastDate = parseFBData(data);
    
      if (lastDate > System.currentTimeMillis()){
        val next = (response \ "paging" \ "next") match {
           case JsDefined(v) => parseFBAddress(v.as[String])
           case undefined: JsUndefined => 
        }
      }
    } catch { 
      case jse: JsResultException => println("exception during json facebook parsing")
      case sktTE: SocketTimeoutException => println("exception during facebook HTTP request: timesout")
      case mysqlIntegrityException: MySQLIntegrityConstraintViolationException => println("Integreity violation")
    }
  }
  
  def parseFBData(value: Seq[JsValue]): Long = {
    var lastStartTime = Long.MaxValue
    for (currentEvent <- value ) {
          val name = (currentEvent \ "name");
          val description = (currentEvent \ "description") match {
            case JsDefined(v) => v.as[String]
            case undefined: JsUndefined => ""
          }
          val latitude = (currentEvent \ "place" \ "location" \ "latitude");
          val latitudeLong = latitude match {
            case JsDefined(v) => Option(v.as[Double])
            case undefined: JsUndefined => None
          }
          
          val longitude = (currentEvent \ "place" \ "location" \ "longitude");
          val longitudeLong = longitude match {
            case JsDefined(v) => Option(v.as[Double])
            case undefined: JsUndefined => None
          }
          
          val address1 = (currentEvent \ "place" \ "name")
          val address1String = address1 match {
            case JsDefined(v) => v.as[String]
            case undefined: JsUndefined => ""
          }
          
          val address2 = (currentEvent \ "place" \ "street")
          val address2String = address2 match {
            case JsDefined(v) => v.as[String]
            case undefined: JsUndefined => ""
          }
          
          val startTime = (currentEvent \ "start_time") match {
            case JsDefined(v) => Option(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ", Locale.ENGLISH).parse(v.as[String]).getTime()) 
            case undefined: JsUndefined => None
          }
          
          val endTime = (currentEvent \ "end_time") match {
            case JsDefined(v) => new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ", Locale.ENGLISH).parse(v.as[String]).getTime()
            case undefined: JsUndefined => -1
          }
          if (startTime != None)
              lastStartTime = startTime.get
          if (longitudeLong != None && latitudeLong != None && startTime != None && startTime.get > System.currentTimeMillis()){
            Thread.sleep(30000)
            Actions.createAction(-1, 7782, name.as[String], description, address1String, address2String, latitudeLong.get, longitudeLong.get, startTime.get, endTime, "fr", false) 
    
          }
        }
    return lastStartTime
  }
}