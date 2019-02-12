package controllers
import play.api._

import play.api.mvc._

import play.api.mvc.Results._
import slick.driver.MySQLDriver.simple._
import play.api.i18n._

import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import slick.jdbc.{GetResult, StaticQuery => Q}
import slick.jdbc.JdbcBackend.Database
import Q.interpolation

class Stats {
  /*
   * Function to log all entries from external websites to one of democras objects
   *//*
  def addActionToStats(objectType: Int, objectID: Long, urlFrom: String){
    if (urlFrom.indexOf("democras.com") == 0){ //If it is not a visit from democras
      Instantiations.Tables.db.withSession { implicit session =>
        //We add it to the database
        val urlFromQuery = Instantiations.Tables.redirectStats.filter(x => x.objectType === objectType && x.objectID === objectID && x.urlFrom === urlFrom)
        val results = urlFromQuery.length.run
        if (results == 0){
          Instantiations.Tables.redirectStats += (objectType, objectID, urlFrom, 0l, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())))
        }
        else{
          sqlu"update REDIRECTSTATS set NUMBER=NUMBER+1 where TYPE = $objectType AND ID = $objectID AND URLFROM = $urlFrom".first
        }
      }
    }
  }*/
}