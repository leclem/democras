package controllers
import play.api._
import play.api.Play.current
import play.api.cache.Cache;
import play.api.mvc._
import play.api.i18n._
import play.api.i18n.I18nSupport
import slick.driver.MySQLDriver.simple._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data._
import play.api.data.Forms._
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.s3.model.PutObjectResult;
import java.io.File
import java.io.FileInputStream;
import javax.activation.MimetypesFileTypeMap;
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import slick.jdbc.{GetResult, StaticQuery => Q}
import slick.jdbc.JdbcBackend.Database
import Q.interpolation
import scala.util.Random

import org.jsoup._
import org.jsoup.safety.Whitelist
/*
 * Returns the username of the user that is associated with this userID. If no user have this userID, returns null
 */
object User extends Controller {
  
  
   val AWS_ACCESS_KEY = "AKIAJRAOJ2B24LTBLHIA"
   val AWS_SECRET_KEY = "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"
   val AWS_S3_BUCKET = "idmcr"
  
  /*
   * The JSON model of an user.
   * UpdatedTimestamp is the timestamp when the member was added to the result set
   */
  implicit val userWrites: Writes[models.MemberInfo] = {
    (
    (JsPath \ "id").write[Long] and
    (JsPath \ "username").write[String] and
    (JsPath \ "firstname").write[String] and
    (JsPath \ "name").write[String] and
    (JsPath \ "mail").write[String] and
    (JsPath \ "ville").write[String] and
    (JsPath \ "presentation").write[String] and
    (JsPath \ "rank").write[Int] and
    (JsPath \ "isUserFollowing").write[Int] and
    (JsPath \ "updatedTimestamp").write[Long])(unlift(models.MemberInfo.unapply))
  }
  //(picture_url: String, picture_offx: Int, picture_offy: Int, picturemin_offx, picturemin_offy)

  implicit val userProfilePictureWrites: Writes[models.MemberProfilePicture] = {
    (
    (JsPath \ "id").write[Long] and
    (JsPath \ "picture_url").write[String] and
    (JsPath \ "picture_offx").write[Int] and
    (JsPath \ "picture_offy").write[Int] and
    (JsPath \ "picturemin_offx").write[Int] and
    (JsPath \ "picturemin_offy").write[Int])(unlift(models.MemberProfilePicture.unapply))
  }
  
  
  case class memberSearchEntry(username: String, member: models.MemberInfo, picture: models.MemberProfilePicture)
  
  
  implicit val memberSearchEntryWrites: Writes[memberSearchEntry] = {
    (
    (JsPath \ "username").write[String] and
    (JsPath \ "memberInfo").write[models.MemberInfo] and
    (JsPath \ "profilePicture").write[models.MemberProfilePicture]
    )(unlift(memberSearchEntry.unapply))
  }
 
  implicit val memberLanguageEntryWrites: Writes[models.MemberLanguages] = {
    (
        (JsPath \ "id").write[Long] and
        (JsPath \ "languages").write[Seq[String]]
    )(unlift(models.MemberLanguages.unapply))
  }
  
  implicit val memberStatsEntryWrites: Writes[models.MemberStats] = {
    (
        (JsPath \ "id").write[Long] and
        (JsPath \ "numberFollowers").write[Long] and
        (JsPath \ "numberLikes").write[Long] and
        (JsPath \ "ranking").write[Long] and
        (JsPath \ "totalNumberMembers").write[Long]
    )(unlift(models.MemberStats.unapply))
  }
  
  implicit val memberFriendsMailsEntryWrites: Writes[models.MembersFriendsMails] = {
    (
        (JsPath \ "friendMail").write[String] and
        (JsPath \ "modificationTimestamp").write[Long] and
        (JsPath \ "optionalUserId").write[Long] and
        (JsPath \ "optionalUsername").write[String]
    )(unlift(models.MembersFriendsMails.unapply))
  }

  def convertMemberInfoDatabaseEntryToClass(databaseEntry: models.Types.memberInfoEntry) = {
    models.MemberInfo(databaseEntry._1, gettUsercameFromID(databaseEntry._1),  databaseEntry._2, databaseEntry._3, databaseEntry._4, databaseEntry._5, Application.outputHTMLText(databaseEntry._6, true), -1, 0, 0)
  }

  def convertFollowDatabaseEntryToClass(databaseEntry: models.Types.followEntry) = {
    models.Follow(databaseEntry._1, databaseEntry._2,  databaseEntry._3, databaseEntry._4, databaseEntry._5.getTime())
  }
 
  def convertProfilePictureDatabaseEntryToClass(databaseEntry: models.Types.memberProfilePictureEntry) = {
    models.MemberProfilePicture(databaseEntry._1, databaseEntry._2, databaseEntry._3, databaseEntry._4, databaseEntry._6, databaseEntry._7)
  }
  
  def convertMemberStatsDatabaseEntryToClass(databaseEntry: models.Types.memberStatsEntry) = {
    models.MemberStats(databaseEntry._1, databaseEntry._2, databaseEntry._3, -1, -1)
  }
  
  def convertMembersMailsEntryToClass(databaseEntry: models.Types.memberFriendsMails) = {
    models.MembersFriendsMails(databaseEntry._2, databaseEntry._4.getTime(), -1, "")
  }
  
  /*
   * Searchs for a member in the database. Searchentry is the search string (limited by the maxcharactername size), 
   * and number the number of results
   */
  def searchMember(searchentry: String, number: Int) = Connection.withConnection{
    
       username => userID => implicit request => lang =>
    Instantiations.Tables.db.withSession {
       implicit session =>
    if (searchentry.length < Application.maxCharacternames && number <= Application.maxResults){
      val explicitInnerJoinDebateYesOrNo = for {
  		  ((member, memberInfo), memberProfilePicture) <- 
  		  Instantiations.Tables.members innerJoin Instantiations.Tables.membersInfo on
  		  ((member, memberInfo) => (member.username like "%" + searchentry + "%") && member.id === memberInfo.id /*&& conversationMember.modificationTimestamp < maxTimestampParam*/) leftJoin Instantiations.Tables.membersProfilePicture on (_._2.id === _.id) 
      } yield (member.username, memberInfo, memberProfilePicture)
     
      val results = explicitInnerJoinDebateYesOrNo.take(number).list
      val resultsParsed = results.par.map(x => memberSearchEntry(x._1, convertMemberInfoDatabaseEntryToClass(x._2), convertProfilePictureDatabaseEntryToClass(x._3))).seq
      Ok(Json.toJson(resultsParsed))
      } else {
        NotFound
      }
    }
    
  }
  
  /*
   * Giives the username with the userID
   */
  def gettUsercameFromID(userID: Long): String = {
     Instantiations.Tables.db.withSession {
       implicit session =>
       val q2 = for {c <- Instantiations.Tables.members if c.id === userID} yield (c.username)
        val resultsList = q2.list

        if (resultsList != Nil)
          resultsList.head
        else
          null
     }
   }

  def getUsernameFromIDJSON(userIDInt: Long) = {
     Connection.withConnection{
       username => userID => implicit request => lang =>
         Ok(Json.toJson(gettUsercameFromID(userIDInt)))

     }
  }

   /*
    * Modifies the information of the member page
    */
   def processMemberInfoModificationForm = Connection.withConnection({
       username => userID => implicit request => lang =>
       var modificationOk = false
	   models.Forms.memberInfoForm.bindFromRequest.value map { memberInfoForm =>
	     Instantiations.Tables.db.withSession {
	    	 implicit session =>
	    	 val userIDInt = userID.toLong
		     val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userIDInt} yield (c.firstname, c.name, c.mail, c.ville, c.presentation)
		       Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
		         
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implmentation, because the old session will be closed when this thread requests will execute
	           q2.update((memberInfoForm.firstname, memberInfoForm.name, memberInfoForm.mail, memberInfoForm.ville, Jsoup.clean(memberInfoForm.presentation, Application.whiteListHTML)))
            }
		       }
	         ActionRepport.actionDone(userIDInt, 0, userIDInt, userIDInt, 1, null)

	         modificationOk = true

	     }
       }
	   if (modificationOk){
	     Redirect(routes.Application.memberInfo())
	   }
	   else{
	     BadRequest
	   }
     }, None, true)

   /*
     * Returns the list of members ordered by their number of likes. 
     * Set since to 0 to consider the likes of all times
     * Set since to 1 to consider the likes of last month
    */
   def getMembersOrderedByLikes(maxLikes: Long, number: Int, since: Int = 0) = Connection.withConnection{
     username => userID => implicit request => lang =>
     var maxLikesReal = maxLikes
     if (maxLikes == -1){
       maxLikesReal = Long.MaxValue
     }
     Instantiations.Tables.db.withSession {
       implicit session =>
       var q2 = for {c <- Instantiations.Tables.membersStats} yield (c.idMember, c.numberLikes)
       if (since == 1)
         q2 = for {c <- Instantiations.Tables.membersStatsMonthly} yield (c.idMember, c.numberLikes)
       //We associate the memberInfo for each id
       val memberInfoAssociated = for {
  	     (memberIds, memberInfo) <- q2 innerJoin Instantiations.Tables.membersInfo on 
  	     ((memberIds, memberInfo) => memberIds._1 === memberInfo.id)} yield (memberIds, memberInfo)
       
  	   //We only show members with the correct main language
       val filterMembersLanguage = for {(request, memberLanguage) <- memberInfoAssociated innerJoin User.requestForUserLanguages(userID.toLong) on ((request, memberLanguage) => request._2.lang === memberLanguage.language)} yield request
       
       val request = for {
  	   (memberIds, follow) <- filterMembersLanguage.sortBy(_._1._2.desc).take(number) leftJoin Instantiations.Tables.follow.filter(x => x.memberId === userID.toLong && x.objectFollowedType === 0) on
  	   ((memberIds, follow) => memberIds._1._1 === follow.objectFollowedID)
  	   } yield (memberIds, follow.strenghOfLink?)
       val membersToRetrieve = request.list.par.map(x => Json.toJson(convertMemberInfoDatabaseEntryToClass(x._1._2).copy(isUserFollowing = {if(x._2 == None){0}else{1}})).as[JsObject] + ("numberLikes" -> Json.toJson(x._1._1._2))).seq
       Ok(Json.toJson(membersToRetrieve))
     }
   }
     
   /*
    * Modifies the information of the member page

   def processSubscriptionPicForm = {
     Connection.withConnection{
       username => userID => implicit request => lang =>
       var modificationOk = false
	   models.Forms.memberInfoForm.bindFromRequest.value map { memberInfoForm =>
	     Instantiations.Tables.db.withSession {
	    	 implicit session =>
	    	 val userIDInt = userID.toLong
		     val pictureu = subscriptionPicForm.pictureu //url
         val picturex = -1//subscriptionPicForm.picturex //offx
         val picturey = -1//subscriptionPicForm.picturey
         val picturem = -1//subscriptionPicForm.picturem //zoom : margin string
         val picturemx = -1//subscriptionPicForm.picturemx //min start
         val picturemy = -1//subscriptionPicForm.picturemy
         val picturemm = -1//subscriptionPicForm.picturemm
	       //We then create the profile picture entry
	       //(id, picture_url, picture_offx, picture_offy, picture_zoom, picturemin_offx, picturemin_offy, picturemin_zoom)
	       //IMPORTANT Instantiations.Tables.membersProfilePicture += (userIDInt, pictureu, picturex, picturey, picturem, picturemx, picturemy, picturemm)
         val q2 = for {c <- Instantiations.Tables.membersProfilePicture if c.id === userIDInt} yield (c.firstname, c.name, c.mail, c.ville, c.presentation)
	         q2.update((memberInfoForm.firstname, memberInfoForm.name, memberInfoForm.mail, memberInfoForm.ville, memberInfoForm.presentation))
	         ActionRepport.actionDone(userIDInt, userIDInt, userIDInt, 1, null)

	         modificationOk = true

	     }
       }
	   if (modificationOk){
	     Redirect(routes.Application.memberInfo())
	   }
	   else{
	     BadRequest
	   }
     }
   }
*/


   /*
    * Takes a username string in parameter and returns the information as a models.userInfo from database
    */
   def doesMemberExistsFromPseudo(username: String): Boolean = {
     Instantiations.Tables.db.withSession {
       implicit session =>
         val q2 = for {c <- Instantiations.Tables.members if c.username === username} yield c
         val resultsList = q2.list
         (resultsList != Nil)
     }
   }

   def getUserStatsJSON(userIDInt: Long) = {
     Connection.withConnection{
       username => userID => implicit request => lang =>
           
           val userStatsObject = getUserStatsObject(userIDInt)
           implicit val wrtier = User.memberStatsEntryWrites
           Ok(Json.toJson(userStatsObject)(wrtier))

     }
  }
   
   def getUserStatsObject(userIDInt: Long) = {
     Instantiations.Tables.db.withSession {
       implicit session =>
         val valueFromCache = Cache.getAs[models.MemberStats]("memberStats" + userIDInt)
         if (valueFromCache == None){
           val q2 = for {c <- Instantiations.Tables.membersStats if c.idMember === userIDInt} yield c
           val result = q2.list
           if (result != Nil){
             var memberStatsToReturn = convertMemberStatsDatabaseEntryToClass(result.head)
             
             //For the ranking, we count the number of people before and after
             val numberLikes = memberStatsToReturn.numberLikes
             val numberBehind = Instantiations.Tables.membersStats.filter(_.numberLikes < numberLikes).length.run
             val numberFront = Instantiations.Tables.membersStats.filter(_.numberLikes >= numberLikes).length.run
             memberStatsToReturn = memberStatsToReturn.copy(ranking = numberFront, totalNumberMembers=(numberFront + numberBehind));
             Cache.set("memberStats" + userIDInt, memberStatsToReturn, Application.cacheDuraction)
             memberStatsToReturn
           }
           else{
             null
           }
         }
         else{
           valueFromCache.get
         }
     }
   }
   
   /*
    * Takes a username string in parameter and returns the id. Returns -1 if the username doesn't exists
    */
   def getMemberIdFromPseudo(username: String): Long = {
     Instantiations.Tables.db.withSession {
       implicit session =>
         val q2 = for {c <- Instantiations.Tables.members if c.username === username} yield c.id
         val resultsList = q2.list
         if (resultsList != Nil){
           resultsList.head
         }
         else{
           -1
         }
     }
   }

   /*
    * Takes a memberID in parameter and returns the information as a models.userInfo from database. 
    * Optional parameter : userIDLong in order to know if the user follows this member
    */
   def getMemberInfoFromID(memberID: Long, userIDLong: Long = -1): models.MemberInfo = {
     Instantiations.Tables.db.withSession {
       implicit session =>
       val valueFromCache = Cache.getAs[models.MemberInfo]("member" + memberID)
       if (valueFromCache == None){
         var memberInfo = for {c <- Instantiations.Tables.membersInfo if c.id === memberID} yield c
         
         //If the user specified his userID, we search if he is following the member in order to directly display it (in the json for example, in order to avoid doing too much requests)
         //If he didnt specify, the right part of the join will be empty (there is no user with userID -1)
         val memberAndFollow = for {(memberInfo, follow) <- memberInfo leftJoin Instantiations.Tables.follow.filter(x => x.memberId === userIDLong && x.objectFollowedType === 0) on 
           ((memberInfo, follow) => memberInfo.id === follow.objectFollowedID)} yield (memberInfo, follow.strenghOfLink?)
          
         val resultsList = memberAndFollow.list
         if (resultsList != Nil){
           val infoFromDB = resultsList.head
           var a = convertMemberInfoDatabaseEntryToClass(infoFromDB._1)
           
           if (infoFromDB._2 != None){//If we found a follow entry for this member on the user side
             a = a.copy(isUserFollowing = 1)
           }
           
           Cache.set("member" + memberID, a, Application.cacheDuraction)
           a
         }
         else{
           Cache.set("member" + memberID, null, Application.cacheDuraction)
           null
         }
       }
       else{
         valueFromCache.get
       }
     }
   }

    /*
    * returns if an user exists
    */
   def userExists(userID: Long) = {
     (getMemberInfoFromID(userID) != null)
   }


   /*
   * Retrieves the json of the  last x groups created before some timestamp that a member joined
   */
  def getGroupsOfAnMemberOrderedbyTimestampJSON(memberID: Long, maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => lang =>{
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        if (number <= controllers.Application.maxResults && number >= 0){
          //groupMembership.id WHERE groupMembership.memberId === userID
          val explicitInnerJoin = for {
      		  (groupMember, group) <- Instantiations.Tables.groupMembership.filter(x => x.memberId === memberID && x.modificationTimestamp < maxTimestampParam).sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.groups.filter(_.logicalDelete === 0) on
      		  ((groupMember, group) =>
      		    groupMember.groupId === group.id
      		  )
    		  } yield group
          val results = explicitInnerJoin.take(number).list.par.map(x => Groups.convertGroupDatabaseEntryToClass(x)).seq
          implicit val wrtier = Groups.groupWrites
          Ok(Json.toJson(results))
        }
        else{
          BadRequest
        }
      }
    }
  }

  /*
   * Modifies firstname. Returns 1 if it succeeded and 0 if it failed
   */
  def processMemberFirstNameModification() = Connection.withConnection({
    username => userID => implicit request => lang =>
      Instantiations.Tables.db.withSession {
       implicit session =>
       var modificationOk = false
       val args = request.body.asFormUrlEncoded.get
        if (args != None && args.contains("value")){
          val userIDInt = userID.toLong
          Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
            
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session will be closed when this thread requests will execute
            val userFirstName = args.get("value").get(0).toString
  		      val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userIDInt} yield c.firstname
            q2.update(userFirstName)
            Cache.remove("member" + userIDInt)
            }
          }
	        ActionRepport.actionDone(userIDInt, 0, userIDInt, userIDInt, 2, null)
	        Ok(Json.toJson(1))
        }
        else{
          Ok(Json.toJson(0))
        }
      }
  }, None, true)

   def processMembertest(id: String) = Connection.withConnection{
    username => userID => implicit request => lang =>
     println(id)
     Ok
   }

  /*
   * Modifies name. Returns 1 if it succeeded and 0 if it failed
   */
  def processMemberNameModification() = Connection.withConnection({
    username => userID => implicit request => lang =>
      Instantiations.Tables.db.withSession {
       implicit session =>
       var modificationOk = false
       val args = request.body.asFormUrlEncoded.get
        if (args != None && args.contains("value")){
          val userIDInt = userID.toLong
          Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session will be closed when this thread requests will execute
              val userName = args.get("value").get(0).toString
    		      val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userIDInt} yield c.name
              q2.update(userName)
              Cache.remove("member" + userIDInt)
            }
          }
	        ActionRepport.actionDone(userIDInt, 0, userIDInt, userIDInt, 3, null)
	        Ok(Json.toJson(1))
        }
        else{
          Ok(Json.toJson(0))
        }
      }
  }, None, true)

  /*presentation
   * Modifies ville. Returns 1 if it succeeded and 0 if it failed
   */
  def processMemberVilleModification() = Connection.withConnection({
    username => userID => implicit request => lang =>
      Instantiations.Tables.db.withSession {
       implicit session =>
       var modificationOk = false
       val args = request.body.asFormUrlEncoded.get
        if (args != None && args.contains("value")){
          val userIDInt = userID.toLong
          Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
              val userVille = args.get("value").get(0).toString
    		      val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userIDInt} yield c.ville
              q2.update(userVille)
              Cache.remove("member" + userIDInt)
            }
          }
	      ActionRepport.actionDone(userIDInt, 0, userIDInt, userIDInt, 4, null)
	      Ok(Json.toJson(1))
        }
        else{
          Ok(Json.toJson(0))
        }
      }
  }, None, true)

  /*
   * Modifies mail. Returns 1 if it succeeded and 0 if it failed
   */
  def processMemberMailModification() = Connection.withConnection({
    username => userID => implicit request => lang =>
      Instantiations.Tables.db.withSession {
       implicit session =>
       var modificationOk = false
       val args = request.body.asFormUrlEncoded.get
        if (args != None && args.contains("value")){
          val userIDInt = userID.toLong
          Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
              
              val userMail = args.get("value").get(0).toString
    	    	  val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userIDInt} yield c.mail
              q2.update(userMail)
              Cache.remove("member" + userIDInt)
            }
          }
	        ActionRepport.actionDone(userIDInt, 0, userIDInt, userIDInt, 5, null)
	        Ok(Json.toJson(1))
        }
        else{
          Ok(Json.toJson(0))
        }
      }
  }, None, true)

   /*
   * Modifies presentation. Returns 1 if it succeeded and 0 if it failed
   */
  def processMemberPresentationModification() = Connection.withConnection({
    username => userID => implicit request => lang =>
      Instantiations.Tables.db.withSession {
       implicit session =>
       var modificationOk = false
       val args = request.body.asFormUrlEncoded.get
        if (args != None && args.contains("value")){
          val userIDInt = userID.toLong
          Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
              val userPresentation = args.get("value").get(0).toString
    	    	  val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userIDInt} yield c.presentation
              q2.update(Jsoup.clean(userPresentation, Application.whiteListHTML))
              Cache.remove("member" + userIDInt)
            }
          }
	        ActionRepport.actionDone(userIDInt, 0, userIDInt, userIDInt, 6, null)
	        Ok(Json.toJson(1))
        }
        else{
          Ok(Json.toJson(0))
        }
      }
  }, None, true)

  def activateDeactivateSendMail() = Connection.withConnection({
    username => userID => implicit request => lang =>
      Instantiations.Tables.db.withSession {
       implicit session =>
       var modificationOk = false
       var jsonToSend = 0
       val args = request.body.asFormUrlEncoded.get
       if (args != None && args.contains("value")){
         val userIDInt = userID.toLong
         val newSendMailValue = args.get("value").get(0).toInt
         if (newSendMailValue == 0 || newSendMailValue == 1){
           Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
             Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
               val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userIDInt} yield c.sendMail
               q2.update(newSendMailValue)
               Cache.remove("memberSendMail" + userIDInt)
               
               val statement = q2.updateStatement
               val invoker = q2.updateInvoker
             }
           }
           jsonToSend = 1
         }
       }
       Ok(Json.toJson(jsonToSend))
      }
  }, None, true)
   
  def isSendMailActivatedJSON = Connection.withConnection {
    username => userID => implicit request => lang =>
      val userSendMail = getUserSendMail(userID.toLong)
      Ok(Json.toJson(userSendMail))
  }
  
  /*
   * Gets user sendMail. 0 if the user don't want any more mails and 1 if he accepts
   */
  def getUserSendMail(userID: Long): Int = {
    Instantiations.Tables.db.withSession {
      implicit session =>
        val valueFromCache = Cache.getAs[Int]("memberSendMail" + userID)
        if (valueFromCache == None){
          val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userID} yield c.sendMail
          val value = q2.list.head
          Cache.set("memberSendMail" + userID, value, Application.cacheDuraction)
          value
        }
        else{
          valueFromCache.get
        }
    }
  }
  
  /*
   * Follows an object defined by POSTDATA objectFollowed (the object type)  and idObject (the id of this object). 
   * Returns Json 1 if it succeeded and 0 if not
   * idObject can be ; 
   * 0 for a member
   * 1 for a group
   * 2 for a concept
   * 3 for a debate
   * 4 for an action
   * 5 for a petition
   * 
   */

  def follow() = Connection.withConnection({
    username => userID => implicit request => lang =>
      val args = request.body.asFormUrlEncoded.get
      if (args != None && args.contains("objectFollowed") && args.contains("idObject")){
        val objectFollowed = Integer.parseInt(args.get("objectFollowed").get(0).toString)
        val idObjectFollowed = args.get("idObject").get(0).toString.toLong
        val userIDInt = userID.toLong
        
        Ok(Json.toJson(followThisObject(userIDInt, objectFollowed, idObjectFollowed)))
      }
      else{
        Ok(Json.toJson(0))
      }
  }, None, true)
  
  /*
   * Follows the object defined by objectFollowed (the object type) and idObjectFollowed (the object id) for an userID
   * Returns 1 if succeeded and 0 if failed
   */
  def followThisObject(userID: Long, objectFollowed: Int, idObjectFollowed: Long) = {
    
    if (!isFollowing(userID, objectFollowed, idObjectFollowed)) {
      Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
        Instantiations.Tables.db.withSession {
          implicit session =>
          Instantiations.Tables.follow += ((userID, objectFollowed, idObjectFollowed, 200, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis()))))
          if (objectFollowed == 0){
            sqlu"update MEMBERSTATS set FOLLOWERS=FOLLOWERS+1 where IDMEMBER = $idObjectFollowed".first
            Cache.remove("memberStats" + idObjectFollowed)
            ActionRepport.actionDone(userID, 0, idObjectFollowed, idObjectFollowed, 100, null) //For the moment, we report only when an members follows another member, because follows for other type of objects are irrelevant and could cause privacy problems
          }
        }
      }
      1
    }
    else {
      0
    }
  }
  
  def unFollow() = Connection.withConnection({
    username => userID => implicit request => lang =>
      
          val args = request.body.asFormUrlEncoded.get
          if (args != None && args.contains("objectFollowed") && args.contains("idObject")){
            val objectFollowed = Integer.parseInt(args.get("objectFollowed").get(0).toString)
            val idObjectFollowed = args.get("idObject").get(0).toString.toLong
            val userIDInt = userID.toLong
            
            Ok(Json.toJson(unfollowThisObject(userIDInt, objectFollowed, idObjectFollowed)))
          }
          Ok(Json.toJson(0))
  }, None, true)

  def unfollowThisObject(userID: Long, objectFollowed: Int, idObjectFollowed: Long) = {
      if (isFollowing(userID, objectFollowed, idObjectFollowed)){
        Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
          Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
          Instantiations.Tables.follow.filter(x => (x.memberId === userID && x.objectFollowedType === objectFollowed && x.objectFollowedID === idObjectFollowed)).delete
          if (objectFollowed == 0){
            sqlu"update MEMBERSTATS set FOLLOWERS=FOLLOWERS-1 where IDMEMBER = $idObjectFollowed".first
            Cache.remove("memberStats" + idObjectFollowed)
          }
          }
        }
        1
      }
      else{
        0
      }
    
  }

  def isUserFollowingJSON(objectFollowed: Int, idObjectFollowed: Long) = Connection.withConnection{
    username => userID => implicit request => lang =>
      if (isFollowing(userID.toLong, objectFollowed, idObjectFollowed))
        Ok(Json.toJson(1))
      else
        Ok(Json.toJson(0))
  }

  def isFollowing(userID: Long, objectFollowed: Int, idObjectFollowed: Long) = Instantiations.Tables.db.withSession {
    implicit session =>
      val q2 = for {c <- Instantiations.Tables.follow if c.memberId === userID && c.objectFollowedType === objectFollowed && c.objectFollowedID === idObjectFollowed} yield c.strenghOfLink
      (q2.list != Nil)
  }

  def objectsFollowed(objectFollowed: Int, maxTimestamp: Long, number: Int) = Connection.withConnection{
    username => userID => implicit request => lang =>
      Instantiations.Tables.db.withSession { implicit session =>
        val userIDInt = userID.toLong
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        //println((for {c <- Instantiations.Tables.follow if c.memberId === userIDInt && c.objectFollowedType === objectFollowed} yield (c.objectFollowedID, c.modificationTimestamp)).list)
        var jsonToSend = objectFollowed match {
          case 0 => {
            //member
            //val request = (for {c <- Instantiations.Tables.follow if c.memberId === userIDInt && c.objectFollowedType === objectFollowed} yield (c.objectFollowedID, c.modificationTimestamp)).list
            val explicitInnerJoin = for {
		    (follow, memberInfo) <- Instantiations.Tables.follow.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.membersInfo on
		    ((follow, memberInfo) => follow.memberId === userIDInt && follow.objectFollowedType === 0 && follow.objectFollowedID === memberInfo.id && follow.modificationTimestamp < maxTimestampParam)
		    } yield (memberInfo, follow.modificationTimestamp)
		    
		    
		    
		    //UserWrites is already defined
            Json.toJson(explicitInnerJoin.take(number).list.par.map(x => convertMemberInfoDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime())).seq)
          }
          case 1 => {
            //group
            val explicitInnerJoin = for {
		    (follow, group) <- Instantiations.Tables.follow.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.groups on
		    ((follow, group) => follow.memberId === userIDInt && follow.objectFollowedType === 1 && follow.objectFollowedID === group.id && follow.modificationTimestamp < maxTimestampParam)
		    } yield (group, follow.modificationTimestamp)
		    implicit val groupWrites = Groups.groupWrites
		    Json.toJson(explicitInnerJoin.take(number).list.par.map(x => Groups.convertGroupDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime())).seq)
          }
          case 2 => {
            //concept
            val explicitInnerJoin = for {
		    (follow, concept) <- Instantiations.Tables.follow.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.concepts on
		    ((follow, concept) => follow.memberId === userIDInt && follow.objectFollowedType === 2 && follow.objectFollowedID === concept.id && follow.modificationTimestamp < maxTimestampParam)
		    } yield (concept, follow.modificationTimestamp)
		    implicit val conceptWrites = Concepts.conceptWrites
		    Json.toJson(explicitInnerJoin.take(number).list.par.map(x => Concepts.convertConceptDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime())).seq)
          }
          case 3 => {
            //debate
            val explicitInnerJoin = for {
		    (follow, debate) <- Instantiations.Tables.follow innerJoin Instantiations.Tables.debate on
		    ((follow, debate) => follow.memberId === userIDInt && follow.objectFollowedType === 3 && follow.objectFollowedID === debate.id && follow.modificationTimestamp < maxTimestampParam)
		    } yield (debate, follow.modificationTimestamp)
		    implicit val debateWrites = Debates.debateWrites
		    Json.toJson(explicitInnerJoin.sortBy(_._2.desc).take(number).list.par.map(x => Debates.convertDebateDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime(), isUserFollowing = 1)).seq)
          }
          case 4 => {
            //action
            val explicitInnerJoin = for {
		    (follow, action) <- Instantiations.Tables.follow.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.actions on
		    ((follow, action) => follow.memberId === userIDInt && follow.objectFollowedType === 4 && follow.objectFollowedID === action.id && follow.modificationTimestamp < maxTimestampParam)
		    } yield (action, follow.modificationTimestamp)
		    implicit val actionWrites = Actions.actionWrites
		    Json.toJson(explicitInnerJoin.take(number).list.par.map(x => Actions.convertActionDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime())).seq)
          }
          case 5 => {
            //petition
            val explicitInnerJoin = for {
		    (follow, petition) <- Instantiations.Tables.follow.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.petition on
		    ((follow, petition) => follow.memberId === userIDInt && follow.objectFollowedType === 5 && follow.objectFollowedID === petition.id && follow.modificationTimestamp < maxTimestampParam)
		    } yield (petition, follow.modificationTimestamp)
		    implicit val petitionWrites = Petitions.petitionWrites
		    Json.toJson(explicitInnerJoin.take(number).list.par.map(x => Petitions.convertPetitionDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime())).seq)
          }
          case _ =>{
            Json.toJson("")
          }
          //case _ => (for {c <- Instantiations.Tables.follow if c.memberId === userIDInt && c.objectFollowedType === objectFollowed} yield (c.objectFollowedID, c.modificationTimestamp)).list
        }
        Ok(jsonToSend)

      }
  }

  /*
   * Retrieves the list of the members followed by this member (defined by memberId)
   */
  
  def membersFollowed(memberID: Long, maxTimestamp: Long, number: Int) = Connection.withConnection{
    username => userID => implicit request => lang =>
      val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
      //val q2 = for {c <- Instantiations.Tables.follow.filter(x => x.memberId === userID && x.objectFollowedType === 0 && x.modificationTimestamp < maxTimestampParam)} yield (c.objectFollowedID, c.modificationTimestamp)
      Instantiations.Tables.db.withSession { implicit session =>
        val explicitInnerJoin = for {
  		    (follow, memberInfo) <- Instantiations.Tables.follow.sortBy(_.modificationTimestamp.desc).filter(follow => follow.memberId === memberID && follow.objectFollowedType === 0 && follow.modificationTimestamp < maxTimestampParam).take(number) innerJoin Instantiations.Tables.membersInfo on
  		    ((follow, memberInfo) => follow.objectFollowedID === memberInfo.id)
  		    } yield (memberInfo, follow.modificationTimestamp)
  		    
  		  val explicitInnerJoin2 = addUserFollowingToMembersInfoRequest(explicitInnerJoin, userID.toLong)
  		  
        val memberList = explicitInnerJoin2.list.par.map(x => convertMemberInfoDatabaseEntryToClass(x._1._1).copy(isUserFollowing = {if(x._2 == None){0}else{1}}, modificationTimestamp = x._1._2.getTime())).seq   
        Ok(Json.toJson(memberList))
      }
  }
  /*
   * Retrieves the list of the members that are following this member (defined by memberId)
   */
 def membersFollowing(memberID: Long, maxTimestamp: Long, number: Int) = Connection.withConnection{
   username => userID => implicit request => lang =>
   val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
   Instantiations.Tables.db.withSession { implicit session =>
     val explicitInnerJoin = for {
  	   (follow, memberInfo) <- Instantiations.Tables.follow.sortBy(_.modificationTimestamp.desc).filter(follow => follow.objectFollowedType === 0 && follow.objectFollowedID === memberID && follow.modificationTimestamp < maxTimestampParam).take(number) innerJoin Instantiations.Tables.membersInfo on
  	   ((follow, memberInfo) => follow.memberId === memberInfo.id)
  	  } yield (memberInfo, follow.modificationTimestamp)
  	  
  	 //For each of them, we check if the user is following them in order to directly display it to the json
  	 val explicitInnerJoin2 = addUserFollowingToMembersInfoRequest(explicitInnerJoin, userID.toLong)
  	  
     val memberList = explicitInnerJoin2.list.par.map(x => convertMemberInfoDatabaseEntryToClass(x._1._1).copy(isUserFollowing = {if(x._2 == None){0}else{1}},modificationTimestamp = x._1._2.getTime())).seq   
     Ok(Json.toJson(memberList))
   }
   
 }
  
 
 /*
  * Adds an isUserFollowing in a request for a list of memberInfo.
  * Transforms a request returning (MemberInfo, ModificationTimestamp) to ((MemberInfo, ModificationTimestamp), isUserFollowing)
  */
 def addUserFollowingToMembersInfoRequest(requestMemberInfo: slick.lifted.Query[(models.MembersInfoTable, slick.lifted.Column[java.sql.Timestamp]),(models.Types.memberInfoEntry, java.sql.Timestamp),Seq], userID: Long): slick.lifted.Query[((models.MembersInfoTable, slick.lifted.Column[java.sql.Timestamp]), slick.lifted.Column[Option[Long]]),((models.Types.memberInfoEntry, java.sql.Timestamp), Option[Long]),Seq] = {
   val request = for {
  	   (membersInfos, follow) <- requestMemberInfo leftJoin Instantiations.Tables.follow.filter(x => x.memberId === userID.toLong && x.objectFollowedType === 0) on
  	   ((membersInfos, follow) => membersInfos._1.id === follow.objectFollowedID)
  	  } yield (membersInfos, follow.strenghOfLink?)
  request
 }
 
 def processProfilePictureModification = Connection.withConnection({
    username => userID => implicit request => lang =>
      var jsonToSend = 0
      
      request.body.asMultipartFormData.map {
        
        fileOnBody => fileOnBody.file("picture").map { picture =>
          
          //Test if it is a valid image
          //Image img = ImageIO.read(new File(name));
          /*
           * import javax.imageio.ImageIO
import java.awt.image.BufferedImage
           */
      println(request.body.asFormUrlEncoded);
          val args = request.body.asFormUrlEncoded
          var endOfTitle = ""
          if (args != None && args.get.contains("endOfFile")){
            endOfTitle = args.get.get("endOfFile").get(0).toString
          }
          
          val image = ImageIO.read(picture.ref.file);
          println(picture.ref.file.length)
          if (
              //(new MimetypesFileTypeMap()).getContentType(picture.ref.file).split("/")(0) == "image"
              image != null
              &&
              picture.ref.file.length < Application.maxSizeOfProfilePictures){
            Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
              //Now we convert the image
              val fileJPG = new File("/tmp/jpeg.jpg")
              ImageIO.write(image, "jpg", fileJPG);
              
              
              val fileInputStream = new FileInputStream(fileJPG);
              val objectMetadata = new ObjectMetadata()
              objectMetadata.setContentType("image/jpeg")
              val putObjectRequest = new PutObjectRequest(AWS_S3_BUCKET, username + ".jpg", fileInputStream, objectMetadata);
              
              val yourAWSCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
              val amazonS3Client = new AmazonS3Client(yourAWSCredentials)
              // This will create a bucket for storage
              //amazonS3Client.createBucket(AWS_S3_BUCKET) 
              //amazonS3Client.putObject(AWS_S3_BUCKET, , picture.ref.file)
              println(amazonS3Client.putObject(putObjectRequest))
              }
            }
            jsonToSend = 1
          }
        }
      }
      Ok(Json.toJson(jsonToSend))
  }, None, true)
  
  def processFileSending = Connection.withConnection({
    username => userID => implicit request => lang =>
      var jsonToSend = "none"
      request.body.asMultipartFormData.map {
        fileOnBody => fileOnBody.file("picture").map { picture =>
          //Test if it is a valid image
          
          val args = request.body.asFormUrlEncoded.get
          var endOfTitle = Random.alphanumeric.take(50).mkString
          val image = ImageIO.read(picture.ref.file);
          if (
              //(new MimetypesFileTypeMap()).getContentType(picture.ref.file).split("/")(0) == "image"
              image != null
              &&
              picture.ref.file.length < Application.maxSizeOfProfilePictures){
            Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
              //Now we convert the image
              val fileJPG = new File("/tmp/jpeg.jpg")
              ImageIO.write(image, "jpg", fileJPG);
              
              val AWS_S3_BUCKET = "idmcr"
              val AWS_ACCESS_KEY = "AKIAJRAOJ2B24LTBLHIA"
              val AWS_SECRET_KEY = "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"
              val fileInputStream = new FileInputStream(fileJPG);
              val objectMetadata = new ObjectMetadata()
              objectMetadata.setContentType("image/jpeg")
              val putObjectRequest = new PutObjectRequest(AWS_S3_BUCKET, username + endOfTitle + ".jpg", fileInputStream, objectMetadata);
              
              val yourAWSCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
              val amazonS3Client = new AmazonS3Client(yourAWSCredentials) 
              // This will create a bucket for storage
              //amazonS3Client.createBucket(AWS_S3_BUCKET) 
              //amazonS3Client.putObject(AWS_S3_BUCKET, , picture.ref.file)
              println(amazonS3Client.putObject(putObjectRequest))
            }
          }
            jsonToSend = username + endOfTitle
          }
        }
      }
      Ok(Json.toJson(jsonToSend))
  }, None, true)
  
  /*def templateProfileElementModification(element: String, q2: slick.lifted.WrappingQuery[Instantiations.Tables.membersInfo], request: Request[AnyContent], userIDInt: Int) = {
      Instantiations.Tables.db.withSession {
       implicit session =>
       var modificationOk = false
       val args = request.body.asFormUrlEncoded.get
        if (args != None && args.contains(element)){

          val userElement = args.get(element).get(0).toString

          q2.update(userElement)
        }
      }
  }*/

  /*def onlyShowResultsInLanguage(query: slick.lifted.WrappingQuery) = {
    0
  }*/
  
  /*
   * Gives the membersLanguage table results corresponding to an userID (so the languages of the user in the database)
   */
  def requestForUserLanguages(userID: Long) = Instantiations.Tables.membersLanguage.filter(_.idMember === userID)

  
  /*
   * Adds the language specified in the userLanguageForm to the user ones. Returns JSON 1 if succeeded and JSON 0 if failed
   */
  def addNewLanguage() = Connection.withConnection({
    username => userID => implicit request => lang =>
      val userIDLong = userID.toLong
      var jsonToSend = 0
      models.Forms.userLanguageForm.bindFromRequest.value map { userLanguageForm =>
        val languageToAdd = userLanguageForm.language
        Instantiations.Tables.db.withSession { implicit session =>
          //We test that this language is a valid one and that the user has not already this language 
          if (Application.langs.contains(languageToAdd) && !requestForUserLanguages(userIDLong).list.contains(languageToAdd)){
            Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
                Instantiations.Tables.membersLanguage += (userIDLong, languageToAdd)
              }
            }
            jsonToSend = 1
          }
        }
      }
      Ok(Json.toJson(jsonToSend))
  }, None, true)
  
  /*
   * Removes the language specified in userLanguageForm for this user. Returns JSON 1 if succeeded and JSON 0 if failed, and 2 if it failed because the language is the session language
   */
  def removeLanguage() = Connection.withConnection({
    username => userID => implicit request => lang =>
      val userIDLong = userID.toLong
      var jsonToSend = 0
      models.Forms.userLanguageForm.bindFromRequest.value map { userLanguageForm =>
        val languageToAdd = userLanguageForm.language
        Instantiations.Tables.db.withSession { implicit session =>
          //We test that this is not the session language
          //println(Instantiations.getLangText(request) + " " + languageToAdd)
          if (Instantiations.getLangText(request) != languageToAdd){
            //We test that this language is a valid one and that the user has not already this language 
            jsonToSend = Instantiations.Tables.membersLanguage.filter(entry => entry.idMember === userIDLong && entry.language === languageToAdd).delete
          }
          else{
            jsonToSend = 2
          }
        }
      }
      Ok(Json.toJson(jsonToSend))
  }, None, true)
  
  def retrieveUserLanguagesJSON() =  Connection.withConnection{
    username => userID => implicit request => lang =>
      Instantiations.Tables.db.withSession { implicit session =>
        val userLanguagesList = requestForUserLanguages(userID.toLong).list
        val memberLanguage = models.MemberLanguages(userLanguagesList.head._1, userLanguagesList.map(_._2))
        //implicit val writer = memberLanguageEntryWrites
        Ok(Json.toJson(memberLanguage))
      }
  }
 
 /*
  * PARALLEL FUNCTION
  * Modifies the number of likes of an user
  */
  def modifynumberofLikes(userID: Long, numberOfLikesDifference: Long) {
    Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      { 
          val tuple = (userID, numberOfLikesDifference)
          sqlu"update MEMBERSTATS set LIKES=LIKES+${tuple._2} where IDMEMBER = ${tuple._1}".first
          
          if (Instantiations.Tables.membersStatsMonthly.filter(_.idMember === userID).exists.run){
            sqlu"update MEMBERSTATSMONTHLY set LIKES=LIKES+${tuple._2} where IDMEMBER = ${tuple._1}".first
          }
          else{
            if (numberOfLikesDifference > 0)
              Instantiations.Tables.membersStatsMonthly += (userID, numberOfLikesDifference, 0l)
          }
        }
    }
  }
  
  /*
  * PARALLEL FUNCTION
  * Modifies the number of dislikes of an user
  */
  def modifynumberofDislikes(userID: Long, numberOfDislikesDifference: Long) {
    Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      Instantiations.Tables.db.withSession { implicit session =>
        val tuple = (userID, numberOfDislikesDifference)
          sqlu"update MEMBERSTATS set DISLIKES=DISLIKES+${tuple._2} where IDMEMBER = ${tuple._1}".first
        if (Instantiations.Tables.membersStatsMonthly.filter(_.idMember === userID).exists.run){
          sqlu"update MEMBERSTATSMONTHLY set DISLIKES=DISLIKES+${tuple._2} where IDMEMBER = ${tuple._1}".first
        }
        else{
          if (numberOfDislikesDifference > 0)
            Instantiations.Tables.membersStatsMonthly += (userID, 0l, numberOfDislikesDifference)
        }
      }
    }
  }
  
  def getMemberMails() = Connection.withConnection{
    username => userID => implicit request => lang =>
      Instantiations.Tables.db.withSession { implicit session =>
        //We unifiate memberInfo and username
        val explicitInnerJoinMembers = for {
  		  (member, memberInfo) <- 
  		  Instantiations.Tables.members innerJoin Instantiations.Tables.membersInfo on
  		  ((member, memberInfo) => member.id === memberInfo.id) } yield (memberInfo, member.username)
  		  
  		  //We retrieve te mails of friends and see if they already have a Democras account
         val leftJoinQ = for {
    		  (membersFriendsMails, allmember) <- Instantiations.Tables.membersFriendsMails.filter(x => x.id === userID.toLong && x.sentMail === 0l) leftJoin explicitInnerJoinMembers on
    		  ((membersFriendsMails, allmember) =>
    		    membersFriendsMails.friendEmail === allmember._1.mail
    		  )
  		  } yield (membersFriendsMails, allmember._1.id?, allmember._2?)
  		  
  		  //We check the ones the user already follows
  		  val explicitInnerJoin = for {
  		  (results, follow) <- leftJoinQ leftJoin Instantiations.Tables.follow.filter(x => x.memberId === userID.toLong && x.objectFollowedType === 0) on
  		  ((results, follow) => 
  		    results._2 === follow.objectFollowedID)} yield (results, follow.objectFollowedID?)
        
  		  //We then make the results presentables, we jerk out the ones the user already follows and the user himself
  		  val endResultsPar = explicitInnerJoin.list.par.filter(x => (x._1._2 == None || x._1._2.get != userID.toLong) && x._2 == None)
  		  
  		  Ok(Json.toJson(endResultsPar.map(x => convertMembersMailsEntryToClass(x._1._1).copy(optionalUserId = if (x._1._2 == None){-1}else{x._1._2.get}, optionalUsername = if (x._1._3 == None){""}else{x._1._3.get})).seq))
             
      }
  }
  
  def storeMembersMails()= Connection.withConnection{
    username => userID => implicit request => lang =>
      Instantiations.Tables.db.withSession { implicit session =>
        models.Forms.membersMailsForm.bindFromRequest.value map { membersMailsForm =>
          val mails: Seq[String] = membersMailsForm.mails.head.split(",")
          mails.par.map(mail => {
            try{
              Instantiations.Tables.membersFriendsMails += (userID.toLong, mail, 0, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())))
            } catch {
              case mysqlexception: Exception => 
                //Mail already sent
            }
          })
        }
        Ok
      }
  }
  
  def sendMemberEmails()= Connection.withConnection{
    username => userID => implicit request => implicit lang =>
      
      Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      { 
          models.Forms.membersMailsForm.bindFromRequest.value map { membersMailsForm =>
            val mails: Seq[String] = membersMailsForm.mails.head.split(",")
            
            val yourAWSCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
            val amazonS3Client = new AmazonS3Client(yourAWSCredentials)
            
            //We compute the name of the member in the title
            var firstnameTitle = ""
            val firstname = (for {c <- Instantiations.Tables.membersInfo if c.id === userID.toLong} yield c.firstname).list.head
            if (firstname == "")
              firstnameTitle = username
            else
              firstnameTitle = firstname + " (" + username + ')'
            
            mails.par.foreach(mail =>
              ///TODO : ne pas oublier de checker la photo avant d'envoyer le mail
              if (Instantiations.Tables.membersFriendsMails.filter(x => x.id === userID.toLong && x.friendEmail === mail).exists.run){
                //update sentmail 
                
                Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                            val sentMailQuery = (for {c <- Instantiations.Tables.membersFriendsMails if c.id === userID.toLong && c.friendEmail === mail} yield c.sentMail)
                            sentMailQuery.update(1) //We set LOGICALDELETE to 1
                            val statement = sentMailQuery.updateStatement
                            val invoker = sentMailQuery.updateInvoker
                }
                
                Mailer.sendEmail("", mail, firstnameTitle + ' ' + Messages("mail.titleInviteFriendsMail"), views.html.mails.inviteFriend.render(userID, username, firstname, amazonS3Client.doesObjectExist(AWS_S3_BUCKET, username + ".jpg"), Instantiations.getLang(request)).body) 
    	          println("Mail sent to " + mail)
              }
            )
          }
        }
      }
      Ok
  }
  
  def storePageLoadedByUser(usedID: Long, userIP: String, pageLoaded: String) = 
    Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
        Instantiations.Tables.membersPagesLoaded += (usedID, userIP, pageLoaded, Option(new java.sql.Timestamp(System.currentTimeMillis())))
      }
    }
    
}
