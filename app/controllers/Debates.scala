package controllers

import models.{DebateYesOrNot, DebateOpinion}
import play.api._
import play.api.mvc._
import play.api.i18n._
import play.api.i18n.I18nSupport
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.functional.syntax._
import slick.driver.MySQLDriver.simple._
import slick.jdbc.{GetResult, StaticQuery => Q}
import slick.jdbc.JdbcBackend.Database
import Q.interpolation
import org.jsoup._
import org.jsoup.safety.Whitelist

import play.api.Play.current
import play.api.cache.Cache;

object Debates extends Controller {
  //Twitter parameters
  val consumerKeyDebateOpinions = ""
  val consumerSecretDebateOpinions = ""
  val accessTokenDebateOpinions = ""
  val accessSecretDebateOpinions = ""
  
  val consumerKeyDebateYesOrNo = ""
  val consumerSecretDebateYesOrNo = ""
  val accessTokenDebateYesOrNo = "-"
  val accessSecretDebateYesOrNo = ""
  
  //debatocracy
  val consumerKeyDebateOpinionsEN = ""
  val consumerSecretDebateOpinionsEN = ""
  val accessTokenDebateOpinionsEN = ""
  val accessSecretDebateOpinionsEN = ""
  
  val consumerKeyDebateYesOrNoEN = ""
  val consumerSecretDebateYesOrNoEN = ""
  val accessTokenDebateYesOrNoEN = ""
  val accessSecretDebateYesOrNoEN = ""
  
  //debatesdopinion
  val consumerKeyDebateOpinionsES = ""
  val consumerSecretDebateOpinionsES = ""
  val accessTokenDebateOpinionsES = ""
  val accessSecretDebateOpinionsES = ""
  //debatesconvin
  val consumerKeyDebateYesOrNoES = ""
  val consumerSecretDebateYesOrNoES = ""
  val accessTokenDebateYesOrNoES = ""
  val accessSecretDebateYesOrNoES = ""
  
  /*
   * The JSON model of a debate
   */
  val debateWrites: Writes[models.Debate] = {
    ((JsPath \ "id").write[Long] and
    (JsPath \ "idcreator").write[Long] and
    (JsPath \ "question").write[String] and
    (JsPath \ "typeOfDebate").write[Int] and
    (JsPath \ "numberOfParticipations").write[Long] and
    (JsPath \ "language").write[String] and
    (JsPath \ "privateGroupId").write[Long] and
    (JsPath \ "isUserFollowing").write[Int] and
    (JsPath \ "modificationTimestamp").write[Long])(unlift(models.Debate.unapply))
  }

  /*
   * The JSON model of an opinion
   */

  val debateOpinionWrites: Writes[models.DebateOpinion] = {
    //case class DebateOpinion(id: Int, idDebate: Int, comment:String, idMember: Int, username: String, votefor: Int, voteagainst: Int, modificationTimestamp: java.sql.Timestamp, creationTimestamp: java.sql.Timestamp) extends EntryClass
    (
    (JsPath \ "id").write[Long] and
    (JsPath \ "idDebate").write[Long] and
    (JsPath \ "debateQuestion").write[String] and
    (JsPath \ "comment").write[String] and
    (JsPath \ "idMember").write[Long] and
    (JsPath \ "username").write[String] and
    (JsPath \ "votefor").write[Int] and
    (JsPath \ "voteagainst").write[Int] and
    (JsPath \ "numberofyesornoopinions").write[Int] and
    (JsPath \ "popularity").write[Long] and
    (JsPath \ "modificationTimestamp").write[java.sql.Timestamp] and
    (JsPath \ "creationTimestamp").write[java.sql.Timestamp]
    )(unlift(models.DebateOpinion.unapply))
  }

  /*
   * The JSON model of a YES OR NO opinion
   */

  val debateYesOrNotWrites: Writes[models.DebateYesOrNot] = {
    //case class DebateYesOrNot(idDebate: Int, comment:String, idMember: Int, opinion: Int, votefor: Int, voteagainst: Int, modificationTimestamp: java.sql.Timestamp, creationTimestamp: java.sql.Timestamp) extends EntryClass
    (
    (JsPath \ "id").write[Long] and
    (JsPath \ "idDebate").write[Long] and
    (JsPath \ "comment").write[String] and
    (JsPath \ "idMember").write[Long] and
    (JsPath \ "username").write[String] and
    (JsPath \ "opinion").write[Int] and
    (JsPath \ "votefor").write[Int] and
    (JsPath \ "voteagainst").write[Int] and
    (JsPath \ "popularity").write[Long] and
    (JsPath \ "modificationTimestamp").write[java.sql.Timestamp] and
    (JsPath \ "creationTimestamp").write[java.sql.Timestamp]
    )(unlift(models.DebateYesOrNot.unapply))
  }

  /*
   * Converts a debate entry from database to a models.Debate
   */
  def convertDebateDatabaseEntryToClass(databaseEntry: models.Types.debateEntry) = {
    models.Debate(databaseEntry._1.get, databaseEntry._2, Application.outputHTMLText(databaseEntry._3, true), databaseEntry._4, databaseEntry._6, databaseEntry._10, databaseEntry._5, 0, databaseEntry._7.getTime())
  }

  /*
   * Converts a debateOpinion entry from database to a models.DebateOpinion
   */
  def convertDebateOpinionDatabaseEntryToClass(databaseEntry: models.Types.debateOpinionEntry) = {
    /* TYPES OF THE OBJECTS : 
      * 0 MEMBER
      * 1 GROUP
      * 2 CONCEPT
      * 3 DEBATE
      * 4 ACTION
      * 5 PETITION
      * 6 YESORNOOPINION
      * 7 OPINION
      * 8 COMMENT
    */
    ActionRepport.addView(7, databaseEntry._1.get)
    models.DebateOpinion(databaseEntry._1.get, databaseEntry._2, "", Application.outputHTMLText(databaseEntry._3, true), databaseEntry._4, controllers.User.gettUsercameFromID(databaseEntry._4), databaseEntry._5, databaseEntry._6, databaseEntry._7, databaseEntry._9, databaseEntry._12.get, databaseEntry._12.get)
  }

  /*
   * Converts a debateYesOrNot entry from database to a models.DebateYesOrNo
   */
  def convertDebateYesOrNotDatabaseEntryToClass(databaseEntry: models.Types.debateYesOrNoEntry) = {
    /* TYPES OF THE OBJECTS : 
      * 0 MEMBER
      * 1 GROUP
      * 2 CONCEPT
      * 3 DEBATE
      * 4 ACTION
      * 5 PETITION
      * 6 YESORNOOPINION
      * 7 OPINION
      * 8 COMMENT
    */
    ActionRepport.addView(6, databaseEntry._1.get)
    models.DebateYesOrNot(databaseEntry._1.get, databaseEntry._2, Application.outputHTMLText(databaseEntry._3, true), databaseEntry._4, controllers.User.gettUsercameFromID(databaseEntry._4), databaseEntry._5, databaseEntry._6, databaseEntry._7, databaseEntry._9, databaseEntry._12.get, databaseEntry._12.get)
  }

  def postDebateOpinionOnTwitter(language: String, debateId: Long, debateTitle: String) = {
    language match {
    case "fr" => Application.postOnTwitter(debateId, 
        debateTitle, 
        "/debate", 
        consumerKeyDebateOpinions, 
        consumerSecretDebateOpinions, 
        accessTokenDebateOpinions, 
        accessSecretDebateOpinions)
     case "es" => Application.postOnTwitter(debateId, 
        debateTitle, 
        "/debate", 
        consumerKeyDebateOpinionsES, 
        consumerSecretDebateOpinionsES, 
        accessTokenDebateOpinionsES, 
        accessSecretDebateOpinionsES)
     case "en" => Application.postOnTwitter(debateId, 
        debateTitle, 
        "/debate", 
        consumerKeyDebateOpinionsEN, 
        consumerSecretDebateOpinionsEN, 
        accessTokenDebateOpinionsEN, 
        accessSecretDebateOpinionsEN)
    }
  }
  
  def postDebateYesOrNoOnTwitter(language: String, debateId: Long, debateTitle: String) = {
    language match {
    case "fr" => Application.postOnTwitter(debateId, 
        debateTitle, 
        "/debate", 
        consumerKeyDebateYesOrNo, 
        consumerSecretDebateYesOrNo, 
        accessTokenDebateYesOrNo, 
        accessSecretDebateYesOrNo)
    case "es" => Application.postOnTwitter(debateId, 
        debateTitle, 
        "/debate", 
        consumerKeyDebateYesOrNoES, 
        consumerSecretDebateYesOrNoES, 
        accessTokenDebateYesOrNoES, 
        accessSecretDebateYesOrNoES)
    case "en" => Application.postOnTwitter(debateId, 
        debateTitle, 
        "/debate", 
        consumerKeyDebateYesOrNoEN, 
        consumerSecretDebateYesOrNoEN, 
        accessTokenDebateYesOrNoEN, 
        accessSecretDebateYesOrNoEN)
    }
  }
  
  /*
   * Creates a debate when a user uses the Forms.debateCreationForm
   */
	def processDebateCreationForm = Connection.withConnection({
        username => userID => implicit request => implicit lang =>

          models.Forms.debateCreationForm.bindFromRequest.value map { debateCreationForm =>
            Instantiations.Tables.db.withSession { implicit session =>
              val debateTypeInt = debateCreationForm.debateType.toLong
              val q2 = for {c <- Instantiations.Tables.debate if (c.question === debateCreationForm.question)} yield c.id
              val reservedToGroup = debateCreationForm.reservedToGroup.toLong
              val userIDInt = userID.toLong
              
              if (q2.list == Nil
                  &&
                  debateCreationForm.question.length < Application.maxCharacternames
                  &&
                  Application.langs.contains(debateCreationForm.language)
                  &&
                  debateTypeInt >= 0 && debateTypeInt <= 1){
                var debateCreated = false
                if (reservedToGroup == -1){ //public debate

                  //We check if there is not a debate with the same question and if the debate type is correct
                  val tuppleToAdd = ((None, userIDInt , debateCreationForm.question, debateCreationForm.debateType.toInt, -1l, 0l, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0, debateCreationForm.language))
                  Instantiations.Tables.debate += tuppleToAdd
                  debateCreated = true
                }
                else{ //Reserved to members of group

                  //We check that this user can create private debates for this group
                  if (Groups.canThisUserCreateGroupPrivateDebates(userIDInt, reservedToGroup)){
                    val tuppleToAdd = ((None, userIDInt , scala.xml.Utility.escape(debateCreationForm.question), debateCreationForm.debateType.toInt, reservedToGroup, 0l, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0, request.session.get("language").get))
                    Instantiations.Tables.debate += tuppleToAdd
                    debateCreated = true
                  }
                }

                if (debateCreated){
                  //We execute q2 another time and this will give us the correct id
                  val idDebate = q2.list.head
                  reservedToGroup match {
                    case -1 => ActionRepport.actionDone(userIDInt, 3, idDebate, idDebate, 30000, null)
                    case _ => ActionRepport.actionDone(userIDInt, 1, reservedToGroup, idDebate, 30010, null, reservedToGroup)
                  }
                  if (reservedToGroup == -1 || (Groups.getGroupModel(reservedToGroup).privacy == 0)){
                    debateTypeInt match {
                      case 0 => postDebateYesOrNoOnTwitter(debateCreationForm.language, idDebate, scala.xml.Utility.escape(debateCreationForm.question))
                      case 1 => postDebateOpinionOnTwitter(debateCreationForm.language, idDebate, scala.xml.Utility.escape(debateCreationForm.question))
                    }
                  }
                  
                  //We follow this debate
                  User.followThisObject(userIDInt, 3, idDebate)
                  
                  Ok(Json.toJson(idDebate))
                }
                else{
                  BadRequest
                }
              }
              else{
                //Ok(views.html.connected.debate(Messages("views.debate_error")))
                Ok(Json.toJson(-1))
              }
            }
          } getOrElse {
            BadRequest
          }
      }, None, true)

      
  def updateDebateTimestamp(debateID: Long) = {
    Instantiations.Tables.db.withSession { implicit session =>
      Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      { 
    	    sqlu"update DEBATE set NUMBEROFPARTICIPATIONS=NUMBEROFPARTICIPATIONS-1 where SUP_ID = $debateID".first
          sqlu"update DEBATE set NUMBEROFPARTICIPATIONS=NUMBEROFPARTICIPATIONS+1 where SUP_ID = $debateID".first
        }
      }
    }
	}
      
	/*
	 * Posts a Yes or no opinion. If this is for a yes or no debate, idObjectUP is the opinionIcD if this is posted in a opinion debate
	 * Creates a entry in the debateYesOrNo database table from the debateYesOrNoForm
	 * Returns -1 if failed, 1 if sucessfully posted in a yes or no debate, and 2 if successfully posted on an opinion debate
	 */
	def processDebateYesOrNoForm(debateID: Long, idObjectUP: Long) = Connection.withConnection({
        username => userID => implicit request => implicit lang =>
          var returnDebateYesOrNoPage = false
          var returnDebateOpinionPage = false
          models.Forms.debateYesOrNoForm.bindFromRequest.value map { debateYesOrNoForm =>
            Instantiations.Tables.db.withSession { implicit session =>
              val userIDInt = userID.toLong
              val opinionInt = Integer.parseInt(debateYesOrNoForm.opinion)

              //We first check if this is a yes or no debate, that the debate exists, and that the user is allowed to participate in it
              val q1 = for {c <- Instantiations.Tables.debate if c.id === debateID} yield c
              val q1list = q1.list
              if (
                  debateYesOrNoForm.comment.size < Application.maxCharactersOpinions
                  &&
                  q1list != Nil
                  &&
                  canThisUserParticipateToThisDebate(debateID, userID.toLong)){

                val debateFromDatabase = convertDebateDatabaseEntryToClass(q1list.head)
                
                lazy val commentWithHtmlFiltered = Jsoup.clean(debateYesOrNoForm.comment, Application.whiteListHTML)

                debateFromDatabase.typeOfDebate match{
                  case 0  =>
                    //This is a yes or no debate, the member has to have one yes or not opinion
                    val q2 = for {c <- Instantiations.Tables.debateYesOrNo  if (c.idMember === userIDInt && c.idDebate === debateID && c.logicalDelete === 0) } yield c.id

                    
	                if (opinionInt <= 1 && opinionInt >= 0 ){
	                  if (q2.list == Nil){
	                    Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
	                        sqlu"update DEBATE set NUMBEROFPARTICIPATIONS=NUMBEROFPARTICIPATIONS+1 where SUP_ID = $debateID".first
	                    }
	                    //The member never posted a opinion on this debate
	                    Instantiations.Tables.debateYesOrNo += ((None, debateID, commentWithHtmlFiltered, userIDInt, opinionInt, 0, 0, 0, 0l, debateID, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0))

	                    ActionRepport.actionDone(userIDInt, 3, debateID, q2.list.head, 30001, null, debateFromDatabase.privateGroupId)

	                    //We follow this debate
                      User.followThisObject(userIDInt, 3, debateID)

	                    //The information was correct, we return the debate page
	                    returnDebateYesOrNoPage = true

	                  }
	                }
                  case 1 =>
                    //This is an opinion debate, the member has to have one yes or not opinion per opinion
                    if (opinionInt <= 1 && opinionInt >= 0 ){
                      val q2 = for {c <- Instantiations.Tables.debateYesOrNo  if (c.idMember === userIDInt && c.idDebate === debateID && c.idObjectUP === idObjectUP && c.logicalDelete === 0) } yield c.id
 
                      if (q2.list == Nil){
                        //println("Added yes or not opinion to opinion " + idObjectUP)
                        Instantiations.Tables.debateYesOrNo += ((None, debateID, commentWithHtmlFiltered, userIDInt, opinionInt, 0, 0, 0, 0l, idObjectUP, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0))
	                      Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                            sqlu"update DEBATEOPINION set NUMBEROFYESORNO=NUMBEROFYESORNO+1 where SUP_ID = $idObjectUP".first
                        }
                        updateDebateTimestamp(debateID)
                        
                        //We make the user vote for the opinion
	                      voteForOpinionSub(debateID, userIDInt, opinionInt, idObjectUP)
	                      
                        ActionRepport.actionDone(userIDInt, 6, idObjectUP, q2.list.head, 30020, null, debateFromDatabase.privateGroupId)
                        
                        //We follow this debate
                        User.followThisObject(userIDInt, 3, debateID)
                        
                        //The information was correct, we return the debate page
	                      returnDebateOpinionPage = true
                      }
                    }
                  case _ =>
                }
                
                
                
              }
            }
          }

          if (returnDebateYesOrNoPage){
            //val debateFromDataBase = getDebateObject(debateID)
            //Ok(views.html.connected.debates.yesorno("", debateFromDataBase))
            Ok(Json.toJson(1))
          }
          else if (returnDebateOpinionPage){
            //val debateFromDataBase = getDebateObject(debateID)
            //Ok(views.html.connected.debates.opinion("", debateFromDataBase))
            Ok(Json.toJson(2))
          }
          else{
            Ok(Json.toJson(-1))
          }
    }, None, true)

	def canThisUserParticipateToThisYesOrNoOpinion(yesOrNoId: Long, userID: Long): Boolean = {
	  Instantiations.Tables.db.withSession { implicit session =>
  	  val debateAssociatedRequest = for { c <- Instantiations.Tables.debateYesOrNo if (c.id === yesOrNoId) } yield c.idDebate
  	  val idDebateList = debateAssociatedRequest.list
  	  if (idDebateList != Nil){
  	    canThisUserParticipateToThisDebate(idDebateList.head, userID)
  	  }
  	  else{
  	    false
  	  }
	  }
	}
	
	/*
   * Deletes a yes or no opinion defined by the "key" POSTDATA. Returns 1 if the comment has been deleted, and 0 otherwise
   */
  def processDebateYesOrNoDelete  = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
    var jsonToSend = 0
    val userIDLong = userID.toLong
    val args = request.body.asFormUrlEncoded.get
    if (
       args != None 
         && 
       args.contains("key") 
       )
    {
      Instantiations.Tables.db.withSession { implicit session =>
        
        //First we verify that this yes or no opinion exists and that it is the user that posted if
        val debateYesOrNo = for { c <- Instantiations.Tables.debateYesOrNo if (c.id === args.get("key").get(0).toLong && c.idMember === userIDLong) } yield c
        val debateYesOrNoList = debateYesOrNo.list
        if (debateYesOrNoList != Nil){
          Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
            val q2 = for { c <- Instantiations.Tables.debateYesOrNo if (c.id === args.get("key").get(0).toLong && c.idMember === userIDLong) } yield c.logicalDelete
            q2.update(1) //We set LOGICALDELETE to 1
            val statement = q2.updateStatement
            val invoker = q2.updateInvoker
            //If the yes or no opinion was on an opinion, decrease the number of yesorno opinions associated
            //First we find the id of the Opinion associated with this yes or no opinion
            val opinionAssociated = for {(debateOpinion, debateYesOrNo) <- Instantiations.Tables.debateOpinion innerJoin debateYesOrNo on ((debateOpinion, debateYesOrNo) => debateOpinion.id === debateYesOrNo.idObjectUP && debateOpinion.idDebate === debateYesOrNo.idDebate)} yield debateOpinion.id
            val opinionAssociatedList = opinionAssociated.list
            //Then if this opinion exists, we decrease its numberofyesorno param by 1
            if (opinionAssociatedList != Nil){
              val idOpinionAssociated = opinionAssociatedList.head
              sqlu"update DEBATEOPINION set NUMBEROFYESORNO=NUMBEROFYESORNO-1 where SUP_ID = $idOpinionAssociated".first
            }
            else{ //If not, that means that this is a yes or no opinion on a yes or no debate
               if (debateYesOrNoList != Nil){
                val idDebate = debateYesOrNoList.head._2
                sqlu"update DEBATE set NUMBEROFPARTICIPATIONS=NUMBEROFPARTICIPATIONS-1 where SUP_ID = $idDebate".first
              }
            }
            
            //We delete every activity associated with that yes or no opinion
            //We delete the DebateYesOrNoParticipation
            val q5 = for { c <- Instantiations.Tables.activity if (c.activityType === 30001 && c.memberId === userIDLong && c.activityTarget === args.get("key").get(0).toLong) } yield c.logicalDelete
            q5.update(1) //We set LOGICALDELETE to 1
            val statement5 = q5.updateStatement
            val invoker5 = q5.updateInvoker
            //We delete the postyesornoopinion of this member
            val q3 = for { c <- Instantiations.Tables.activity if (c.activityType === 30020 && c.memberId === userIDLong && c.activityTarget === args.get("key").get(0).toLong) } yield c.logicalDelete
            q3.update(1) //We set LOGICALDELETE to 1
            val statement3 = q3.updateStatement
            val invoker3 = q3.updateInvoker
            //We delete the votefor yes or no opinion and the voteagainst yes or no opinion and the ModifVoteForToAgainstYesOrNoOpinion and the ModifVoteAgainstToForYesOrNoOpinion
            val q4 = for { c <- Instantiations.Tables.activity if ((c.activityType === 30002 || c.activityType === 30003 || c.activityType === 30004 || c.activityType === 30005) && c.activityTarget === args.get("key").get(0).toLong) } yield c.logicalDelete
            q4.update(1) //We set LOGICALDELETE to 1
            val statement4 = q4.updateStatement
            val invoker4 = q4.updateInvoker
            
            //Delete CommentAction
            val q6 = for { c <- Instantiations.Tables.activity if (c.activityType === 60004 && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
            q6.update(1) //We set LOGICALDELETE to 1
            val statement6 = q6.updateStatement
            val invoker6 = q6.updateInvoker
            
            //We delete from cache 
            Cache.remove("debateYesOrNo" + args.get("key").get(0).toLong)
        }
          jsonToSend = 1
        }
      }
    }
    Ok(Json.toJson(jsonToSend))
  }, None, true)


	/*
	 * Posts an Opinion on a debate. Returns 1 if success and 0 if not
	 */
	def processDebateOpinionForm(debateID: Long) = Connection.withConnection({
	  username => userID => implicit request => implicit lang =>
          var returnDebatePage = false
          models.Forms.debateOpinionForm.bindFromRequest.value map { debateOpinionForm =>
            Instantiations.Tables.db.withSession { implicit session =>
              val userIDInt = userID.toLong
              //We first check if this is an opinion debate, that the debate exists, and that the user is allowed to participate in it
              val q1 = for {c <- Instantiations.Tables.debate if c.id === debateID && c.logicalDelete === 0} yield c
              val q2 = for {c <- Instantiations.Tables.debateOpinion  if (c.idMember === userIDInt && c.idDebate === debateID && c.logicalDelete === 0) } yield c.id
              
              val debateEntry = q1.list
              if (debateEntry != Nil){
                val debateFromDatabase = convertDebateDatabaseEntryToClass(debateEntry.head)
                if (
                    debateOpinionForm.comment.size < Application.maxCharactersOpinions
                    &&
                    debateFromDatabase.typeOfDebate == 1
                    &&
                    canThisUserParticipateToThisDebate(debateID, userID.toLong)
                    &&
                    q2.list == Nil
                   ){
                    Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                      { 
                        sqlu"update DEBATE set NUMBEROFPARTICIPATIONS=NUMBEROFPARTICIPATIONS+1 where SUP_ID = $debateID".first
                      }
                    }
                    Instantiations.Tables.debateOpinion += ((None, debateID, Jsoup.clean(debateOpinionForm.comment, Application.whiteListHTML), userIDInt, 0, 0, 0, 0, 0l, debateID, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0))
                    ActionRepport.actionDone(userIDInt, 3, debateID, q2.list.head, 30015, null, debateFromDatabase.privateGroupId)
                    
                    //We follow this debate
                    User.followThisObject(userIDInt, 3, debateID)
                    
                    //The information was correct, we return the debate page
                    returnDebatePage = true
                  }
              }
            }
          }
           if (returnDebatePage){
           // val debateFromDataBase = getDebateObject(debateID)

            //println("new opinion : ")
            //Ok(views.html.connected.debates.opinion("", debateFromDataBase))
            Ok(Json.toJson(1))
          }
          else{
           // println("not new opinion")
            Ok(Json.toJson(0))
          }
	}, None, true)

	/*
   * Deletes a opinion defined by the "key" POSTDATA. Returns 1 if the comment has been deleted, and 0 otherwise
   */
  def processDebateOpinionDelete  = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
    var jsonToSend = 0
    val userIDLong = userID.toLong
    val args = request.body.asFormUrlEncoded.get
    if (
       args != None 
         && 
       args.contains("key") 
       )
    {
      Instantiations.Tables.db.withSession { implicit session =>
        //First we verify that this yes or no opinion exists and that it is the user that posted if
        val debateOpinion = for { c <- Instantiations.Tables.debateOpinion if (c.id === args.get("key").get(0).toLong && c.idMember === userIDLong) } yield c.idDebate
        val idOfDebateList = debateOpinion.list
        if (idOfDebateList != Nil){
          Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
            Instantiations.Tables.db.withSession { implicit session =>
            val q2 = for { c <- Instantiations.Tables.debateOpinion if (c.id === args.get("key").get(0).toLong && c.idMember === userIDLong) } yield c.logicalDelete
            q2.update(1) //We set LOGICALDELETE to 1
            val statement = q2.updateStatement
            val invoker = q2.updateInvoker
            
            //We then decrease the number of participations of the debate of one
            val idDebate = idOfDebateList.head
            sqlu"update DEBATE set NUMBEROFPARTICIPATIONS=NUMBEROFPARTICIPATIONS-1 where SUP_ID = $idDebate".first
            
            //We then delete all activities associated with this opinion
            //We delete the DebateOpinionParticipation
            val q5 = for { c <- Instantiations.Tables.activity if (c.activityType === 30015 && c.memberId === userIDLong && c.activityTarget === args.get("key").get(0).toLong) } yield c.logicalDelete
            q5.update(1) //We set LOGICALDELETE to 1
            val statement5 = q5.updateStatement
            val invoker5 = q5.updateInvoker
            //We delete the postyesornoopinion associated with this opinion of all members
            val q3 = for { c <- Instantiations.Tables.activity if (c.activityType === 30020 && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
            q3.update(1) //We set LOGICALDELETE to 1
            val statement3 = q3.updateStatement
            val invoker3 = q3.updateInvoker
            //We delete the votefor yes or no opinion and the voteagainst yes or no opinion and the ModifVoteForToAgainstYesOrNoOpinion and the ModifVoteAgainstToForYesOrNoOpinion
            val q4 = for { c <- Instantiations.Tables.activity if ((c.activityType === 30016 || c.activityType === 30017 || c.activityType === 30018 || c.activityType === 30019) && c.activityTarget === args.get("key").get(0).toLong) } yield c.logicalDelete
            val statement4 = q4.updateStatement
            val invoker4 = q4.updateInvoker
            
            //We delete from cache 
            Cache.remove("debateOpinion" + args.get("key").get(0).toLong)
          }
          }
          jsonToSend = 1
        }
      }
    }
    Ok(Json.toJson(jsonToSend))
  }, None, true)
	

	/*
	 * Returns if this user can participate to the debate
	 */
	def canThisUserParticipateToThisDebate(debateID: Long, userID: Long): Boolean = {
	  Instantiations.Tables.db.withSession { implicit session =>
	    //We check if the user is in the correct group, or if the privacy settings
   	    val q2 = for {c <- Instantiations.Tables.debate.filter(x => x.id === debateID && x.logicalDelete === 0)} yield c.privateGroupId
        val result = q2.list
        //println("User status in group : " + Groups.userStatusInGroup(result.head, userID))
        if (result == Nil){//Debate doesnt exist
          false
        }
        else if (result.head == -1){ //public debate
          true
        }
        else{//Group debate
          val userStatusInGroup = Groups.userStatusInGroup(result.head, userID, true) //The user can access if it is a public group (3) or if he is in the group (1)
          if (userStatusInGroup == 1 || userStatusInGroup == 3){
            true
          }
          else{
            false
          }
        }
	  }
	}

	/*
	 * Returns the json of the last "numberOfDebates" debates
	 * Param groupPrivacy is -1 in order to get public debates, and the id of the group in order to see debates reserved to a group
	 */
    def getLastDebatesOrderedbyTimestampJSON(groupPrivacy: Long, maxTimestamp: Long, number: Int) = Connection.withConnection({
      username => userID => implicit request => implicit lang => {
        val userIDLong = userID.toLong
        val userStatusInGroup = Groups.userStatusInGroup(groupPrivacy, userID.toLong, true)
          if (
            //We first check that the user is allowed to see these kinds of debates
            (groupPrivacy == -1 || userStatusInGroup == 1 || userStatusInGroup == 3)
            &&
            //Then we check that the user asked a correct number of results
            (number <= controllers.Application.maxResults && number >= 0)
          ){
            getLastDebatesOrderedbyTimestampJSONsub(userIDLong, groupPrivacy, maxTimestamp, number)
          }
          else{
            BadRequest
          }
      }
    }, Option((request: Request[AnyContent]) => {
      val statusInGroup = Groups.userStatusInGroup(groupPrivacy, -1, true)
      
      if (groupPrivacy != -1 && (statusInGroup == 1 || statusInGroup == 3)){ //only for publics groups
        getLastDebatesOrderedbyTimestampJSONsub(-1, groupPrivacy, maxTimestamp, number)
      }
      else if (groupPrivacy == -1){//List of public debates for non connected people
        getLastDebatesOrderedbyTimestampJSONsub(-1, -1, maxTimestamp, number, Some(Instantiations.getLangText(request)))
      }
      else{
        BadRequest
      }
    }))
   def getLastDebatesOrderedbyTimestampJSONsub(userIDLong: Long, groupPrivacy: Long, maxTimestamp: Long, number: Int, onlyShowLanguage: Option[String] = None) = {
     Instantiations.Tables.db.withSession { implicit session =>
          val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
            val debatesFiltered = for {c <- Instantiations.Tables.debate.filter(x => x.modificationTimestamp < maxTimestampParam && x.logicalDelete === 0)} yield c
            
            var debatesOfWantedGroupPrivacy = debatesFiltered.filter(_.privateGroupId === groupPrivacy)
            
            if (groupPrivacy == -1){ //Public list of all debates
              
              if (userIDLong != -1){//We check for private groups debates only if the user is connected
                //We have to add all the debates of private groups where the user is member
                val debatesOfUserGroup = for { //All the debates of the groups where user is member
                  (groupMember, debatesFiltered) <- Instantiations.Tables.groupMembership innerJoin debatesFiltered on ((groupMember, debatesFiltered) => 
                    (groupMember.memberId === userIDLong && groupMember.groupId === debatesFiltered.privateGroupId))
                } yield (debatesFiltered, groupMember.groupId)
              
                val debatesOfUserPrivateGroups = for { //We only take the debates of the private groups, because the public ones are going to be added later (so we dont display twice the same debate)
                  (group, debatesOfUserGroup) <- Instantiations.Tables.groups.filter(_.privacy === 1) innerJoin debatesOfUserGroup on ((group, debatesOfUserGroup) =>
                    group.id === debatesOfUserGroup._2
                  ) 
                } yield (debatesOfUserGroup._1)
                debatesOfWantedGroupPrivacy = debatesOfWantedGroupPrivacy ++ debatesOfUserPrivateGroups
              }
              //We add also the debates of public groups
              val debatesOfPublicGroups= for {
                (group, debatesFiltered) <- Instantiations.Tables.groups innerJoin debatesFiltered on ((group, debatesFiltered) => 
                  (group.privacy === 0 && group.id === debatesFiltered.privateGroupId))
              } yield debatesFiltered
              debatesOfWantedGroupPrivacy = debatesOfWantedGroupPrivacy ++ debatesOfPublicGroups
            }
            
            
            
            var onlyShowMemberLanguagesInRequest = {
              if (groupPrivacy == -1 && userIDLong != -1) { 
                  for {(request, memberLanguage) <- debatesOfWantedGroupPrivacy innerJoin User.requestForUserLanguages(userIDLong) on ((request, memberLanguage) => request.language === memberLanguage.language)} yield request
              } 
              else if (onlyShowLanguage != None)
                debatesOfWantedGroupPrivacy.filter(_.language === onlyShowLanguage.get)
              else {
                debatesOfWantedGroupPrivacy
              }
            }
            
            //We search if the member is following the debates. We rightjoin so that first part of tuple is None if user doesn't follow, and Some(Iduser) if he follows
            val debatesAndFollows = for {
                (follow, debatesFiltered)  <- Instantiations.Tables.follow.filter(x => x.memberId === userIDLong && x.objectFollowedType === 3) rightJoin onlyShowMemberLanguagesInRequest on ((follow, debatesFiltered) => 
                  (follow.objectFollowedID === debatesFiltered.id))
                } yield (follow.memberId?, debatesFiltered)
            
            val results = debatesAndFollows.sortBy(_._2.modificationTimestamp.desc).take(number).list.par.map(x => convertDebateDatabaseEntryToClass(x._2).copy(isUserFollowing = {if (x._1 == None){0}else{1}})).seq
            implicit val wrtier = debateWrites
            //println(results)//TODO : the JSON
            Ok(Json.toJson(results))
          }
   }
   
    /*
   * Retrieves the last 10 actions created by the user from a certain id
   */
  def getLastDebatesCreatedByUserOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val q2 = for {c <- Instantiations.Tables.debate.filter(x => (x.idCreator === userIDInt && x.modificationTimestamp < maxTimestampParam && x.logicalDelete === 0)).sortBy(_.modificationTimestamp.desc).take(number)} yield c
          
          //We search if the member is following the debates. We rightjoin so that first part of tuple is None if user doesn't follow, and Some(Iduser) if he follows
            val debatesAndFollows = for {
                (follow, debatesFiltered)  <- Instantiations.Tables.follow.filter(x => x.memberId === userIDInt && x.objectFollowedType === 3) rightJoin q2 on ((follow, debatesFiltered) => 
                  (follow.objectFollowedID === debatesFiltered.id))
                } yield (follow.memberId?, debatesFiltered)
          
          val results = debatesAndFollows.list.par.map(x => convertDebateDatabaseEntryToClass(x._2).copy(isUserFollowing = {if (x._1 == None){0}else{1}})).seq
          implicit val wrtier = debateWrites
          Ok(Json.toJson(results))
        }
        else {
          BadRequest
        }
      }
    }
  }


    /*
     * Returns the json of the opinions on a debate sorted by posted date
     */
    def getLastOpinionsOrderedbyTimestampJSON(debateID: Long, maxTimestamp: Long, number: Int) = Connection.withConnection (
      username => userID => implicit request => implicit lang => {
        getLastOpinionsOrderedbyTimestampJSONsub(debateID, maxTimestamp, number, username, userID.toLong, request)
      }
    , Option((request: Request[AnyContent]) => getLastOpinionsOrderedbyTimestampJSONsub(debateID, maxTimestamp, number, "", -1, request)))

    def getLastOpinionsOrderedbyTimestampJSONsub(debateID: Long, maxTimestamp: Long,number: Int, username: String, userID: Long, request: Request[AnyContent]) = {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        //We check that the number of debates asked is correct and that the user is allowed to participate in this debate (in order to see results)
        if (number <= controllers.Application.maxResults && number >= 0 && canThisUserParticipateToThisDebate(debateID, userID.toLong)){
          val q2 = Instantiations.Tables.debateOpinion.filter(x => x.idDebate === debateID && x.modificationTimestamp < maxTimestampParam && x.logicalDelete === 0).sortBy(_.modificationTimestamp.desc).take(number)
          val results = q2.list.map(x => convertDebateOpinionDatabaseEntryToClass(x))
          implicit val wrtier = debateOpinionWrites
          Ok(Json.toJson(results))
        }
        else{
          BadRequest
        }
      }
    }
    
     /*
     * Returns the json of the opinions on a debate sorted by posted date
     */
    def getLastOpinionsOrderedbyPopularityJSON(debateID: Long, maxPopularity: Long, number: Int) = Connection.withConnection (
      username => userID => implicit request => implicit lang => {
        getLastOpinionsOrderedbyPopularityJSONsub(debateID, maxPopularity, number, username, userID.toLong, request)
      }
    , Option((request: Request[AnyContent]) => getLastOpinionsOrderedbyPopularityJSONsub(debateID, maxPopularity, number, "", -1, request)))

    //With Randomness for the 3rd opinion
    def getLastOpinionsOrderedbyPopularityJSONsub(debateID: Long, maxPopularity: Long, number: Int, username: String, userID: Long, request: Request[AnyContent]) = {
      Instantiations.Tables.db.withSession { implicit session =>
        //We check that the number of debates asked is correct and that the user is allowed to participate in this debate (in order to see results)
        //println("CALLED with maxPop " + maxPopularity + "number " + number)
        if (number <= controllers.Application.maxResults && number >= 0 && canThisUserParticipateToThisDebate(debateID, userID.toLong)){

          var numRandom = number/3
          if (userID == -1){
            numRandom = 0
          }
          val q2 = Instantiations.Tables.debateOpinion.filter(x => x.idDebate === debateID && x.popularityTimestamp < maxPopularity && x.logicalDelete === 0).sortBy(_.popularityTimestamp.desc).take(number-numRandom)
          var results = q2.list.map(x => convertDebateOpinionDatabaseEntryToClass(x))

          if (userID != -1){
            

            //This is supposed to favorise opinions that are low otherwise. We do not want to see for ex the first opinion downgraded as a week random opinion.


            //All randomopinions for a user/opinion combination have the same timestamp. If this ts is older than one hour, drop them all and redo them.
            // TODO For now just use one randomopinion, add possibility for any number
            val randomTimeStamp = Instantiations.Tables.debateRandomOpinion.filter(x => x.idDebate === debateID && x.idMember === userID && x.position === 3).firstOption
            //println("ZEBIII" + randomTimeStamp)
            val nowTimeStamp = new java.sql.Timestamp(System.currentTimeMillis())

            //Create Random opinions in the database table: For now just one
            def generateRandomOpinions() = {
              val rand = SimpleFunction.nullary[Double]("RAND")
              val randomStep1 = Instantiations.Tables.debateOpinion.filter(x => x.idDebate === debateID && x.popularityTimestamp < maxPopularity && x.logicalDelete === 0).sortBy(_.popularityTimestamp.desc).drop(Application.numOpinionsNotToRandomize).sortBy(x => rand).take(1)
              val randomStep1Results = randomStep1.list
              if (randomStep1Results.nonEmpty) {
                Instantiations.Tables.debateRandomOpinion += ((debateID, userID, 3, randomStep1Results.head._1.get, nowTimeStamp, Some(nowTimeStamp)))
              }
            }
            //No entry for this user/de ate combination exists
            if (randomTimeStamp.isEmpty) {
         //     print("Emtpy" + "userID " + userID)
              generateRandomOpinions()
            }
            //Entry has expired
            else if (nowTimeStamp.getTime - randomTimeStamp.get._5.getTime > Application.timeIntervalForReFresh) {
       //       print("Removing old")
              Instantiations.Tables.debateRandomOpinion.filter(x => x.idDebate === debateID && x.idMember === userID && x.position === 3).delete
              generateRandomOpinions()
            }
            else {
       //       print("nothing to do")
            }
            //Now we get the necessary random opinions: one for each 3rd opinion

            val randomOpinionList = (Instantiations.Tables.debateRandomOpinion.filter(x => x.idDebate === debateID && x.idMember === userID && x.position === 3 ).sortBy(_.position.asc) innerJoin Instantiations.Tables.debateOpinion on
              ((rand, alldebs) => rand.idOpinion === alldebs.id)).list.map(x => convertDebateOpinionDatabaseEntryToClass(x._2))

            if (randomOpinionList.nonEmpty) {
              //Enough opinions for randomness: fetch them
              val oneRandomOpinion = randomOpinionList.head
              results = results.take(2) ::: oneRandomOpinion :: results.filterNot(opinion => opinion.id == oneRandomOpinion.id).drop(2)

           //   println("Showing as random on third: " + randomOpinionList)

            }
          //  println("Opinions: " + results)

          }

          implicit val wrtier = debateOpinionWrites
          Ok(Json.toJson(results))
        }
        else{
          BadRequest
        }
      }
    }


  //Old implementation without any randomness
  def getLastOpinionsOrderedbyPopularityJSONsubNoRandom(debateID: Long, maxPopularity: Long, number: Int, username: String, userID: Long, request: Request[AnyContent]) = {
    Instantiations.Tables.db.withSession { implicit session =>
      //We check that the number of debates asked is correct and that the user is allowed to participate in this debate (in order to see results)
      if (number <= controllers.Application.maxResults && number >= 0 && canThisUserParticipateToThisDebate(debateID, userID.toLong)){

        val q2 = Instantiations.Tables.debateOpinion.filter(x => x.idDebate === debateID && x.popularityTimestamp < maxPopularity && x.logicalDelete === 0).sortBy(_.popularityTimestamp.desc).take(number)

        val results = q2.list.map(x => convertDebateOpinionDatabaseEntryToClass(x))
        implicit val wrtier = debateOpinionWrites
        Ok(Json.toJson(results))
      }
      else{
        BadRequest
      }
    }
  }
    
    /*
     * Returns the json of the debates where the user participated
     */
    def getLastDebatesParticipatedOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
      username => userID => implicit request => implicit lang => {
        val userIDInt = userID.toLong
        getLastDebatesPArticipatedOfMemberOrderedByTimestampJSONResult(userIDInt, maxTimestamp, number)
      }
    }
    
  def getLastDebatesPArticipatedOfMemberOrderedByTimestampJSON(userIDInt: Long, maxTimestamp: Long, number: Int) = Connection.withConnection {
      username => userID => implicit request => implicit lang => {
        getLastDebatesPArticipatedOfMemberOrderedByTimestampJSONResult(userIDInt, maxTimestamp, number)
      }
  }
  
  def getLastDebatesPArticipatedOfMemberOrderedByTimestampJSONResult(userIDInt: Long, maxTimestamp: Long, number: Int) =  {
       if (number <= controllers.Application.maxResults && number >= 0){
         ///TODO There may be a better way to do this, by asking with a big aggregated query.
          
         val results = getLastDebatesParticipatedList(userIDInt, maxTimestamp, number).map(x => convertDebateDatabaseEntryToClass(x._2._1).copy(modificationTimestamp = x._2._2.get.getTime(), isUserFollowing = {if (x._1 == None){0}else{1}}))

         //We search if the member is following the debates. We rightjoin so that first part of tuple is None if user doesn't follow, and Some(Iduser) if he follows

         implicit val wrtier = debateWrites
         Ok(Json.toJson(results))
       }
       else {
         BadRequest
       }
    }
    
    def getLastDebatesParticipatedList(userIDInt: Long, maxTimestamp: Long, number: Int): List[(Option[Long], (models.Types.debateEntry, Option[java.sql.Timestamp]))] = {
      
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
               val explicitInnerJoinDebateYesOrNo = for {
  		  (debateYesOrNo, debate) <- Instantiations.Tables.debateYesOrNo.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.debate on
  		  ((debateYesOrNo, debate) => debateYesOrNo.idMember === userIDInt && debateYesOrNo.idDebate === debate.id && debateYesOrNo.modificationTimestamp < maxTimestampParam && debateYesOrNo.logicalDelete === 0 && debate.logicalDelete === 0)
  		  } yield (debate, debateYesOrNo.modificationTimestamp)
  
  		  val explicitInnerJoinDebateOpinion = for {
  		  (debateOpinion, debate) <- Instantiations.Tables.debateOpinion.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.debate on
  		  ((debateOpinion, debate) => debateOpinion.idMember === userIDInt && debateOpinion.idDebate === debate.id && debateOpinion.modificationTimestamp < maxTimestampParam && debateOpinion.logicalDelete === 0  && debate.logicalDelete === 0)
  		  } yield (debate, debateOpinion.modificationTimestamp)
  
  		  val unionDebates = (explicitInnerJoinDebateOpinion ++ explicitInnerJoinDebateYesOrNo).groupBy(x=>x._1).map{
  		       case (debate, timestamps) => 
  		         (debate, timestamps.map(_._2).max)}
  		  //println(unionDebates.list)
  		  val debatesAndFollowsUnionDebates = for {
                (follow, debatesFiltered)  <- Instantiations.Tables.follow.filter(x => x.memberId === userIDInt && x.objectFollowedType === 3) rightJoin unionDebates on ((follow, debatesFiltered) => 
                  (follow.objectFollowedID === debatesFiltered._1.id))
                } yield (follow.memberId?, debatesFiltered)
  
         debatesAndFollowsUnionDebates.take(number).sortBy(_._2._2.desc).take(number).list
      }
    }

    /*
     * Returns the json of the yes or not opinions on yes or no debate sorted by posted date
     */

    def getLastYesOrNoOpinionsForYesOrNoDebateOrderedbyTimestampJSON(debateID: Long, opinion: Int, maxTimestamp: Long,number: Int) = {
      getLastYesOrNoOpinionsOrderedbyTimestampJSON(debateID: Long, -1l, opinion: Int, maxTimestamp: Long,number: Int)
    }

    /*
     * Returns the json of the yes or not opinions on opinion debate for an opinion defined by opinionID sorted by posted date
     * if opinionid is -1, get all yes or no opinions of the debate (this is for a yes or no debate)
     * if not, get only opinions that correspond to this opinionID
     */

    def getLastYesOrNoOpinionsOrderedbyTimestampJSON(debateID: Long, opinionID: Long, opinion: Int, maxTimestamp: Long,number: Int) = Connection.withConnection {
      username => userID => implicit request => implicit lang => {
        Instantiations.Tables.db.withSession { implicit session =>
          val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
          //We check that the number of debates asked is correct and that the user is allowed to participate in this debate (in order to see results)
          if (number <= controllers.Application.maxResults && number >= 0 && canThisUserParticipateToThisDebate(debateID, userID.toLong)){
            var q2: slick.lifted.Query[models.OpinionYesOrNoDebateTable,models.OpinionYesOrNoDebateTable#TableElementType,Seq] = null
            if (opinionID == -1)
              q2 = Instantiations.Tables.debateYesOrNo.filter(x => x.idDebate === debateID && x.opinion === opinion && x.modificationTimestamp < maxTimestampParam && x.logicalDelete === 0).sortBy(_.modificationTimestamp.desc).take(number)
            else
              q2 = Instantiations.Tables.debateYesOrNo.filter(x => x.idDebate === debateID && x.opinion === opinion && x.modificationTimestamp < maxTimestampParam && x.idObjectUP === opinionID && x.logicalDelete === 0).sortBy(_.modificationTimestamp.desc).take(number)
            val results = q2.list.map(x => convertDebateYesOrNotDatabaseEntryToClass(x))
            implicit val wrtier = debateYesOrNotWrites
            Ok(Json.toJson(results))
          }
          else{
            BadRequest
          }
        }
      }
    }

    /*
     * Returns the json of the yes or no opinions on a yes or not debate ordered by popularity
     */
    def getLastYesOrNoOpinionsForYesOrNoDebateOrderedByPopularityJSON(debateID: Long, opinion: Int, maxPopularity: Long, number: Int) = {
      getLastYesOrNoOpinionsOrderedByPopularityJSON(debateID: Long, -1, opinion: Int, maxPopularity: Long, number: Int)
    }


    /*
     * Returns the json of the yes or no opinions of an opinion on a opinion debate ordered by popularity
     * if opinionid is -1, get all yes or no opinions of the debate (this is for a yes or no debate)
     * if not, get only opinions that correspond to this opinionID
     */
    def getLastYesOrNoOpinionsOrderedByPopularityJSON(debateID: Long, opinionID: Long, opinion: Int, maxPopularity: Long, number: Int): play.api.mvc.Action[play.api.mvc.AnyContent] = Connection.withConnection (
      username => userID => implicit request => implicit lang => {
        getLastYesOrNoOpinionsOrderedByPopularityJSONsub(debateID, opinionID, opinion, number, maxPopularity, username, userID.toLong, request)
      }, Option((request: Request[AnyContent]) => getLastYesOrNoOpinionsOrderedByPopularityJSONsub(debateID, opinionID, opinion, number, maxPopularity, "", -1, request)))

    def getLastYesOrNoOpinionsOrderedByPopularityJSONsub(debateID: Long, opinionID: Long, opinion: Int, number: Int, maxPopularity: Long, username: String, userID: Long, request: Request[AnyContent]) = {
      Instantiations.Tables.db.withSession { implicit session =>
        //We check that the number of debates asked is correct and that the user is allowed to participate in this debate (in order to see results)
        //println("THIS IS OPENEND AS EXPECTED, userID" + userID + "opinionID" + opinionID + "debateID" + debateID)
          if (number <= controllers.Application.maxResults && number >= 0 && canThisUserParticipateToThisDebate(debateID, userID.toLong)){

            var q2: slick.lifted.Query[models.OpinionYesOrNoDebateTable,models.OpinionYesOrNoDebateTable#TableElementType,Seq] = null
            if (opinion == -1){
              if (opinionID == -1)
                q2 = Instantiations.Tables.debateYesOrNo.filter(x => x.idDebate === debateID && x.logicalDelete === 0 && x.popularityTimestamp < maxPopularity).sortBy(_.popularityTimestamp.desc).take(number)
              else
                q2 = Instantiations.Tables.debateYesOrNo.filter(x => x.idDebate === debateID && x.idObjectUP === opinionID  && x.logicalDelete === 0 && x.popularityTimestamp < maxPopularity).sortBy(_.popularityTimestamp.desc).take(number)

            }
            else{
              if (opinionID == -1)
                q2 = Instantiations.Tables.debateYesOrNo.filter(x => x.idDebate === debateID && x.opinion === opinion && x.logicalDelete === 0 && x.popularityTimestamp < maxPopularity).sortBy(_.popularityTimestamp.desc).take(number)
              else
                q2 = Instantiations.Tables.debateYesOrNo.filter(x => x.idDebate === debateID && x.opinion === opinion && x.idObjectUP === opinionID && x.logicalDelete === 0 && x.popularityTimestamp < maxPopularity).sortBy(_.popularityTimestamp.desc).take(number)
            }
            var results = q2.list.map(x => convertDebateYesOrNotDatabaseEntryToClass(x))
           // println("USERID")
            if (userID != -1){
              //This is supposed to favorise opinions that are low otherwise. We do not want to see for ex the first opinion downgraded as a week random opinion.
  
  
              //In this methid we have to sistinguish if opinionID==-1 -> simple debate or if not, then it s an opinion on opinion
  
              //All randomopinions for a user/opinion combination have the same timestamp. If this ts is older than one hour, drop them all and redo them.
              // TODO For now just use one randomopinion, add possibility for any number
              val randomTimeStamp = Instantiations.Tables.debateRandomOpinion.filter(x => x.idDebate === debateID && x.idMember === userID && x.position  === -3).firstOption
              //println("INYESORNOOPINIONS" + randomTimeStamp)
              val nowTimeStamp = new java.sql.Timestamp(System.currentTimeMillis())
  
              //Create Random opinions in the database table: For now just one
              def generateRandomOpinions() = {
                val rand = SimpleFunction.nullary[Double]("RAND")
                val randomStep1 = Instantiations.Tables.debateYesOrNo.filter(x => x.idDebate === debateID && x.popularityTimestamp < maxPopularity && x.logicalDelete === 0).sortBy(_.popularityTimestamp.desc).drop(Application.numOpinionsNotToRandomize).sortBy(x => rand).take(1)
                val randomStep1Results = randomStep1.list
                if (randomStep1Results.nonEmpty) {
                  Instantiations.Tables.debateRandomOpinion += ((debateID, userID, -3, randomStep1Results.head._1.get, nowTimeStamp, Some(nowTimeStamp)))
                }
                else{
                 // print("Not enough opinions posted")
                }
              }
              //No entry for this user/de ate combination exists
              if (randomTimeStamp.isEmpty) {
                //print("Emtpy" + "userID " + userID)
                generateRandomOpinions()
              }
              //Entry has expired
              else if (nowTimeStamp.getTime - randomTimeStamp.get._5.getTime > Application.timeIntervalForReFresh) {
                //print("Removing old ts was" + randomTimeStamp)
                Instantiations.Tables.debateRandomOpinion.filter(x => x.idDebate === debateID && x.idMember === userID && x.position === -3).delete
                generateRandomOpinions()
              }
              else {
             //   print("nothing to do")
              }
              //Now we get the necessary random opinions: one for each 3rd opinion


              val randomOpinionList = if (opinionID == -1) (Instantiations.Tables.debateRandomOpinion.filter(x => x.idDebate === debateID && x.idMember === userID && x.position === -3).sortBy(_.position.asc) innerJoin Instantiations.Tables.debateYesOrNo on
                ((rand, alldebs) => rand.idOpinion === alldebs.id)).list.map(x => convertDebateYesOrNotDatabaseEntryToClass(x._2))
              else (Instantiations.Tables.debateRandomOpinion.filter(x => x.idDebate === debateID && x.idMember === userID && x.position === -3).sortBy(_.position.asc) innerJoin Instantiations.Tables.debateYesOrNo on
                ((rand, alldebs) => rand.idOpinion === alldebs.id && alldebs.idObjectUP === opinionID)).list.map(x => convertDebateYesOrNotDatabaseEntryToClass(x._2))
             // println("JAJAJA" + randomOpinionList)

              //.map(x => convertDebateOpinionDatabaseEntryToClass(x))
              if (randomOpinionList.nonEmpty) {
                //Enough opinions for randomness: fetch them
              //  println("Adding randomness")
                val oneRandomOpinion = randomOpinionList.head
                val first2 = results.take(2)
                if(first2.contains(oneRandomOpinion)){
               //   println("ALERT DO NOT DISPLAY")
                }
                else {
                //  print("We iditots are adding: " + oneRandomOpinion)
                  results = results.take(2) ::: oneRandomOpinion :: results.filterNot(opinion => opinion.id == oneRandomOpinion.id).drop(2)
                }
                //println("Showing as random on third: " + randomOpinionList)
  
              }
             // println("Opinions: " + results)
            }
            implicit val wrtier = debateYesOrNotWrites
            Ok(Json.toJson(results))
          }
          else{
            BadRequest
          }
        }
      
    }
      
    /*
     * Tells if an user has already given his yes or no opinion on an opinion defined by opinionID on an "opinion debate"
     */
    def hasAlreadyGivenHisYesOrNoOpinionOnOpinionJSON(debateID: Long, opinionID: Long)= Connection.withConnection {
      username => userID => implicit request => implicit lang => {
        Instantiations.Tables.db.withSession { implicit session =>
          val q2 = for {c <- Instantiations.Tables.debateYesOrNo  if (c.idMember === userID.toLong && c.idDebate === debateID && c.idObjectUP === opinionID  && c.logicalDelete === 0) } yield c.id
          Ok(Json.toJson((q2.list != Nil)))
        }
      }
    }

    /*
     * Displays the debate defined by debateID
     */
    def getDebate(debateID: Long) = Connection.withConnection (
      username => userID => implicit request => implicit lang => {
      //We check that the user can participate to this debate
      if (canThisUserParticipateToThisDebate(debateID, userID.toLong)){
        //We get the debate and check if it exists
         /* TYPES OF THE OBJECTS : 
            * 0 MEMBER
            * 1 GROUP
            * 2 CONCEPT
            * 3 DEBATE
            * 4 ACTION
            * 5 PETITION
            * 6 YESORNOOPINION
            * 7 OPINION
            * 8 COMMENT
          */
        ActionRepport.addView(3, debateID)
        val debateFromDataBase = getDebateObject(debateID, userID.toLong)
        debateFromDataBase.typeOfDebate match {
          case 0 => Ok(views.html.connected.debates.yesorno("", debateFromDataBase))
          case 1 => Ok(views.html.connected.debates.opinion("", debateFromDataBase))
          case _ => Ok(views.html.connected.debate("Debate type not implemented yet"))
        }
      }
      else{
        Ok(views.html.connected.debate(Messages("views.debate_doesnt_exists")))
      }
      }, Option((request: Request[AnyContent]) => getDebateUnconnected(debateID, request)) )

    def getDebateUnconnected(debateID: Long, request: Request[AnyContent]) = {
      if (canThisUserParticipateToThisDebate(debateID, -1)){
        //We get the debate and check if it exists

        val debateFromDataBase = getDebateObject(debateID)
        if (debateFromDataBase != null){
          /* TYPES OF THE OBJECTS : 
            * 0 MEMBER
            * 1 GROUP
            * 2 CONCEPT
            * 3 DEBATE
            * 4 ACTION
            * 5 PETITION
            * 6 YESORNOOPINION
            * 7 OPINION
            * 8 COMMENT
          */
          ActionRepport.addView(3, debateID)
          debateFromDataBase.typeOfDebate match {
            case 0 => Ok(views.html.connected.debates.yesorno("", debateFromDataBase, false)(request.session, Instantiations.getLang(request))).withSession("language" -> debateFromDataBase.language)
            case 1 => Ok(views.html.connected.debates.opinion("", debateFromDataBase, false)(request.session, Instantiations.getLang(request))).withSession("language" -> debateFromDataBase.language)
            case _ => Ok(views.html.index("")(Instantiations.getLang(request)))
          }
        }
        else{
          Ok(views.html.index("")(Instantiations.getLang(request)))
        }
      }
      else{
        Ok(views.html.index("")(Instantiations.getLang(request)))
      }
    }
    
    /*
     * Tells if an user has already given his opinion on a debate
     */
    def hasAlreadyGivenHisOpinion(debateID: Long, userID: Long, debateType: Int): Boolean = {
      Instantiations.Tables.db.withSession { implicit session =>
      debateType match {
          case 0 => /* yes or no debate */
            val numberOfOccurencesInDatabase = Instantiations.Tables.debateYesOrNo.filter(x => x.idDebate === debateID && x.idMember === userID && x.logicalDelete === 0).length.run
            (numberOfOccurencesInDatabase != 0)
          case 1 => /* opinion debate*/
            val numberOfOccurencesInDatabase = Instantiations.Tables.debateOpinion.filter(x => x.idDebate === debateID && x.idMember === userID && x.logicalDelete === 0).length.run
            (numberOfOccurencesInDatabase != 0)
          case _ => false
        }
      }
    }

    /*
     * Tells the vote of the user has voted for this yes or no opinion
     */
    def getUserVoteOfYesOrNoJSON(yesOrNotOpinionID: Long, idDebate: Long) = Connection.withConnection {
      username => userID => implicit request => implicit lang => {
        Instantiations.Tables.db.withSession { implicit session =>
          if (opinionYesOrNoExists(yesOrNotOpinionID, idDebate)){
            val userIDInt = userID.toLong
            val q2 = for {c <- Instantiations.Tables.voteYesOrNo if (c.idOpinion === yesOrNotOpinionID && c.idMember === userIDInt) } yield c.vote
            val result = q2.list
            if (result == Nil){
              Ok(Json.toJson(-1))
            }
            else{
              Ok(Json.toJson(result.head))
            }
          }
          else{
            BadRequest
          }
        }
      }
    }

     /*
     * Tells the vote of the user has voted for this yes or no opinion
     */
    def getUserVoteJSON(opinionID: Long, idDebate: Long) = Connection.withConnection {
      username => userID => implicit request => implicit lang => {
        Instantiations.Tables.db.withSession { implicit session =>
          if (opinionExists(opinionID, idDebate)){
            val userIDInt = userID.toLong
            val q2 = for {c <- Instantiations.Tables.voteOpinion if (c.idOpinion === opinionID && c.idMember === userIDInt) } yield c.vote
            val result = q2.list
            if (result == Nil){
              Ok(Json.toJson(-1))
            }
            else{
              Ok(Json.toJson(result.head))
            }
          }
          else{
            BadRequest
          }
        }
      }
    }

    /*
     * Creates a debate object from an debateID by looking in the database
     */
    def getDebateObject(debateID: Long, userID : Long = -1): models.Debate = {
      var debateFromDatabase: models.Debate = null
      val valueFromCache = Cache.getAs[models.Debate]("debate" + debateID)
      if (valueFromCache == None){
        Instantiations.Tables.db.withSession { implicit session =>
          
          val q2 = Instantiations.Tables.debate.filter(c => c.id === debateID && c.logicalDelete === 0)
  
          val isdebateFollowed = if (userID == -1 ) false else {
            //If userID was -1, debate is not followed since user not specified. If userId !=1, go look if debate is followed in database
            val debatesAndFollows = for {
              (follow, debatesFiltered) <- Instantiations.Tables.follow.filter(x => x.memberId === userID && x.objectFollowedType === 3) rightJoin q2 on ((follow, debatesFiltered) =>
                (follow.objectFollowedID === debatesFiltered.id))
            } yield (follow.memberId.?, debatesFiltered)
            val isdebateFollowedByUser = debatesAndFollows.list.head._1 match {
              case None => false
              case x => true
            }
  
            isdebateFollowedByUser
        }
          val requestExecuted = q2.list
          if (requestExecuted != Nil){
            val result = requestExecuted.head
  
            debateFromDatabase = convertDebateDatabaseEntryToClass(result) //We convert the debate from database to a models.debate
            if(isdebateFollowed){
              //If debate is followed by the user, modify the value of isUserFollowing, else keep default value 0
              debateFromDatabase = debateFromDatabase.copy(isUserFollowing = 1)
            }
  
          }
        }
        Cache.set("debate" + debateID, debateFromDatabase, Application.cacheDuraction)
        debateFromDatabase
      }
      else{
        valueFromCache.get
      }
    }

    /*
     * Creates a debateOpinion object from an opinionID by looking in the database
     */
    def getDebateOpinionObject(opinionID: Long): models.DebateOpinion = {
      var debateOpinionFromDatabase: models.DebateOpinion = null
      val valueFromCache = Cache.getAs[models.DebateOpinion]("debateOpinion" + opinionID)
      if (valueFromCache == None){
        Instantiations.Tables.db.withSession { implicit session =>
          val q2 = Instantiations.Tables.debateOpinion.filter(c => c.id === opinionID && c.logicalDelete === 0)
          val q2DebateName =  for {(debateOpinion, debate) <- q2 innerJoin Instantiations.Tables.debate on ((debateOpinion, debate) => debateOpinion.idDebate === debate.id)} yield (debateOpinion, debate.question)
          val requestExecuted = q2DebateName.list
          if (requestExecuted != Nil){
            val result = requestExecuted.head
            debateOpinionFromDatabase = convertDebateOpinionDatabaseEntryToClass(result._1).copy(debateQuestion = result._2) //We convert the debate from database to a models.debate
          }
        }
        ActionRepport.delView(7, opinionID) // We delete the view added by the convertDebateOpinionDatabaseEntryToClass function because we are going to add one if we display the opinion in getDebateOpinion
        Cache.set("debateOpinion" + opinionID, debateOpinionFromDatabase, Application.cacheDuraction)
        debateOpinionFromDatabase
      }
      else{
        valueFromCache.get
      }
    }

    def getDebateOpinion(opinionID: Long) = Connection.withConnection ({
      username => userID => implicit request => implicit lang => {
        val opinion = getDebateOpinionObject(opinionID)
        if (opinion != null){
          /* TYPES OF THE OBJECTS : 
            * 0 MEMBER
            * 1 GROUP
            * 2 CONCEPT
            * 3 DEBATE
            * 4 ACTION
            * 5 PETITION
            * 6 YESORNOOPINION
            * 7 OPINION
            * 8 COMMENT
          */
          ActionRepport.addView(7, opinionID)
          Ok(views.html.connected.displayDebateOpinion("", opinion, true))
        }
        else
          Ok(views.html.index(""))
      }
    }, Option((request: Request[AnyContent]) => {
      val opinion = getDebateOpinionObject(opinionID)
        if (opinion != null){
          /* TYPES OF THE OBJECTS : 
            * 0 MEMBER
            * 1 GROUP
            * 2 CONCEPT
            * 3 DEBATE
            * 4 ACTION
            * 5 PETITION
            * 6 YESORNOOPINION
            * 7 OPINION
            * 8 COMMENT
          */
          ActionRepport.addView(7, opinionID)
          Ok(views.html.connected.displayDebateOpinion("", opinion, false)(request.session, Instantiations.getLang(request)))
        }
        else
          Ok(views.html.index("")(Instantiations.getLang(request)))
    }) )
    

    /*
     * Creates a debate yes or no opinion object from an yesOrNotOpinionID by looking in the database
     */
    def getYesOrNoOpinionObject(yesOrNotOpinionID: Long): models.DebateYesOrNot = {
      Instantiations.Tables.db.withSession { implicit session =>
        //println(yesOrNotOpinionID)
        val valueFromCache = Cache.getAs[models.DebateYesOrNot]("debateYesOrNo" + yesOrNotOpinionID)
        if (valueFromCache == None){
          val q2 = Instantiations.Tables.debateYesOrNo.filter(c => c.id === yesOrNotOpinionID && c.logicalDelete === 0)
          val requestExecuted = q2.list
          if (requestExecuted != Nil){
            val valueToReturn = convertDebateYesOrNotDatabaseEntryToClass(requestExecuted.head)
            Cache.set("debateYesOrNo" + yesOrNotOpinionID, valueToReturn, Application.cacheDuraction)
            ActionRepport.delView(6, yesOrNotOpinionID) // We delete the view added by the convertDebateYesOrNoOpinionDatabaseEntryToClass function because we are going to add one if we display the yes or no opinion
            valueToReturn
          }
          else{
            Cache.set("debateYesOrNo" + yesOrNotOpinionID, null, Application.cacheDuraction)
            null
          }
        }
        else{
          valueFromCache.get
        }
      }
    }

    def getDebateYesOrNoOpinion(yesOrNotOpinionID: Long) = Connection.withConnection ({
      username => userID => implicit request => implicit lang => {
        val opinion = getYesOrNoOpinionObject(yesOrNotOpinionID)
        if (opinion != null){
          /* TYPES OF THE OBJECTS : 
            * 0 MEMBER
            * 1 GROUP
            * 2 CONCEPT
            * 3 DEBATE
            * 4 ACTION
            * 5 PETITION
            * 6 YESORNOOPINION
            * 7 OPINION
            * 8 COMMENT
          */
          ActionRepport.addView(6, yesOrNotOpinionID)
          Ok(views.html.connected.displayDebateYesOrNoOpinion("", getYesOrNoOpinionObject(yesOrNotOpinionID), true))
          
        }
        else
          Ok(views.html.index(""))
      }
    }, Option((request: Request[AnyContent]) => {
      val opinion = getYesOrNoOpinionObject(yesOrNotOpinionID)
        if (opinion != null){
          /* TYPES OF THE OBJECTS : 
            * 0 MEMBER
            * 1 GROUP
            * 2 CONCEPT
            * 3 DEBATE
            * 4 ACTION
            * 5 PETITION
            * 6 YESORNOOPINION
            * 7 OPINION
            * 8 COMMENT
          */
          ActionRepport.addView(6, yesOrNotOpinionID)
          Ok(views.html.connected.displayDebateYesOrNoOpinion("", getYesOrNoOpinionObject(yesOrNotOpinionID), false)(request.session, Instantiations.getLang(request)))
        }
        else
          Ok(views.html.index("")(Instantiations.getLang(request)))
    }) )
    

    /*
     * Vote for opinion defined in POSTDATA
     */
    def voteForOpinion(idDebate: Long) = Connection.withConnection ({
      username => userID => implicit request => implicit lang =>
      //We first get the posted idOpinion and vote
      val args = request.body.asFormUrlEncoded.get
      var returnedJson = 0
      if (args != None && args.contains("vote") && args.contains("idOpinion")
          &&
          canThisUserParticipateToThisDebate(idDebate, userID.toLong)
         ){
        voteForOpinionSub(idDebate, userID.toLong, Integer.parseInt(args.get("vote").get(0)), args.get("idOpinion").get(0).toLong)
      
      }
      else{
        BadRequest
      }
    }, None, true)

    def voteForOpinionSub(idDebate: Long, userIDInt: Long, vote: Int, idOpinion: Long) = {
      //We check that the arguments are correct and that the user can participate to this debate
      
        val debateFromDataBase = getDebateObject(idDebate, userIDInt)
        val idPrivateGroup = debateFromDataBase.privateGroupId
        
        Instantiations.Tables.db.withSession { implicit session =>
          //We first check if the user hasn't voted yet for this opinion, if the opinion exists in this debate, and if the vote is 0 or 1
          val q2 = for { c <- Instantiations.Tables.voteOpinion if (c.idOpinion === idOpinion && c.idMember === userIDInt) } yield c.vote
          val voteList = q2.list
          
          var updatePopularityTimestamp = false
          val opinionObject = getDebateOpinionObject(idOpinion)
          if (opinionObject != null && (vote == 0 || vote == 1 )){
            Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              
              Instantiations.Tables.db.withSession { implicit session =>
              println("eeezz")
            /*Coefficient to multiply the basic popularity in order to augment it if the post has been 
             * modified recently, and to give an (almost) unique popularity 
             */
            val idCreatorOpinion = opinionObject.idMember
            
            val currentTimeMinusConst = ((System.currentTimeMillis()/1000l)-1447000000l).toDouble
            val debateTimeMinusConst = ((debateFromDataBase.modificationTimestamp/1000l)-1447000000l).toDouble
            val division: Double = currentTimeMinusConst/debateTimeMinusConst
            
            val coefForPopularityTimestamp = 100000 * (currentTimeMinusConst/debateTimeMinusConst)
            //println(coefForPopularityTimestamp)
            //The user can vote
            if (voteList == Nil){ //If this is a new vote
              Instantiations.Tables.voteOpinion += (idOpinion, userIDInt, vote)
              if (vote == 1){
                sqlu"update DEBATEOPINION set VOTEFOR=VOTEFOR+1 where SUP_ID = $idOpinion".first
                //sqlu"update DEBATEOPINION set POPULARITYTIMESTAMP=POPULARITY where SUP_ID = $idOpinion".first
                sqlu"update DEBATEOPINION set POPULARITY=POPULARITY+4 where SUP_ID = $idOpinion".first
                
                User.modifynumberofLikes(idCreatorOpinion, 1)
                
                ActionRepport.actionDone(userIDInt, 3, idDebate, idOpinion, 30016, null, idPrivateGroup)
              }
              else{
                sqlu"update DEBATEOPINION set VOTEAGAINST=VOTEAGAINST+1 where SUP_ID = $idOpinion".first
                sqlu"update DEBATEOPINION set POPULARITY=POPULARITY-2 where SUP_ID = $idOpinion".first
                
                User.modifynumberofDislikes(idCreatorOpinion, 1)                
                ActionRepport.actionDone(userIDInt, 3, idDebate, idOpinion, 30017, null, idPrivateGroup)
              }
              updatePopularityTimestamp = true
            }
            else if (voteList.head != vote){ //This is a modification of his vote
              q2.update(vote)
              if (vote == 0){
                //We changed the vote for a negative vote
                sqlu"update DEBATEOPINION set VOTEFOR=VOTEFOR-1 where SUP_ID = $idOpinion".first
                sqlu"update DEBATEOPINION set VOTEAGAINST=VOTEAGAINST+1 where SUP_ID = $idOpinion".first
                sqlu"update DEBATEOPINION set POPULARITY=POPULARITY-6 where SUP_ID = $idOpinion".first
                
                User.modifynumberofLikes(idCreatorOpinion, -1)
                User.modifynumberofDislikes(idCreatorOpinion, 1)  
                
                ActionRepport.actionDone(userIDInt, 3, idDebate, idOpinion, 30018, null, idPrivateGroup)
              }
              else{
                //We changed the vote for a positive vote
                sqlu"update DEBATEOPINION set VOTEAGAINST=VOTEAGAINST-1 where SUP_ID = $idOpinion".first
                sqlu"update DEBATEOPINION set VOTEFOR=VOTEFOR+1 where SUP_ID = $idOpinion".first
                sqlu"update DEBATEOPINION set POPULARITY=POPULARITY+6 where SUP_ID = $idOpinion".first
                
                User.modifynumberofLikes(idCreatorOpinion, 1)  
                User.modifynumberofDislikes(idCreatorOpinion, -1)
                
                
                ActionRepport.actionDone(userIDInt, 3, idDebate, idOpinion, 30019, null, idPrivateGroup)
              }
              updatePopularityTimestamp = true
            }
            if (updatePopularityTimestamp){
              Cache.remove("debateOpinion" + idOpinion)
              Cache.remove("memberStats" + idCreatorOpinion)
              sqlu"update DEBATEOPINION set POPULARITYTIMESTAMP=POPULARITY*$coefForPopularityTimestamp where SUP_ID = $idOpinion".first
            }
              }
            }
            
            Ok
          }
          else{
            BadRequest
          }
        }
    }
    
     /*
      * Votes for a yes or no opinion defined in POSTDATA
      */
    def voteForYesOrNoOpinion(idDebate: Long) = Connection.withConnection ({
      username => userID => implicit request => implicit lang =>
      //We first get the posted idOpinion and vote
      val args = request.body.asFormUrlEncoded.get
      var returnedJson = 0
      //We check that the arguments are correct and that the user can participate to this debate
      if (args != None && args.contains("vote") && args.contains("idOpinion")
          &&
          canThisUserParticipateToThisDebate(idDebate, userID.toLong)
         ){

        //We extract the values posted
        val userIDInt = userID.toLong
        val vote = Integer.parseInt(args.get("vote").get(0))
        val idOpinion = args.get("idOpinion").get(0).toLong
        
        val debateFromDataBase = getDebateObject(idDebate, userIDInt)
        val idPrivateGroup = debateFromDataBase.privateGroupId
        
        var updatePopularityTimestamp = false
        println("coucou")
        Instantiations.Tables.db.withSession { implicit session =>
          //We first check if the user hasn't voted yet for this opinion, if the opinion exists in this debate, and if the vote is 0 or 1
          val q2 = for { c <- Instantiations.Tables.voteYesOrNo if (c.idOpinion === idOpinion && c.idMember === userIDInt) } yield c.vote
          val voteList = q2.list
          val currentTimeMinusConst = ((System.currentTimeMillis()/1000l)-1447000000l).toDouble
          val debateTimeMinusConst = ((debateFromDataBase.modificationTimestamp/1000l)-1447000000l).toDouble
          val division: Double = currentTimeMinusConst/debateTimeMinusConst
            
          val coefForPopularityTimestamp = 100000 * (currentTimeMinusConst/debateTimeMinusConst)
          
          val opinionYesOrNo = getYesOrNoOpinionObject(idOpinion)
          
          if (opinionYesOrNo != null && (vote == 0 || vote == 1 )){ //If this is a new vote
            Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              
              val idCreatorYesOrNoOpinion = opinionYesOrNo.idMember
              if (voteList == Nil){
                //We can take this vote into account
                Instantiations.Tables.voteYesOrNo += (idOpinion, userIDInt, vote)
                if (vote == 1){
                  sqlu"update DEBATEYESORNO set VOTEFOR=VOTEFOR+1 where SUP_ID = $idOpinion".first
                  sqlu"update DEBATEYESORNO set POPULARITY=POPULARITY+4 where SUP_ID = $idOpinion".first
                  
                  User.modifynumberofLikes(idCreatorYesOrNoOpinion, 1)
                  ActionRepport.actionDone(userIDInt, 3, debateFromDataBase.id, idOpinion, 30002, null, idPrivateGroup)
                }
                else{
                  sqlu"update DEBATEYESORNO set VOTEAGAINST=VOTEAGAINST+1 where SUP_ID = $idOpinion".first
                  sqlu"update DEBATEYESORNO set POPULARITY=POPULARITY-2 where SUP_ID = $idOpinion".first
                  
                  
                  User.modifynumberofDislikes(idCreatorYesOrNoOpinion, 1)
                  
                  ActionRepport.actionDone(userIDInt, 3, debateFromDataBase.id, idOpinion, 30003, null, idPrivateGroup)
                }
                updatePopularityTimestamp = true
              }
              else if (voteList.head != vote){
                q2.update(vote)
                val statement = q2.updateStatement
                val invoker = q2.updateInvoker
                if (vote == 0){
                  //We changed the vote for a negative vote
                  sqlu"update DEBATEYESORNO set VOTEFOR=VOTEFOR-1 where SUP_ID = $idOpinion".first
                  sqlu"update DEBATEYESORNO set VOTEAGAINST=VOTEAGAINST+1 where SUP_ID = $idOpinion".first
                  sqlu"update DEBATEYESORNO set POPULARITY=POPULARITY-6 where SUP_ID = $idOpinion".first
                  
                  User.modifynumberofLikes(idCreatorYesOrNoOpinion, -1)
                  User.modifynumberofDislikes(idCreatorYesOrNoOpinion, 1)
                  
                  ActionRepport.actionDone(userIDInt, 3, debateFromDataBase.id, idOpinion, 30004, null, idPrivateGroup)
                }
                else{
                  //We changed the vote for a positive vote
                  sqlu"update DEBATEYESORNO set VOTEAGAINST=VOTEAGAINST-1 where SUP_ID = $idOpinion".first
                  sqlu"update DEBATEYESORNO set VOTEFOR=VOTEFOR+1 where SUP_ID = $idOpinion".first
                  sqlu"update DEBATEYESORNO set POPULARITY=POPULARITY+6 where SUP_ID = $idOpinion".first
                  
                  User.modifynumberofLikes(idCreatorYesOrNoOpinion, 1)
                  User.modifynumberofDislikes(idCreatorYesOrNoOpinion, -1)
                  
                  ActionRepport.actionDone(userIDInt, 3, debateFromDataBase.id, idOpinion, 30005, null, idPrivateGroup)
                }
                updatePopularityTimestamp = true
              }
              if (updatePopularityTimestamp){
                Cache.remove("debateYesOrNo" + idOpinion)
                Cache.remove("memberStats" + idCreatorYesOrNoOpinion)
                sqlu"update DEBATEYESORNO set POPULARITYTIMESTAMP=POPULARITY*$coefForPopularityTimestamp where SUP_ID = $idOpinion".first
              }
          }
            Ok
          }
          else{
            BadRequest
          }
        }
      }
      else{
        BadRequest
      }
    }, None, true)


    def getParticipantsOfDebate(debateID: Long, userID: Long = -1): List[Long] = {
      val debateObject = getDebateObject(debateID: Long, userID)
      Instantiations.Tables.db.withSession { implicit session =>
        debateObject.typeOfDebate match {
          case 0 => //yesorno  
            (for { c <- Instantiations.Tables.debateYesOrNo if (c.idDebate === debateID && c.logicalDelete === 0) } yield c.idMember).list
          case 1 => //opinion
            (for { c <- Instantiations.Tables.debateOpinion if (c.idDebate === debateID && c.logicalDelete === 0) } yield c.idMember).list
          case _ => Nil
        }
      }
    }

    /*
     * Returns if a debate defined by debateId exists in the database
     */
    def debateExists(debateId: Long):Boolean = {
      Instantiations.Tables.db.withSession { implicit session =>
        val q2 = for { c <- Instantiations.Tables.debate if (c.id === debateId && c.logicalDelete === 0) } yield c.id
        val requestExecuted = q2.list
        (requestExecuted != Nil)
      }
    }

    /*
     * Returns if a opinion on a yes or no debate defined by opinionYesOrNoId exists in the database and if this opinion is linked to the correct debateId
     */
    def opinionYesOrNoExists(opinionYesOrNoId: Long, idDebate: Long):Boolean = {
      Instantiations.Tables.db.withSession { implicit session =>
        val q2 = for { c <- Instantiations.Tables.debateYesOrNo if (c.id === opinionYesOrNoId && c.logicalDelete === 0) } yield c.idDebate
        val requestExecuted = q2.list
        (requestExecuted != Nil && requestExecuted.head == idDebate)
      }
    }

    /*
     * Returns if a opinion on a debate defined by opinionid exists in the database and if this opinion is linked to the correct debateId
     */
    def opinionExists(opinionId: Long, idDebate: Long):Boolean = {
      Instantiations.Tables.db.withSession { implicit session =>
        val q2 = for { c <- Instantiations.Tables.debateOpinion if (c.id === opinionId && c.logicalDelete === 0) } yield c.idDebate
        val requestExecuted = q2.list
        (requestExecuted != Nil && requestExecuted.head == idDebate)
      }
    }
    
    /*
   * Deletes a debate defined by the "key" POSTDATA. Returns 1 if the comment has been deleted, and 0 otherwise
   */
  def processDebateDelete  = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
    var jsonToSend = 0
    val userIDLong = userID.toLong
    val args = request.body.asFormUrlEncoded.get
    if (
       args != None 
         && 
       args.contains("key") 
       )
    {
      Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      { 
          //First we verify that the debate exists
          val creatorTypeAndIdList = (for { c <- Instantiations.Tables.debate if (c.id === args.get("key").get(0).toLong) } yield (c.privateGroupId)).list
          if (creatorTypeAndIdList != Nil){
            if (creatorTypeAndIdList.head != -1){
              //The creator is a group
              if (Groups.canThisUserDeleteGroupContent(userIDLong, creatorTypeAndIdList.head)){
                val q2 = for { c <- Instantiations.Tables.debate if (c.id === args.get("key").get(0).toLong) } yield c.logicalDelete
                q2.update(1) //We set LOGICALDELETE to 1
                val statement = q2.updateStatement
                val invoker = q2.updateInvoker
                
                //We delete the activities specific to a debate group creation ("DebateForGroupCreation")
                val q5 = for { c <- Instantiations.Tables.activity if (c.activityType === 30010 && c.activityObject === creatorTypeAndIdList.head && c.activityTarget === args.get("key").get(0).toLong) } yield c.logicalDelete
                q5.update(1) //We set LOGICALDELETE to 1
                val statement5 = q5.updateStatement
                val invoker5 = q5.updateInvoker
                
                jsonToSend = 1
              }
            }
            else{
              val q2 = for { c <- Instantiations.Tables.debate if (c.id === args.get("key").get(0).toLong && c.idCreator === userIDLong) } yield c.logicalDelete
              q2.update(1) //We set LOGICALDELETE to 1
              val statement = q2.updateStatement
              val invoker = q2.updateInvoker
              
              //We delete the activities specific to a member group creation ("DebateCreation")
              val q5 = for { c <- Instantiations.Tables.activity if (c.activityType === 30000 && c.memberId === userIDLong && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
              q5.update(1) //We set LOGICALDELETE to 1
              val statement5 = q5.updateStatement
              val invoker5 = q5.updateInvoker
               
              
              jsonToSend = 1
            }
            //We deleted the debate, now we have to delete all activity associated with it
            /*
             *30020-PostYesOrNoOpinionOnOpinion //memberID : The member who posted his opinion / Object: the id of the opinion where the yes or no opinion was posted / Target : The id of the yes or no opinion
     
             */
            
            //We delete the votefor yes or no opinion and the voteagainst yes or no opinion and the ModifVoteForToAgainstYesOrNoOpinion and the ModifVoteAgainstToForYesOrNoOpinion
            //and the votefor opinion and the voteagainst opinion and the ModifVoteForToAgainstOpinion and the ModifVoteAgainstToForOpinion
            val q4 = for { c <- Instantiations.Tables.activity if ((c.activityType === 30002 || c.activityType === 30003 || c.activityType === 30004 || c.activityType === 30005 || c.activityType === 30016 || c.activityType === 30017 || c.activityType === 30018 || c.activityType === 30019 ) && c.activityTarget === args.get("key").get(0).toLong) } yield c.logicalDelete
            val statement4 = q4.updateStatement
            val invoker4 = q4.updateInvoker
            
            //We then delete the DebateYesOrNoParticipation and the DebateOpinionParticipation
            val q5 = for { c <- Instantiations.Tables.activity if ((c.activityType === 30001 || c.activityType === 30015) && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
            val statement5 = q5.updateStatement
            val invoker5 = q5.updateInvoker
            
            //We delete from cache 
            Cache.remove("debate" + args.get("key").get(0).toLong)
          }
        }
      }
    }
    Ok(Json.toJson(jsonToSend))
  }, None, true)
}
