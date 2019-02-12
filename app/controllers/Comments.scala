package controllers
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

object Comments extends Controller {
  /* TYPES OF THE OBJECTS : 
   * 0 MEMBER
   * 1 GROUP
   * 2 CONCEPT
   * 3 DEBATE
   * 4 ACTION
   * 5 PETITION
   * 6 YESORNOOPINION
   * 7 OPINION
   */
  
  
   /*
  * Converts a database Comment entry into a proper models.Comment class
  */ 
  def convertConceptDatabaseEntryToClass(databaseEntry: models.Types.commentEntry) = {
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
    ActionRepport.addView(8, databaseEntry._1.get)
    models.Comment(databaseEntry._1.get, databaseEntry._2, "", databaseEntry._5, databaseEntry._7.get.getTime())
  }
  
  val commentWrites: Writes[models.Comment] = {
    ((JsPath \ "id").write[Long] and
    (JsPath \ "senderId").write[Long] and
    (JsPath \ "senderName").write[String] and
    (JsPath \ "message").write[String] and
    (JsPath \ "modificationTimestamp").write[Long])(unlift(models.Comment.unapply))
  }
  
  /*
   * Returns if an member can comment on an object defined by its objetType and objectID
   */
  def canThisUserAccessCommentsHere(objectType: Long, objectID: Long, userID: Long): Boolean = {
    if (
        (objectType == 1 && Groups.canThisUserParticipateToThisGroup(objectID, userID))
        ||
        (objectType == 2)
        ||
        (objectType == 3 && Debates.canThisUserParticipateToThisDebate(objectID, userID))
        ||
        (objectType == 4 && Actions.canThisUserParticipateToThisAction(objectID, userID))
        ||
        (objectType == 5 && Petitions.canThisUserParticipateToThisPetition(objectID, userID))
        ||
        (objectType == 6 && Debates.canThisUserParticipateToThisYesOrNoOpinion(objectID, userID))
       )
    {
      true
    }
    else{
      false
    }
  }
  
  /*
   * Posts a comment on an object defined by its objectType and objectID. Returns 1 if the comment has been posted and 0 if not
   */
  
  def processCommentCreationForm = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
    var jsonToSend = 0
      models.Forms.commentSendingForm.bindFromRequest.value map { commentSendingForm =>
        val objectType = commentSendingForm.objecType.toLong
        val objectID = commentSendingForm.objectID.toLong
        val message = commentSendingForm.message
        val userIDInt = userID.toLong
        var commentID = -1l
        if (message.length < Application.maxCharactersOpinions && canThisUserAccessCommentsHere(objectType, objectID, userIDInt)){
          Instantiations.Tables.db.withSession { implicit session =>
            //println("created" + objectType)
            commentID = (Instantiations.Tables.comment returning Instantiations.Tables.comment.map(_.id)) += ((None, userIDInt, objectType , objectID, Jsoup.clean(message, Application.whiteListHTML), new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0))
            jsonToSend = 1
          }
          
          //We then handle the notifications
          objectType match {
            case 1 => {
              ActionRepport.actionDone(userIDInt, 1, objectID, commentID, 60001, null) //CommentConcept
            }
            case 2 => {
              val conceptOwner = Concepts.getConceptModel(objectID).idCreator
              ActionRepport.actionDone(userIDInt, 2, objectID, commentID, 60002, null) //CommentConcept
            }
            case 4 => { 
              val actionModel = Actions.getActionModel(objectID)
              var groupPrivacy = -1l
              if (actionModel.creatorType == 1) //We only track the creator for actions created by an user
                groupPrivacy = actionModel.creatorID
              ActionRepport.actionDone(userIDInt, 4, objectID, commentID, 60004, null, groupPrivacy) //CommentAction
            }
            case 5 => {
              val petitionModel = Petitions.getPetitionModel(objectID)
              var groupPrivacy = -1l
              if (petitionModel.creatorType == 1) 
                groupPrivacy = petitionModel.creatorID
              ActionRepport.actionDone(userIDInt, 5, objectID, commentID, 60005, null, groupPrivacy) //CommentPetition
            }
            case 6 => {
              val yesornoModel = Debates.getYesOrNoOpinionObject(objectID)
              var yesornoOwner = yesornoModel.idMember
              ActionRepport.actionDone(userIDInt, 6, objectID, commentID, 60006, null, Debates.getDebateObject(yesornoModel.idDebate, userIDInt).privateGroupId) //CommentYesornoopinion
            }
          }
        }
      }
    Ok(Json.toJson(jsonToSend))
  }, None, true)
  
  /*
   * Returns the Comment object of the comment defined by its commentId
   */
  def getCommentObject(commentId: Long): models.Comment = {
    val valueFromCache = Cache.getAs[models.Comment]("comment" + commentId)
    if (valueFromCache == None){
      Instantiations.Tables.db.withSession { implicit session =>
        val q2List = Instantiations.Tables.comment.filter(_.id === commentId).list
        if (q2List != Nil){
          val valueToReturn = convertConceptDatabaseEntryToClass(q2List.head)
          ActionRepport.delView(8, commentId) // We delete the view added by the convertCommentOpinionDatabaseEntryToClass function because we are going to add one if we display the comment
            
          Cache.set("comment" + commentId, valueToReturn, Application.cacheDuraction)
          valueToReturn
        }
        else{
          Cache.set("comment" + commentId, null, Application.cacheDuraction)
          null
        }
      }
    }
    else{
      Cache.getAs[models.Comment]("comment" + commentId).get
    }
  }
  
  /*
   * Deletes a comment. Returns 1 if the comment has been deleted, and 0 otherwise
   */
  def processCommentDelete  = Connection.withConnection({
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
          Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
          val q2 = for { c <- Instantiations.Tables.comment if (c.id === args.get("key").get(0).toLong && c.idSender === userIDLong) } yield c.logicalDelete
          q2.update(1) //We set LOGICALDELETE to 1
          val statement = q2.updateStatement
          val invoker = q2.updateInvoker
          
          //We now delete activity associated
          val q3 = for { c <- Instantiations.Tables.activity if (c.activityType >= 60001 && c.activityType <= 60006 && c.activityTarget === args.get("key").get(0).toLong) } yield c.logicalDelete
          q3.update(1) //We set LOGICALDELETE to 1
          val statement2 = q3.updateStatement
          val invoker2 = q3.updateInvoker
          }
        }
        jsonToSend = 1
    }
    Ok(Json.toJson(jsonToSend))
  }, None, true)
  
  /*
   * Modifies a comment. Returns 1 if the comment has been modified, and 0 otherwise
   */
  def processCommentModificationForm = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
    var jsonToSend = 0
      models.Forms.commentModificationForm.bindFromRequest.value map { commentModificationForm =>
        Instantiations.Tables.db.withSession { implicit session =>
          val userIDLong = userID.toLong
          val commentID = commentModificationForm.id.toLong
          val newCommentMessage = commentModificationForm.message
          val q2 = for { c <- Instantiations.Tables.comment if (c.id === commentID && c.idSender === userIDLong  && c.logicalDelete === 0) } yield c.message
          if (q2.exists.run && newCommentMessage.length < Application.maxCharactersOpinions){
            Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute

                q2.update(newCommentMessage)
                val statement = q2.updateStatement
                val invoker = q2.updateInvoker
              }
            }
            jsonToSend = 1
          }
        }
      }
        Ok(Json.toJson(jsonToSend))
  }, None, true)
  
  def getNumberComments(objectType: Long, objectID: Long) = Action {
        Instantiations.Tables.db.withSession { implicit session =>
          val q2 = for {c <- Instantiations.Tables.comment if c.objectCommentedType === objectType && c.objectCommentedID === objectID && c.logicalDelete === 0} yield c.id

          Ok(Json.toJson(q2.length.run))
        }
      
  }
  
  
  /*
     * Returns the json of the comments on an object defined by its objectType and objectID
     */
  def getLastCommentsOrderedbyTimestampJSON(objectType: Long, objectID: Long, maxTimestamp: Long,number: Int) = Connection.withConnection(
    username => userID => implicit request => implicit lang => {
      getLastCommentsOrderedbyTimestampJSONsub(userID.toLong, objectType, objectID, maxTimestamp, number)}
  , Option((request: Request[AnyContent]) => getLastCommentsOrderedbyTimestampJSONsub(-1, objectType, objectID, maxTimestamp, number)))
    
  def getLastCommentsOrderedbyTimestampJSONsub(userID: Long, objectType: Long, objectID: Long, maxTimestamp: Long,number: Int) = {
        Instantiations.Tables.db.withSession { implicit session =>
          val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
          //We check that the number of debates asked is correct and that the user is allowed to participate in this debate (in order to see results)
          if (number <= controllers.Application.maxResults && number >= 0 && canThisUserAccessCommentsHere(objectType, objectID, userID.toLong)){
            //val q2 = Instantiations.Tables.conversationMessages.filter(x => x.idConversation === conversationID && x.modificationTimestamp < maxTimestampParam).sortBy(_.modificationTimestamp.desc).take(number)
            
            val explicitInnerJoinDebateYesOrNo = for {
		    (comment, member) <- Instantiations.Tables.comment innerJoin Instantiations.Tables.members on
		    ((comment, member) => comment.objectCommentedType === objectType && comment.objectCommentedID === objectID && comment.idSender === member.id && comment.creationTimestamp < maxTimestampParam && comment.logicalDelete === 0)
		    } yield (comment, member.username)
            val results = explicitInnerJoinDebateYesOrNo.sortBy(_._1.creationTimestamp.desc).take(number).list.par.map(x => convertConceptDatabaseEntryToClass(x._1).copy(senderName = x._2)).seq
            
            implicit val wrtier = commentWrites
            Ok(Json.toJson(results))
          }
          else{
            BadRequest
          }
        }
   }
  
   
  
}