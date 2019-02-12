package controllers

import play.api._
import play.api.mvc._
import play.api.i18n._
import play.api.i18n.I18nSupport
import play.api.Play.current
import slick.driver.MySQLDriver.simple._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data._
import play.api.data.Forms._
import slick.jdbc.{GetResult, StaticQuery => Q}
import slick.jdbc.JdbcBackend.Database
import Q.interpolation


object Conversations extends Controller {

  /*
   * Converts a conversationMember entry from database to a models.ConversationMember
   */
  def convertConversationMemberDatabaseEntryToClass(databaseEntry: models.Types.conversationMemberEntry) = {
    models.ConversationMember(databaseEntry._2, "", databaseEntry._3, -1l, "", "", databaseEntry._4.getTime())
  }

  /*
   * Converts a conversationMessage entry from database to a models.ConversationMessage
   */
  def convertConversationMessageDatabaseEntryToClass(databaseEntry: models.Types.conversationMessagesEntry) = {
    models.ConversationMessage(databaseEntry._3, "", "", "", Application.outputHTMLText(databaseEntry._4, true), databaseEntry._5.getTime())
  }

  /*
   * Converts a conversation entry from database to a models.ConversationMessage
   */ //id: Long, conversationString: String, modificationTimestamp: Long
  def convertConversationDatabaseEntryToClass(databaseEntry: models.Types.conversation) = {
    models.Conversation(databaseEntry._1.get, databaseEntry._2, databaseEntry._3.getTime())
  }


  //lastSenderID: Long, lastSenderUsername: String, lastMessage: String,
  val conversationMemberWrites: Writes[models.ConversationMember] = {
    ((JsPath \ "conversationId").write[Long] and
    (JsPath \ "conversationString").write[String] and
    (JsPath \ "newMessages").write[Boolean] and
    (JsPath \ "lastSenderID").write[Long] and
    (JsPath \ "lastSenderUsername").write[String] and
    (JsPath \ "lastMessage").write[String] and
    (JsPath \ "modificationTimestamp").write[Long])(unlift(models.ConversationMember.unapply))
  }

  val conversationMessageWrites: Writes[models.ConversationMessage] = {
    ((JsPath \ "senderId").write[Long] and
    (JsPath \ "senderName").write[String] and
    (JsPath \ "senderFirstname").write[String] and
    (JsPath \ "senderUsername").write[String] and
    (JsPath \ "message").write[String] and
    (JsPath \ "modificationTimestamp").write[Long])(unlift(models.ConversationMessage.unapply))
  }
  
  
   
   
    /*
     * Creates a conversation. Returns JSON 0 if not created, 1 if created and 2 if a conversation with these members already existed (so we just added the message to the conversation already existing)
     */
    def processConversationCreationForm = {
      Connection.withConnection({
        username => userID => implicit request => implicit lang =>
	      models.Forms.conversationCreationForm.bindFromRequest.value map { conversationCreationForm =>
	        val receiversWithDuplicates: Seq[String] = conversationCreationForm.receivers.head.split(",") :+ username
	        val message = conversationCreationForm.message
	        val userIDInt = Integer.parseInt(userID)
	        if (receiversWithDuplicates.size <= Application.maxNumberOfMessageReceivers){
	          /* First we check if every user exists :
	           * The array is parallelized, then a function checking if each member exists is applied,
	           * and the AND logical operation is applied to the resulting array
	           */
	          val receivers = receiversWithDuplicates.toSet.toList //We remove duplicates entries
	          val areAllUsersOk = receivers.par.map(x => User.doesMemberExistsFromPseudo(x)).reduceLeft[Boolean](_ && _)
	          if (areAllUsersOk){
	            //We sort the receivers array in order to have the same string for the same set of users
	            val receiversSortedFlat = receivers.sorted.reduceLeft[String](_ + ',' + _)
	            Instantiations.Tables.db.withSession { implicit session =>
	              //We check if the conversation already exists
	              var jsonToReturn = 2 //JSON to return if we don't create a new conversation
	              val q2 = for {c <- Instantiations.Tables.conversation if c.conversationString === receiversSortedFlat} yield c.id
	              var conversationRetrieved = q2.list
	              if (conversationRetrieved == Nil){
	                //We then create the conversation and get the ID of it
	                Instantiations.Tables.conversation += ((None, receiversSortedFlat, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis()))))

	                conversationRetrieved = q2.list
	                //We add every member in the list to the conversationmembersTable
	                receivers.par.foreach { x =>
	                  Instantiations.Tables.conversationMember += ((User.getMemberIdFromPseudo(x), conversationRetrieved.head, true, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis()))))
	                }
	                jsonToReturn = 1
	              }
	              else{
	                //We have a new message to the conversation, we tell it to the participants
	                setConversationUnread(conversationRetrieved.head)
	              }
	              val conversationID = conversationRetrieved.head
	              //We add the message to the conversation
	              Instantiations.Tables.conversationMessages += ((None, conversationID, userIDInt, scala.xml.Utility.escape(message), new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis()))))
	              Ok(Json.toJson(jsonToReturn))
	            }
	          }
	          else{
	            Ok(Json.toJson(0))
	          }
	        }
	        else{
	          BadRequest
	        }
	      } getOrElse {
             BadRequest
          }
      }, None, true)
    }

    /*
     * Posts on a conversation defined by its conversationID. Sends 0 if the post failed and 1 if not
     */
    def processConversationSendingForm = Connection.withConnection({
        username => userID => implicit request => implicit lang =>
          var jsonToSend = 0
          Instantiations.Tables.db.withSession { implicit session =>
	        models.Forms.conversationSendingForm.bindFromRequest.value map { conversationSendingForm =>
	          if (conversationSendingForm.message.length < Application.maxCharactersOpinions){
	            val userIDInt = userID.toLong
  	            val conversationID = conversationSendingForm.id.toLong
	            val messageToSend = conversationSendingForm.message
	            if (canThisMemberParticipateToThisConversation(conversationID, userIDInt)){
	              //We add the message and set the conversation unread for all users
	              Instantiations.Tables.conversationMessages += ((None, conversationID, userIDInt, messageToSend, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis()))))
	              setConversationUnread(conversationID)
	              jsonToSend = 1
	            }
	          }
	        }
          }
	      Ok(Json.toJson(jsonToSend))
      }, None, true)
  /*
   * Returns if a member can participate to a conversation
   */
  def canThisMemberParticipateToThisConversation(conversationID: Long, userIDInt : Long) = Instantiations.Tables.db.withSession { implicit session =>
    val q3 = for {c <- Instantiations.Tables.conversationMember if c.memberId === userIDInt} yield c.newMessages
	(q3.list != Nil)
  }

  /*
   * Retrieves the last 10 conversation where an user belongs
   */
  def getLastConversationsOfUserOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        if (number <= controllers.Application.maxResults && number >= 0){
          val userIDInt = userID.toLong
          
          /*First, we retrieve, only the "last messages of each conversation", in order to join each of them
           * with its corresponding conversation later
           */
          
          //View that shows only the last message of each conversation
          //First we get the id of the last message
          val maxQuery =
          Instantiations.Tables.conversationMessages
            .groupBy { _.idConversation }
            .map {
              case (idConversation, ua) =>
                idConversation -> ua.map(_.modificationTimestamp).max
            }
          //Then we get the proper message
          val onlyLastMessages =
          for {
            ua <- Instantiations.Tables.conversationMessages
            m <- maxQuery
            if (ua.idConversation === m._1 && ua.modificationTimestamp === m._2)
          } yield ua
          
          /*
           * Then, we retrieve each conversation of the member joined 
           * The request is ugly but it allows to have everything in one request, 
           * witch is useful for such a complex request called in each page 
           * (saves a lot of time and database power)
           */
          val listOfConversationWithLastConversationMessage = for {
		      (((conversationMember, conversation), conversationMessage), member) <- 
	    	  ((Instantiations.Tables.conversationMember innerJoin /*We get the conversations of the user by joining the conversations and conversationmember tables*/ Instantiations.Tables.conversation on
		        ((conversationMember, conversation) => 
		          conversationMember.conversationId === conversation.id && 
		          conversationMember.memberId === userIDInt && 
		          conversationMember.modificationTimestamp < maxTimestampParam)) leftJoin/*We join each conversation of the user to its last message posted*/ onlyLastMessages on (_._2.id === _.idConversation)) leftJoin /*We join with the members tables in order to get the username of the last sender in order to display it in the GUI*/ Instantiations.Tables.members on (_._2.idSender === _.id)
          } yield (conversationMember, conversation.conversationString, conversationMessage.idSender, conversationMessage.message, conversation.id, member.username)

          
         val results = listOfConversationWithLastConversationMessage.sortBy(_._1.modificationTimestamp.desc).take(number).list.par.map(x => convertConversationMemberDatabaseEntryToClass(x._1).copy(conversationString = x._2, lastSenderID = x._3, lastSenderUsername = x._6, lastMessage = x._4)).seq
          
         implicit val wrtier = conversationMemberWrites
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
    def getLastConverationMessagesOrderedbyTimestampJSON(conversationID: Long, maxTimestamp: Long,number: Int) = Connection.withConnection {
      username => userID => implicit request => implicit lang => {
        Instantiations.Tables.db.withSession { implicit session =>
          val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
          val userIDInt = userID.toLong
          //We check that the number of debates asked is correct and that the user is allowed to participate in this debate (in order to see results)
          if (number <= controllers.Application.maxResults && number >= 0 && canThisMemberParticipateToThisConversation(conversationID, userIDInt)){
            //val q2 = Instantiations.Tables.conversationMessages.filter(x => x.idConversation === conversationID && x.modificationTimestamp < maxTimestampParam).sortBy(_.modificationTimestamp.desc).take(number)

           /* val explicitInnerJoinDebateYesOrNo = for {
		    ((conversationMessage, member), membersInfo) <- Instantiations.Tables.conversationMessages.sortBy(_.modificationTimestamp.desc) leftJoin Instantiations.Tables.members on
		    ((conversationMessage, member) => conversationMessage.idConversation === conversationID && conversationMessage.modificationTimestamp < maxTimestampParam && conversationMessage.idSender === member.id) leftJoin Instantiations.Tables.membersInfo on (_._2.id === _.id)
		    } yield (conversationMessage, member.username, membersInfo.firstname, membersInfo.name)*/
           val explicitInnerJoinDebateYesOrNo = for {
		       ((membersInfo, member), conversationMessage) <- (Instantiations.Tables.membersInfo innerJoin Instantiations.Tables.members on ((memberInfo, member) => memberInfo.id === member.id) innerJoin  Instantiations.Tables.conversationMessages/*.sortBy(_.modificationTimestamp.desc)*/ on
		       ((tupleMember, conversationMessage) => conversationMessage.idConversation === conversationID && conversationMessage.modificationTimestamp < maxTimestampParam && conversationMessage.idSender === tupleMember._1.id)).sortBy(_._2.modificationTimestamp.desc)
		       } yield (conversationMessage, member.username, membersInfo.firstname, membersInfo.name)
		    
            val results = explicitInnerJoinDebateYesOrNo.take(number).list/*.sortBy(_._1._6.get.getTime())*/.par.map(x => convertConversationMessageDatabaseEntryToClass(x._1).copy(senderName = x._4, senderFirstname = x._3, senderUsername = x._2)).seq

            setConversationReadForUser(userIDInt, conversationID)

            implicit val wrtier = conversationMessageWrites
            Ok(Json.toJson(results))
          }
          else{
            BadRequest
          }
        }
      }
    }

    /*
     * Sets unread a conversation defined by its conversationID
     */
    def setConversationUnread(conversationID: Long) = Instantiations.Tables.db.withSession { implicit session =>
      Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      {  //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
        //We set all the messages of this conversation unread
    	  val q3 = for {c <- Instantiations.Tables.conversationMember if c.conversationId === conversationID} yield c.newMessages
    	  q3.update(true)
        }
      }
    }

    /*
     * Sets unread a conversation defined by its conversationID
     */
    def setConversationReadForUser(userID: Long, conversationID: Long) = Instantiations.Tables.db.withSession { implicit session =>
      Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      {  //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
        val userIDInt = userID.toLong
        //We set all the messages of this conversation unread
    	  val q3 = for {c <- Instantiations.Tables.conversationMember if c.conversationId === conversationID && c.memberId === userID} yield c.newMessages
    	  q3.update(false)
        }
      }
    }

  //Returns a view of the conversation
  def getConversation(conversationID: Long) = Connection.withConnection {
    username => userID => implicit request => implicit lang =>
      val conversationRetrieved = getConversationModel(conversationID)
      //if (conversationRetrieved != null){
        //Ok(views.html.connected.displayConversation("", conversationRetrieved))
      //}
      //else{
        Ok(views.html.connected.concept(Messages("views.convers.doesnt_exists")))
      //}
  }

  /*
   * Returns a model.Concept corresponding to the concept having the ID provided
   */
  def getConversationModel(conversationID: Long) = {
    Instantiations.Tables.db.withSession { implicit session =>
      val q2 = Instantiations.Tables.conversation.filter(_.id === conversationID)
      val requestExecuted = q2.list
      if (requestExecuted != Nil){
        convertConversationDatabaseEntryToClass(requestExecuted.head)
      }
      else{
        null
      }
    }
  }

}
