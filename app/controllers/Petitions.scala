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

import play.api.Play.current
import play.api.cache.Cache;

import org.jsoup._
import org.jsoup.safety.Whitelist

object Petitions extends Controller {
  val petitionWrites: Writes[models.Petition] = {
    ((JsPath \ "id").write[Long] and
    (JsPath \ "creatorType").write[Int] and
    (JsPath \ "creatorID").write[Long] and
    (JsPath \ "creatorName").write[String] and
    (JsPath \ "name").write[String] and
    (JsPath \ "definition").write[String] and
    (JsPath \ "timeEnd").write[Long] and
    (JsPath \ "numberJoined").write[Long] and
    (JsPath \ "language").write[String] and
    (JsPath \ "modificationTimestamp").write[Long])(unlift(models.Petition.unapply))
  }
  def convertPetitionDatabaseEntryToClass(databaseEntry: models.Types.petitionEntry) = {
  models.Petition(databaseEntry._1.get, databaseEntry._2, databaseEntry._3, "", Application.outputHTMLText(databaseEntry._4, false), Application.outputHTMLText(databaseEntry._5, true), databaseEntry._6, databaseEntry._7, databaseEntry._11, databaseEntry._8.getTime())
}
  def processPetitionCreationForm = Connection.withConnection({
      username => userID => implicit request => implicit lang =>      
        models.Forms.petitionCreationForm.bindFromRequest.value map { petitionCreationForm =>
          Instantiations.Tables.db.withSession { implicit session =>
            val idGroup = petitionCreationForm.idGroup.toLong
            val userIDInt = userID.toLong
            val q2 = for {c <- Instantiations.Tables.petition if c.name === petitionCreationForm.name} yield c.id
            if (q2.list == Nil
                &&
                petitionCreationForm.name.length < Application.maxCharacternames 
                &&
                Application.langs.contains(petitionCreationForm.language)
                //&& 
                //petitionCreationForm.timeEnd.toLong > (System.currentTimeMillis()-86400000) //Minus one day in order to handle all timezones
                &&
                petitionCreationForm.definition.length < Application.maxCharacterDefinitions){
              if (idGroup == -1){
                val tupleToAdd = (None, 0, userID.toLong, scala.xml.Utility.escape(petitionCreationForm.name), Jsoup.clean(petitionCreationForm.definition, Application.whiteListHTML), petitionCreationForm.timeEnd.toLong, 0L, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0, petitionCreationForm.language)
                //We have nothing to check, all the actions are OK if created by an user
                Instantiations.Tables.petition += tupleToAdd
                val createdPetition = q2.list.head
                
                //We follow this petition
                User.followThisObject(userIDInt, 5, createdPetition)
                
                ActionRepport.actionDone(userIDInt, 5, createdPetition, createdPetition, 50000, null)
                Ok(Json.toJson(createdPetition))
              }
              else{
                val idOfGroupCreator = petitionCreationForm.idGroup.toLong
                if (Groups.canThisUserCreateGroupPetitions(userID.toLong, idOfGroupCreator)){ //We verify that the user can create actions with this group
                  val tupleToAdd = (None, 1, idOfGroupCreator, scala.xml.Utility.escape(petitionCreationForm.name), Jsoup.clean(petitionCreationForm.definition, Application.whiteListHTML), petitionCreationForm.timeEnd.toLong, 0L, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0, request.session.get("language").get)
                  Instantiations.Tables.petition += tupleToAdd
                  val createdPetition = q2.list.head
                  //We follow this petition
                  User.followThisObject(userIDInt, 5, createdPetition)
                  
                  ActionRepport.actionDone(userIDInt, 1, idOfGroupCreator, createdPetition, 50001, null, idOfGroupCreator)
                  
                  Ok(Json.toJson(createdPetition))
                }
                else{
                  BadRequest
                }
              }
            }
            else{
              Ok(Json.toJson(-1))
            }
          }
        } getOrElse {
         BadRequest
       }
    }, None, true)
  
    /*
     * Gets the JSON of members in the group limited by numberOfGroups where the timestamp of last modification
     * is inferior to the one in parameter
     */
    def getMembersInPetitionOrderedbyTimestampJSON(petitionID: Long, maxTimestamp: Long, number: Int) = Connection.withConnection ({
    username => userID => implicit request => implicit lang =>
      if (canThisUserParticipateToThisPetition(petitionID, userID.toLong))
        getMembersInPetitionOrderedbyTimestampJSONsub(petitionID, maxTimestamp, number, userID.toLong)
      else
        BadRequest
    },  Option((request: Request[AnyContent]) => {
      if (canThisUserParticipateToThisPetition(petitionID, -1))
        getMembersInPetitionOrderedbyTimestampJSONsub(petitionID, maxTimestamp, number)
      else
        BadRequest
      })) 
  
  def getMembersInPetitionOrderedbyTimestampJSONsub(petitionID: Long, maxTimestamp: Long, number: Int, userID: Long = 0) = {
    val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
      if (number <= Application.maxResults){
        Instantiations.Tables.db.withSession { implicit session =>
          //We create a request to get all the memberInfo we want, the timestamps of joining the group, and the rank of the member in the group
          val explicitInnerJoin = for {
            ((petitionJoin, memberInfo), petition) <- 
    		    (Instantiations.Tables.petitionJoin innerJoin Instantiations.Tables.membersInfo on
    	  	  ((petitionJoin, memberInfo) => petitionJoin.petitionId === petitionID && petitionJoin.memberId === memberInfo.id)) leftJoin Instantiations.Tables.petition on ((tuplePetitionJoinMemberInfo, petition) => tuplePetitionJoinMemberInfo._1.petitionId === petition.id && petition.logicalDelete == 0)
    		  } yield (memberInfo, petitionJoin.modificationTimestamp)
		  
		      //For each of them, we check if the user is following them in order to directly display it in the json
    		  val explicitInnerJoin2 = User.addUserFollowingToMembersInfoRequest(explicitInnerJoin, userID)
		  
          //we get them and convert them into a models.userInfo.
          //We set the correct modification timestamp 
          val results = explicitInnerJoin2.list.map(x => User.convertMemberInfoDatabaseEntryToClass(x._1._1).copy(isUserFollowing = {if(x._2 == None){0}else{1}}, modificationTimestamp = x._1._2.getTime()))
          implicit val writer = User.userWrites
          
          Ok(Json.toJson(results))
        }
      }
      else{
        BadRequest
      }
  }
  
  /*
   * Sets the right creator name to the petition, so the group name if the creator is a group and the username if the creator is an user
   */
  def associateRightCreatorname(petition: models.Petition, username: Option[String], group: Option[String]) = {
    if (petition.creatorType == 0){//a member created it
      petition.copy(creatorName = username.get)
    }
    else{
      petition.copy(creatorName = group.get)
    }
  }
    
  /*
   * Retrieves the last 10 petitions created from a certain id
   */
  def getLastPetitionsOrderedbyTimestampJSON(groupPrivacy: Long, maxTimestamp: Long, number: Int) = Connection.withConnection({
    username => userID => implicit request => implicit lang => {
        val userIDLong = userID.toLong
        getLastPetitionsOrderedbyTimestampJSONsub(userIDLong, groupPrivacy, maxTimestamp, number)
    }
  }, Option((request: Request[AnyContent]) => getLastPetitionsOrderedbyTimestampJSONsub(-1, groupPrivacy, maxTimestamp, number)) )
  
  def getLastPetitionsOrderedbyTimestampJSONsub(userIDLong: Long, groupPrivacy: Long, maxTimestamp: Long, number: Int) = {
    val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        
      Instantiations.Tables.db.withSession { implicit session =>
        if (number <= controllers.Application.maxResults && number >= 0){
          var finalResults: slick.lifted.Query[(models.PetitionTable, slick.lifted.Column[Option[String]], slick.lifted.Column[Option[String]], slick.lifted.Column[Option[Int]], slick.lifted.Column[Option[Long]], slick.lifted.Column[Option[Long]]),(models.Types.petitionEntry, Option[String], Option[String], Option[Int], Option[Long], Option[Long]),Seq] = null
          
            val listOfPetitionsAndGroupMemberAssociated = for {
  		      ((petition, member), group) <- 
  	    	  ((Instantiations.Tables.petition innerJoin Instantiations.Tables.members on
  		        ((petition, member) => 
  		          petition.modificationTimestamp < maxTimestampParam && 
  		          petition.creatorID === member.id && 
  		          petition.logicalDelete === 0)) leftJoin Instantiations.Tables.groups on ((petitionMember, group) => petitionMember._1.creatorID === group.id && group.logicalDelete === 0))
            } yield (petition, member.username?, group.name?, group.privacy?, group.id?, member.id?)
           if (groupPrivacy == -1){
            //We display petitions created by private groups where the user is member
            val filterPrivatePetitionsByGroupsMember = for {
              (groupMember, petitionsAndGroupMemberAssociated) <- Instantiations.Tables.groupMembership.filter(_.memberId === userIDLong) innerJoin listOfPetitionsAndGroupMemberAssociated.filter(x => x._1.creatorType === 1 && x._4 === 1) on ((groupMember, petitionsAndGroupMemberAssociated) => 
                (groupMember.groupId === petitionsAndGroupMemberAssociated._5))
            } yield petitionsAndGroupMemberAssociated
            
            //We add to it all the public petitions (all not created by a private group)
            val filterAllPrivatePetitions = listOfPetitionsAndGroupMemberAssociated.filter(x => x._1.creatorType === 0 || (x._1.creatorType === 1 && x._4 === 0))
            //We union these two lists of petitions so we have all petitions that the user can view
            val filterPrivatePetitions = filterPrivatePetitionsByGroupsMember ++ filterAllPrivatePetitions
            finalResults = filterPrivatePetitions
          }
          else{
            //We want the petitions of a group
            val userStatusInGroup = Groups.userStatusInGroup(groupPrivacy, userIDLong, true)
            if (userStatusInGroup == 1 || userStatusInGroup == 3)
              finalResults = listOfPetitionsAndGroupMemberAssociated.filter(x => x._1.creatorType === 1 && x._1.creatorID === groupPrivacy)
            else
              finalResults = listOfPetitionsAndGroupMemberAssociated.filter(x => x._1.creatorType === -1) //We return nothing
          }
          val onlyShowMemberLanguagesInRequest = {
            if (groupPrivacy == -1)
              for {(request, memberLanguage) <- finalResults innerJoin User.requestForUserLanguages(userIDLong) on ((request, memberLanguage) => request._1.language === memberLanguage.language)} yield request
            else
              finalResults
          }
          val results = onlyShowMemberLanguagesInRequest.sortBy(_._1.modificationTimestamp.desc).take(number).list.par.map(x => associateRightCreatorname(convertPetitionDatabaseEntryToClass(x._1), x._2, x._3)).seq
           
          implicit val wrtier = petitionWrites
          Ok(Json.toJson(results))
        } 
        else {
          BadRequest
        }
    }
  }
  
  /*
   * Retrieves the last 10 actions created by the user from a certain id
   */
  def getLastPetitionsCreatedByUserOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          //val q2 = for {c <- Instantiations.Tables.petition.filter(x => (x.creatorType === 0 && x.creatorID === userIDInt && x.modificationTimestamp < maxTimestampParam)).sortBy(_.modificationTimestamp.desc).take(number)} yield c
          val explicitInnerJoinPetition = for {
		  (petition, member) <- Instantiations.Tables.petition.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.members on
		  ((petition, member) => petition.creatorType === 0 && petition.creatorID === userIDInt && petition.creatorID === member.id && petition.modificationTimestamp < maxTimestampParam && petition.logicalDelete === 0)
		  } yield (petition, member.username)
          
          val results = explicitInnerJoinPetition.list.map(x => convertPetitionDatabaseEntryToClass(x._1).copy(creatorName = x._2))
          implicit val wrtier = petitionWrites
          Ok(Json.toJson(results))
        } 
        else {
          BadRequest
        }
      }
    }
  }
  
  /*
	 * Returns if this user can participate to the petition
	 */
	def canThisUserParticipateToThisPetition(petitionID: Long, userID: Long): Boolean = {
	  Instantiations.Tables.db.withSession { implicit session =>
	      //First we get the petition to display
   	    val petitionToDisplay = for {c <- Instantiations.Tables.petition.filter(x => x.id === petitionID && x.logicalDelete === 0)} yield (c.creatorType, c.creatorID)
       
   	    val result = petitionToDisplay.list
   	    if (result == Nil){
   	      false
   	    }
   	    else if (result.head._1 == 0){ //Petition created by member : public
          true
        }
        else{
          val userStatusInGroup = Groups.userStatusInGroup(result.head._2, userID, true) //The user can access if it is a public group (3) or if he is in the group (1)
          if (result.head._1 == 1 && (userStatusInGroup == 1 || userStatusInGroup == 3)){
            true
          }
          else{
            false
          }
        }
	  }
	}
  
  /*
   * Returns the view of the action defined by actionID
   */
  def getPetition(petitionID: Long) = Connection.withConnection (
    username => userID => implicit request => implicit lang => {
      if (canThisUserParticipateToThisPetition(petitionID, userID.toLong)){
        val petitionRetrieved = getPetitionModel(petitionID)
        if (petitionRetrieved != null){
          Ok(views.html.connected.displayPetition("", petitionRetrieved))
        }
        else{
          Ok(views.html.connected.petition(Messages("views.petition_doesnt_exists")))
        }
      }
      else{
        Ok(views.html.connected.petition(Messages("views.petition_doesnt_exists")))
      }
    }, Option((request: Request[AnyContent]) => getPetitionUnconnected(petitionID, request)) )
  
  def getPetitionUnconnected(petitionID: Long, request: Request[AnyContent]) = {
    if (canThisUserParticipateToThisPetition(petitionID, -1)){
      val petitionRetrieved = getPetitionModel(petitionID)
      if (petitionRetrieved != null){
        Ok(views.html.connected.displayPetition("", petitionRetrieved, false)(request.session, Instantiations.getLang(request))).withSession("language" -> petitionRetrieved.language)
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
   * Boolean telling if an petiion exists
   */
  def petitionExists(petitionID: Long): Boolean = {
    Instantiations.Tables.db.withSession { implicit session =>
       Instantiations.Tables.petition.filter(x => x.id === petitionID && x.logicalDelete === 0).exists.run
    }
  }
  
  /*
   * Returns a model.Petition corresponding to the action define by the ID provided
   */
  def getPetitionModel(petitionID: Long) = {
    Instantiations.Tables.db.withSession { implicit session =>
      val valueFromCache = Cache.getAs[models.Petition]("petition" + petitionID)
      if (valueFromCache == None){
        //We get the petition and group and member associated
        val petitionsAndGroupMemberAssociated = for {
  		      ((petition, member), group) <- 
  	    	  (Instantiations.Tables.petition.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.members on
  		        ((petition, member) => 
  		          petition.id === petitionID && 
  		          petition.creatorID === member.id && 
  		          petition.logicalDelete === 0)) leftJoin Instantiations.Tables.groups on (_._1.creatorID === _.id)
            } yield (petition, member.username?, group.name?)
        
        val requestExecuted = petitionsAndGroupMemberAssociated.list
        if (requestExecuted != Nil){
          val valueToReturn = associateRightCreatorname(convertPetitionDatabaseEntryToClass(requestExecuted.head._1), requestExecuted.head._2, requestExecuted.head._3)
          Cache.set("petition" + petitionID, valueToReturn, Application.cacheDuraction)
          valueToReturn
        }
        else{
          Cache.set("petition" + petitionID, null, Application.cacheDuraction)
          null
        }
      }
      else{
        valueFromCache.get
      }
    }
  }
  
  /*
   * Tells if a member defined by memberID has joined an action defined by actionID
   */
  def isMemberInPetition(memberID: Long, petitionID: Long): Boolean = {
    Instantiations.Tables.db.withSession { implicit session =>
       Instantiations.Tables.petitionJoin.filter(x => x.petitionId === petitionID && x.memberId === memberID).exists.run
    }
  }
  
  
  /*
   * Makes the loged member joind the action defined by petitionID
   */
  def joinPetition() = Connection.withConnection ({
    username => userID => implicit request => implicit lang =>
      var returnedJson = 0
      Instantiations.Tables.db.withSession { implicit session =>
        val args = request.body.asFormUrlEncoded.get
        
        if (args != None && args.contains("petitionID")){
          
         // TODO : VERIFY IF ACTION EXISTS AND MEMBER IS NOT ALREADY
          val petitionID = args.get("petitionID").get(0).toLong
          val userIDInt = userID.toLong
          
          val petitionModel = getPetitionModel(petitionID)
          
          if (petitionModel != null && !isMemberInPetition(userIDInt, petitionID)){
            Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
              Instantiations.Tables.petitionJoin += (userIDInt, petitionID, 0, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())))
              sqlu"update `PETITION` set NBREJOINED=NBREJOINED+1 where PETITION.SUP_ID = $petitionID".first
              }
            }
            //We follow this petition
            User.followThisObject(userIDInt, 5, petitionID)
            
            //We set the privacy when we register the activity
            if (petitionModel.creatorType == 1){
              ActionRepport.actionDone(userIDInt, 5, petitionID, petitionID, 50002, null, petitionModel.creatorID)
            }
            else{
              ActionRepport.actionDone(userIDInt, 5, petitionID, petitionID, 50002, null, -1)
            }
            
            returnedJson = 1
          }
        }
      }
      Ok(Json.toJson(returnedJson))
  }, None, true)
  
  def leavePetition() = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
      var returnedJson = 0
      Instantiations.Tables.db.withSession { implicit session =>
        val args = request.body.asFormUrlEncoded.get
        if (args != None && args.contains("petitionID")){
          val petitionID = args.get("petitionID").get(0).toLong
          val userIDInt = userID.toLong
          val petitionModel = getPetitionModel(petitionID)
          val q = Instantiations.Tables.petitionJoin.filter(x => (x.memberId === userIDInt && x.petitionId === petitionID)).delete
            if (petitionModel != null && q == 1){
              Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
                sqlu"update `PETITION` set NBREJOINED=NBREJOINED-1 where PETITION.SUP_ID = $petitionID".first
                }
              }
              returnedJson = 1
              //We set the privacy when we register the activity
              if (petitionModel.creatorType == 1){
                ActionRepport.actionDone(userIDInt, 5, petitionID, petitionID, 50003, null, petitionModel.creatorID)
              }
              else{
                ActionRepport.actionDone(userIDInt, 5, petitionID, petitionID, 50003, null, -1)
              }
              
              
              //We unfollow this petition (the user don't know that petition can be followed, it is for this action to appear in its news feed)
              User.unfollowThisObject(userIDInt, 5, petitionID)
              
            }
        }
      }
    Ok(Json.toJson(returnedJson))
  }, None, true)
  
  /*
   * Retrieves the last 10 petitions that the user joined
   */
  def getLastPetitionsJoinedOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val explicitInnerJoin = for {
		  (petitionJoin, petition) <- Instantiations.Tables.petitionJoin.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.petition on
		  ((petitionJoin, petition) => petitionJoin.memberId === userIDInt && petitionJoin.petitionId === petition.id && petitionJoin.modificationTimestamp < maxTimestampParam && petition.logicalDelete === 0)
		  } yield (petition, petitionJoin.modificationTimestamp)
          val results = explicitInnerJoin.take(number).list.map(x => convertPetitionDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime()))
          implicit val wrtier = petitionWrites
          Ok(Json.toJson(results))
        } 
        else {
          BadRequest
        }
      }
    }
  }
  
  /*
   * Deletes a petition defined by the "key" POSTDATA. Returns 1 if the comment has been deleted, and 0 otherwise
   */
  def processPetitionDelete  = Connection.withConnection({
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
          var deleteRestOfActivities = false
          val creatorTypeAndIdList = (for { c <- Instantiations.Tables.petition if (c.id === args.get("key").get(0).toLong) } yield (c.creatorType, c.creatorID)).list
          if (creatorTypeAndIdList != Nil && creatorTypeAndIdList.head._1 == 1){
            //The creator is a group
            if (Groups.canThisUserDeleteGroupContent(userIDLong, creatorTypeAndIdList.head._2)){
              val q2 = for { c <- Instantiations.Tables.petition if (c.id === args.get("key").get(0).toLong) } yield c.logicalDelete
              q2.update(1) //We set LOGICALDELETE to 1
              val statement = q2.updateStatement
              val invoker = q2.updateInvoker
              
              //We then delete the activities specific to a petition creation for group
              val q5 = for { c <- Instantiations.Tables.activity if (c.activityType === 50001 && c.activityTarget === args.get("key").get(0).toLong) } yield c.logicalDelete
              q5.update(1) //We set LOGICALDELETE to 1
              val statement5 = q5.updateStatement
              val invoker5 = q5.updateInvoker
              
              jsonToSend = 1
              deleteRestOfActivities = true
            }
          }
          else{
            val q2 = for { c <- Instantiations.Tables.petition if (c.id === args.get("key").get(0).toLong && c.creatorID === userIDLong) } yield c.logicalDelete
            q2.update(1) //We set LOGICALDELETE to 1
            val statement = q2.updateStatement
            val invoker = q2.updateInvoker
            
            //We then delete the activities specific to a petition creation for member
            val q5 = for { c <- Instantiations.Tables.activity if (c.activityType === 50000 && c.activityObject === args.get("key").get(0).toLong && c.memberId === userIDLong) } yield c.logicalDelete
            q5.update(1) //We set LOGICALDELETE to 1
            val statement5 = q5.updateStatement
            val invoker5 = q5.updateInvoker
              
            jsonToSend = 1
            deleteRestOfActivities = true
          }
          
          if (deleteRestOfActivities){
            //We then delete all the joinAction and leaveAction activities
            val q2 = for { c <- Instantiations.Tables.activity if ((c.activityType === 50002 || c.activityType === 50003) && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
            q2.update(1) //We set LOGICALDELETE to 1
            val statement2 = q2.updateStatement
            val invoker2 = q2.updateInvoker
            
            //Delete CommentPetition
            val q6 = for { c <- Instantiations.Tables.activity if (c.activityType === 60005 && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
            q6.update(1) //We set LOGICALDELETE to 1
            val statement6 = q6.updateStatement
            val invoker6 = q6.updateInvoker
            
            //We delete from cache 
            Cache.remove("petition" + args.get("key").get(0).toLong)
            
          }
        }
      }
    }
    Ok(Json.toJson(jsonToSend))
  }, None, true)
  
}