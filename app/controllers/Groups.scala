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

object Groups extends Controller {

    def convertGroupDatabaseEntryToClass(databaseEntry: models.Types.groupEntry) = {
      models.Group(databaseEntry._1.get, Application.outputHTMLText(databaseEntry._2, false), Application.outputHTMLText(databaseEntry._3, true), databaseEntry._4, databaseEntry._5, databaseEntry._9, databaseEntry._6.getTime())
    }
    val groupWrites: Writes[models.Group] = {
      //Concept(name: String, definition: String, idCreator:Int, membersFor: Int, membersAgainst: Int)
      ((JsPath \ "id").write[Long] and
      (JsPath \ "name").write[String] and
      (JsPath \ "definition").write[String] and
      (JsPath \ "numberMembers").write[Int] and
      (JsPath \ "privacy").write[Int] and
      (JsPath \ "language").write[String] and
      (JsPath \ "modificationTimestamp").write[Long])(unlift(models.Group.unapply))
    }

    val groupMembershipWrites: Writes[models.GroupMembership] = {
      ((JsPath \ "memberId").write[Long] and
      (JsPath \ "groupID").write[Long] and
      (JsPath \ "rank").write[Int] and
      (JsPath \ "modificationTimestamp").write[Long])(unlift(models.GroupMembership.unapply))
    }


   /*
   * Retrieves the json of the  last x groups created before some timestamp
   */
  def getLastGroupsOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang =>{
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDLong = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val q2 = for {c <- Instantiations.Tables.groups.filter(x => (x.modificationTimestamp < maxTimestampParam && x.logicalDelete === 0)).sortBy(_.modificationTimestamp.desc).take(number)} yield c
          
          val onlyShowMemberLanguagesInRequest =  for {(request, memberLanguage) <- q2 innerJoin User.requestForUserLanguages(userIDLong) on ((request, memberLanguage) => request.language === memberLanguage.language)} yield request
          
          val results = onlyShowMemberLanguagesInRequest.list.par.map(x => convertGroupDatabaseEntryToClass(x)).seq
          implicit val wrtier = groupWrites
          Ok(Json.toJson(results))
        }
        else{
          BadRequest
        }
      }
    }
  }
  
  /*
   * Retrieves the last n groups whose user joined
   */
  def getLastGroupsJoinedOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val explicitInnerJoin = for {
		  (groupMembership, group) <- Instantiations.Tables.groupMembership.filter(x => x.memberId === userIDInt && x.modificationTimestamp < maxTimestampParam).sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.groups.filter(_.logicalDelete === 0) on
		  ((groupMembership, group) => 
		    groupMembership.groupId === group.id
		    )
		  } yield (group, groupMembership.modificationTimestamp)
          val results = explicitInnerJoin.take(number).list.par.map(x => convertGroupDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime())).seq
          implicit val wrtier = groupWrites
          Ok(Json.toJson(results))
        } 
        else {
          BadRequest
        }
      }
    }
  }
  
  /*
   * Retrieves the last n groups whose user joined
   */
  def getLastGroupsCandidatedOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val explicitInnerJoin = for {
		  (groupApplication, group) <- Instantiations.Tables.groupApplication.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.groups on
		  ((groupApplication, group) => 
		    groupApplication.idMember === userIDInt && 
		    groupApplication.idGroup === group.id && 
		    groupApplication.modificationTimestamp < maxTimestampParam && 
		    group.logicalDelete === 0)
		  } yield (group, groupApplication.modificationTimestamp)
          val results = explicitInnerJoin.take(number).list.par.map(x => convertGroupDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime())).seq
          implicit val wrtier = groupWrites
          Ok(Json.toJson(results))
        } 
        else {
          BadRequest
        }
      }
    }
  }
  
  /*
   * Retrieves the last 10 groups whose user joined
   */
  def getLastGroupsWhereUsedhaveThisRankOrderedbyTimestampJSON(rank: Int, maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val explicitInnerJoin = for {
		  (groupMembership, group) <- Instantiations.Tables.groupMembership.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.groups on
		  ((groupMembership, group) => 
		    groupMembership.memberId === userIDInt && 
		    groupMembership.groupId === group.id && 
		    groupMembership.roleGroup === rank &&
		    groupMembership.modificationTimestamp < maxTimestampParam && 
		    group.logicalDelete === 0)
		  } yield (group, groupMembership.modificationTimestamp)
          val results = explicitInnerJoin.take(number).list.par.map(x => convertGroupDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime())).seq
          implicit val wrtier = groupWrites
          Ok(Json.toJson(results))
        } 
        else {
          BadRequest
        }
      }
    }
  }


  /*
   * Returns if an user can approve group applications in a group
   */
  def canThisUserApproveApplications(userID: Long, groupID: Long): Boolean = {
    Application.groupRanksThatCanApproveMembersApplication.contains(userRankInGroup(groupID, userID))
  }
  /*
   * Returns if an user can change ranks of the members in a group
   */
  def canThisUserChangeRanks(userID: Long, groupID: Long): Boolean = {
    Application.groupRanksThatCanChangeMemberRank.contains(userRankInGroup(groupID, userID))
  }

  /*
   * Returns if an user create group private debates
   */
  def canThisUserCreateGroupPrivateDebates(userID: Long, groupID: Long): Boolean = {
    Application.groupRanksThatCanCreatePrivateDebates.contains(userRankInGroup(groupID, userID))
  }

  /*
   * Returns if an user create change group definition
   */
  def canThisUserChangeGroupDefinition(userID: Long, groupID: Long): Boolean = {
    Application.groupRanksThatCanChangeDefinition.contains(userRankInGroup(groupID, userID))
  }

  /*
   * Returns if an user create change group name
   */
  def canThisUserChangeGroupName(userID: Long, groupID: Long): Boolean = {
    Application.groupRanksThatCanChangeName.contains(userRankInGroup(groupID, userID))
  }

  /*
   * Returns if an user create group actions
   */
  def canThisUserCreateGroupActions(userID: Long, groupID: Long): Boolean = {
    Application.groupRanksThatCanCreateActions.contains(userRankInGroup(groupID, userID))
  }

  /*
   * Returns if an user can create group petitions
   */
  def canThisUserCreateGroupPetitions(userID: Long, groupID: Long): Boolean = {
    Application.groupRanksThatCanCreatePetitions.contains(userRankInGroup(groupID, userID))
  }
  
  /*
   * Returns if an user can delete group
   */
  def canThisUserDeleteGroup(userID: Long, groupID: Long): Boolean = {
    Application.groupRanksThatCanDeleteGroup.contains(userRankInGroup(groupID, userID))
  }
  
  /*
   * Returns if an user can delete group debates/petitions/actions
   */
  def canThisUserDeleteGroupContent(userID: Long, groupID: Long): Boolean = {
    Application.groupRanksThatCanDeleteGroupContent.contains(userRankInGroup(groupID, userID))
  }

  /*
	 * Returns if this user can comment to the group
	 */
	def canThisUserParticipateToThisGroup(groupID: Long, userID: Long): Boolean = {
	  Instantiations.Tables.db.withSession { implicit session =>
	      //First we get the privacy of the group
   	    val userStatusInGroup = Groups.userStatusInGroup(groupID, userID, true) 
        if (userStatusInGroup == 1 || userStatusInGroup == 3){
          true
        }
        else{
          false
        }
	  }
	}
  

  /*
   * Returns if a rank exists
   */
  def doesThisRankExists(rank: Int): Boolean = {
    Application.possibleGroupRanks.contains(rank)
  }

  
  
  /*
   * Returns the members waiting for an approbation
   */
    def getGroupApplicationsOrderedByTimestamJSON(groupId: Long, maxTimestamp: Long, number: Int) = Connection.withConnection {
      username => userID => implicit request => implicit lang =>{
        //We check if the user has the right to see this
        if (canThisUserApproveApplications(userID.toLong, groupId)){
          Instantiations.Tables.db.withSession { implicit session =>
            val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))

            //We extract all the membersIds waiting for approbation
            val q2 = for {c <- Instantiations.Tables.groupApplication.filter(x => (x.modificationTimestamp < maxTimestampParam && x.idGroup === groupId )).sortBy(_.modificationTimestamp.desc).take(number)} yield (c.idMember, c.modificationTimestamp)

            //We get their models.MemberInfo
            val results = q2.list.map(x => User.getMemberInfoFromID(x._1).copy(modificationTimestamp = x._2.getTime()))

            //And we send them in JSON format
            implicit val wrtier = User.userWrites
            Ok(Json.toJson(results))
          }
        }
        else{
          BadRequest
        }
      }
    }

    /*
     * Accepts a group application with POST parameter the id of the member and the decision (0 t reject and 1 to accept)
     */
    def decideGroupApplication(groupID: Long) = Connection.withConnection ({
      username => userID => implicit request => implicit lang =>{
        val args = request.body.asFormUrlEncoded.get
        if (args.contains("memberID") && args.contains("decision")){
          val memberID = args.get("memberID").get(0).toLong
          val decision = args.get("decision").get(0).toInt

          val userIDInt = userID.toLong

          if (isWaintingForApprobation(groupID, memberID) && canThisUserApproveApplications(userID.toLong, groupID)){
            //The user can approve members and the member is waiting for approbation in the group, we thus accept it
            decision match {
              case 1 =>
              //We delete the application from the application table
              Instantiations.Tables.db.withSession { implicit session =>
                Instantiations.Tables.groupApplication.filter(x => (x.idMember === memberID && x.idGroup === groupID)).delete
              }

              //And we add the member to the group
              addUserToGroup(memberID, groupID, 1)
              ActionRepport.actionDone(memberID, 1, groupID, memberID, 10001, null) //Activity : the member was accepted
              ActionRepport.actionDone(userIDInt, 1, groupID, memberID, 10005, null) //Activity : user accepted this member
              Ok

            case 0 =>
              //We just delete the application from the application table
              Instantiations.Tables.db.withSession { implicit session =>
                Instantiations.Tables.groupApplication.filter(x => (x.idMember === memberID && x.idGroup === groupID)).delete
              }
              Ok

            case _ =>
              BadRequest
            }
          }
          else{
            BadRequest
          }
        }
        else{
          BadRequest
        }
      }
    }, None, true)

    /*
     * Changes the rank of a member if the user has the correct privileges.
     * Takes in parameter the groupID and has to be loaded with POST parameters memberID
     * (that specifies the id of the member to change) and newrank (that specifies the new rank to add)
     * Returns JSON:
     * 1 if the change succeeded
     * 0 if it failed
     */
    def changeMemberRank(groupID: Long) = Connection.withConnection ({
      username => userID => implicit request => implicit lang =>{
        val args = request.body.asFormUrlEncoded.get
        if (args.contains("memberID") && args.contains("newrank")){
          val userIDInt = userID.toLong
          val memberIDInt = args.get("memberID").get(0).toLong
          val newRank = Integer.parseInt(args.get("newrank").get(0))

          /*We verify that this rank exists, that the member is in the group,
          and that the administrator has the correct privileges*/
          val isMemberInGroup = (userStatusInGroup(groupID, memberIDInt) == 1)
          val isThisUserAllowedToChangeRanks = canThisUserChangeRanks(userIDInt, groupID)
          val rankExists = doesThisRankExists(newRank)

          //We also verify that the user is not changing his own rank
          val isChangingHisOwnRank = (userIDInt == memberIDInt)

          if (isMemberInGroup && isThisUserAllowedToChangeRanks && rankExists && !isChangingHisOwnRank){
            Instantiations.Tables.db.withSession { implicit session =>
              Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
                //The verifications are ok, we can change the rank of the member in the group
                val q2 = for { c <- Instantiations.Tables.groupMembership if (c.memberId === memberIDInt && c.groupId === groupID) } yield c.roleGroup
                q2.update(newRank)
                val statement = q2.updateStatement
                val invoker = q2.updateInvoker
                }
              }
              //Instantiations.Tables.activity += ((None, userId, idOpinion, 30004, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())))) //We add it t the activity did by the member to the activity table in order to display it easily later
              ActionRepport.actionDone(userIDInt, 1, groupID, memberIDInt, 10002, "" + newRank)

            }
          }
        }
        Ok
      }
    }, None, true)

    /*
     * Process the models.Forms.groupCreationForm form to create a new group
     */
    def processGroupCreationForm = Connection.withConnection({
        username => userID => implicit request => implicit lang =>
          models.Forms.groupCreationForm.bindFromRequest.value map { groupCreationForm =>
            Instantiations.Tables.db.withSession { implicit session =>
              val q2 = for {c <- Instantiations.Tables.groups if (c.name === groupCreationForm.name)} yield c.id
              val privacyOfGroup = Integer.parseInt(groupCreationForm.privacy)
              if (
                  groupCreationForm.name.length < Application.maxCharacternames
                  &&
                  groupCreationForm.definition.length < Application.maxCharacterDefinitions
                  &&
                  q2.list == Nil
                  &&
                  (privacyOfGroup >= 0 && privacyOfGroup <= 1)){
                Instantiations.Tables.groups += (None, scala.xml.Utility.escape(groupCreationForm.name), Jsoup.clean(groupCreationForm.definition, Application.whiteListHTML), 0, privacyOfGroup, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0, request.session.get("language").get)
                //We then get the groupID
                val groupID = q2.list.head
                val userIDInt = userID.toLong
                
                //We add the creator of the group to the group with admin rank
                addUserToGroup(userIDInt, groupID, 1000)
                
                //We follow this group
                User.followThisObject(userIDInt, 1, groupID)
                
                ActionRepport.actionDone(userIDInt, 1, groupID, groupID, 10000, null)

                Ok(Json.toJson(groupID))

              }
              else{
                Ok(Json.toJson(Messages("views.group_already_exists")))
              }
            }
          } getOrElse {
            BadRequest
          }
      }, None, true)

    /*
     * This controller adds the user currently logged in to the group specified by the POST DATA "groupID"
     * Returns : 0 if the insertion failed - 1 if the insertion succeeded - 2 if we are waiting for an approbation
     *
     */
    def joinGroup() = Connection.withConnection ({
      username => userID => implicit request => implicit lang =>
      //We first get the posted group ID
      val args = request.body.asFormUrlEncoded.get
      var returnedJson = 0
      if (args != None && args.contains("groupID")){
        val groupID = Integer.parseInt(args.get("groupID").get(0))
        val userIDInt = userID.toLong

          if (groupExists(groupID) && userStatusInGroup(groupID, userIDInt) == 0){
            //We get the privacy of the group
            val privacy = getPrivacy(groupID)
            privacy match {
              case 0 => //The group is public : we add the member
                addUserToGroup(userIDInt, groupID, 1)
                
                //We follow this group
                User.followThisObject(userIDInt, 1, groupID)
                
                ActionRepport.actionDone(userIDInt, 1, groupID, userIDInt, 10001, null)
                returnedJson = 1
              case 1 => //The group is not public : we add it to the approbation list of this group if he is not wainting

              if (!isWaintingForApprobation(groupID, userIDInt)){
                Instantiations.Tables.db.withSession { implicit session =>
                  Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                    Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
                    //We add the user to the group application table
                    Instantiations.Tables.groupApplication += (None, userIDInt, groupID, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())))
                  }
                  }
                  //We follow this group
                  User.followThisObject(userIDInt, 1, groupID)
                  
                  returnedJson = 2
                }
              }
            case _ =>
          }
        }
      }
      Ok(Json.toJson(returnedJson))
    }, None, true)

    /*
     * Adds an user to a group (without verification)
     */
    def addUserToGroup(userIDInt: Long, groupID: Long, rank: Int){
      Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      { 
          sqlu"update `GROUP` set NBREMEMBERS=NBREMEMBERS+1 where GROUP.SUP_ID = $groupID".first
          val isInserted = Instantiations.Tables.groupMembership.insert(userIDInt, groupID, rank, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())))
        }
      }
    }

    /*
     * This controller deletes the user currently logged in to the group specified by the POST DATA "groupID"
     * Returns
     * 1 if the leaving of the group succeeded,
     * 0 if it failed
     * 2 if the application cancelation succeeded (the member wasn't accepted yet in the group)
     * 3 if it failed because the user is the only administrator
     */
    def leaveGroup() = Connection.withConnection ({
      username => userID => implicit request => implicit lang =>
      val args = request.body.asFormUrlEncoded.get
      var returnedJson = 0
      if (args != None && args.contains("groupID")){
        val groupID = args.get("groupID").get(0).toLong
        val userIDInt = userID.toLong
        Instantiations.Tables.db.withSession { implicit session =>
          if (groupExists(groupID)){
            //We check if the user is in the group
            val userStatusInGroupInt = userStatusInGroup(groupID, userID.toLong)
            if (userStatusInGroupInt == 1){//We check that the user is not waiting for his application to be processed
              //We check if the user is not the only administrator
              if (!(userRankInGroup(groupID, userIDInt) == 1000 && getNbreMembersWithRank(groupID, 1000) == 1)){
                //The member is in the group, we make him leave it
                val userIDInt = userID.toLong
                Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                  Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
                    val q = Instantiations.Tables.groupMembership.filter(x => (x.memberId === userIDInt && x.groupId === groupID)).delete
                    if (q == 1)
                      sqlu"update `GROUP` set NBREMEMBERS=NBREMEMBERS-1 where GROUP.SUP_ID = $groupID".first
                  }
                }
                returnedJson = 1
                
                //We unfollow this group (the user don't know that groups can be followed, it is for this action to appear in its news feed)
                User.unfollowThisObject(userIDInt, 1, groupID)
                
                ActionRepport.actionDone(userIDInt, 1, groupID, userIDInt, 10010, null)

              }
              else{
                returnedJson = 3
              }
            }
            else if (userStatusInGroupInt == 2){
              Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
                  //The member is just waiting for his application to be approved, we make him cancel his application
                  val q = Instantiations.Tables.groupApplication.filter(x => (x.idMember === userIDInt && x.idGroup === groupID)).delete
                }
              }
              returnedJson = 2

            }
          }
        }
      }
      Ok(Json.toJson(returnedJson))
    }, None, true)
    /*
     * Displays the HTML page of a group corresponding to the groupID
     */
    def getGroup(groupID: Long) = Connection.withConnection (
    username => userID => implicit request => implicit lang => {
      val groupRetrieved = getGroupModel(groupID)
      if (groupRetrieved != null){
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
        ActionRepport.addView(1, groupID)
        Ok(views.html.connected.displayGroup("", groupRetrieved))
      }
      else{
        Ok(views.html.connected.group(Messages("views.group_doesnt_exists")))
      }
    }, Option((request: Request[AnyContent]) => getGroupUnconnected(groupID, request)))
    
    def getGroupUnconnected(groupID: Long, request: Request[AnyContent]) = {
      val groupRetrieved = getGroupModel(groupID)
      if (groupRetrieved != null){
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
        ActionRepport.addView(1, groupID)
        Ok(views.html.connected.displayGroup("", groupRetrieved, false)(request.session, Instantiations.getLang(request))).withSession("language" -> groupRetrieved.language)
      }
      else{
        Ok(views.html.index("")(Instantiations.getLang(request)))
      }
   }
    
    /*
     * Retrieves the model.group for this groupID.
     */
    def getGroupModel(groupID: Long): models.Group = {
      Instantiations.Tables.db.withSession { implicit session =>
        val valueFromCache = Cache.getAs[models.Group]("group" + groupID)
        if (valueFromCache == None){
          val q2 = Instantiations.Tables.groups.filter(x => x.id === groupID && x.logicalDelete === 0)
          val requestExecuted = q2.list
          if (requestExecuted != Nil){
            
             val valueToReturn = convertGroupDatabaseEntryToClass(requestExecuted.head)
             Cache.set("group" + groupID, valueToReturn, Application.cacheDuraction)
             valueToReturn
          }
          else{
            Cache.set("group" + groupID, null, Application.cacheDuraction)
            null
          }
        }
        else{
          valueFromCache.get
        }
      }
    }

    /*
     * Returns a boolean indicating if a group defined by his groupID exists
     */
    def groupExists(groupID: Long):Boolean = {
      Instantiations.Tables.db.withSession { implicit session =>
        val q2 = Instantiations.Tables.groups.filter(x => x.id === groupID && x.logicalDelete === 0)
        val requestExecuted = q2.list
        if (requestExecuted != Nil){
          true
        }
        else{
          false
        }
      }
    }

    /*
     * Returns the privacy of a group
     */
    def getPrivacy(groupID: Long): Int = {
      Instantiations.Tables.db.withSession { implicit session =>
        val q = (for { c <- Instantiations.Tables.groups if (c.id === groupID) } yield c.privacy)
        q.list.head
      }
    }

    /*
     * Returns if an user defined by his userId is waiting for approbation in a group defined by his groupID
     */
    def isWaintingForApprobation(groupID: Long, userId: Long): Boolean ={
      Instantiations.Tables.db.withSession { implicit session =>
        val q2 = for { c <- Instantiations.Tables.groupApplication if (c.idGroup === groupID && c.idMember === userId) } yield c.idMember
        return q2.list != Nil
      }
    }


    /*
     * Checks the status of an user in a group
     * 0 : The user is not in the group and the group is private
     * 1 : The user is in the group
     * 2 : The user is waiting for an administrator to accept his group application request
     * 3 : The user is not in the group and the group is public and the optional checkIfPublicGroup isset  to true
     */
    def userStatusInGroup(groupID: Long, userId: Long, checkIfPublicGroup: Boolean = false): Int ={
      Instantiations.Tables.db.withSession { implicit session =>
        val q2 = for { (group, groupMembership) <- Instantiations.Tables.groups.filter(x => x.id === groupID && x.logicalDelete === 0) leftJoin Instantiations.Tables.groupMembership on
          ((group, groupMembership) => groupMembership.memberId === userId && groupMembership.groupId === group.id) } yield (group.privacy, groupMembership.memberId?)
        val result = q2.list
        if (result == Nil){
          0
        }
        else if (result.head._2 != None){
          1
        }
        else if (isWaintingForApprobation(groupID, userId)){
          2
        }
        else if (checkIfPublicGroup && result.head._1 == 0){
          3
        }
        else{
          0
        }
      }
    }
    
    
    /*
     * Sends the Json of the userStatusInGroup function
     */
    def userStatusInGroupJSON(groupID: Long) = Connection.withConnection {
    username => userID => implicit request => implicit lang => 
      Ok(Json.toJson(userStatusInGroup(groupID, userID.toLong)))
    }
    
    /*
     * Gets the rank of the user in the group. Returns -1 if the user is not in the group
     */
    def userRankInGroup(groupID: Long, userId: Long): Int = {
      Instantiations.Tables.db.withSession { implicit session =>
        //We search also for the groups logical deleted, in a case a user wants to delete action, petitions or debates of a group
        val q2 = for {  (group, groupMembership) <- Instantiations.Tables.groups innerJoin Instantiations.Tables.groupMembership on
          ((group, groupMembership) => groupMembership.groupId === groupID && groupMembership.memberId === userId && groupMembership.groupId === group.id) } yield groupMembership.roleGroup
        val results = q2.list
        if (results != Nil){
          results.head
        }
        else{
          -1
        }
      }
    }


    /*
     * Gets the number of members with a rank defined by rank in the group defined by groupID
     */
    def getNbreMembersWithRank(groupID: Long, rank: Int): Int = {
      var cnt: Int = 0;
      Instantiations.Tables.db.withSession { implicit session =>
	    val q = (for { c <- Instantiations.Tables.groupMembership if (c.groupId === groupID && c.roleGroup === rank) } yield c.memberId).length
        cnt= q.run
      }
      cnt
    }


    /*
     * Gets the JSON of members in the group limited by numberOfGroups where the timestamp of last modification
     * is inferior to the one in parameter
     */
    def getMembersInGroupOrderedbyTimestampJSON(groupID: Long, maxTimestamp: Long, numberOfGroups: Int) = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
      if (canThisUserParticipateToThisGroup(groupID, userID.toLong))
        getMembersInGroupOrderedbyTimestampJSONsub(groupID, maxTimestamp, numberOfGroups, userID.toLong)
      else
        BadRequest
    }, Option((request: Request[AnyContent]) => {
      Instantiations.Tables.db.withSession { implicit session =>
      //ONly public groups have their member list public
      val groupRequest = for {c <- Instantiations.Tables.groups.filter(x => x.id === groupID && x.logicalDelete === 0)} yield (c.privacy)
      val groupPrivacy = groupRequest.list
      if (groupPrivacy != Nil && groupPrivacy == 0)
        getMembersInGroupOrderedbyTimestampJSONsub(groupID, maxTimestamp, numberOfGroups)
      else
        BadRequest
      }}))

     /*
      * We create a request to get all the memberInfo we want, 
      * the timestamps of joining the group, and the rank of the member in the group
      */

   def getMembersInGroupOrderedByTimestampRequest(groupID: Long, maxTimestamp: Long) = {
      val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
      val explicitInnerJoin = for {
		  (groupMember, memberInfo) <- Instantiations.Tables.groupMembership innerJoin Instantiations.Tables.membersInfo on
		  ((groupMember, memberInfo) => groupMember.groupId === groupID 
		      && groupMember.memberId === memberInfo.id 
		      && groupMember.modificationTimestamp < maxTimestampParam)
		  } yield (memberInfo, groupMember.modificationTimestamp, groupMember.roleGroup)
		  explicitInnerJoin
    }
      
   def getMembersInGroupOrderedbyTimestampJSONsub(groupID: Long, maxTimestamp: Long, numberOfGroups: Int, userID: Long = -1) = {
     
      if (numberOfGroups <= Application.maxResults){
        Instantiations.Tables.db.withSession { implicit session =>
          //We get the members in the Group
           val membersInGroupRequest = getMembersInGroupOrderedByTimestampRequest(groupID, maxTimestamp)
           
           //For each of them, we check if the user is following them in order to directly display it in the json
    		   val explicitInnerJoin2 = for {
      	   (membersInGroup, follow) <- membersInGroupRequest leftJoin Instantiations.Tables.follow.filter(x => x.memberId === userID.toLong && x.objectFollowedType === 0) on
      	   ((membersInGroup, follow) => membersInGroup._1.id === follow.objectFollowedID)
      	  } yield (membersInGroup, follow.strenghOfLink?)
          
            //we get them and convert them into a models.userInfo.We set the correct modification timestamp and the correct rank
            val results = explicitInnerJoin2.sortBy(_._2).take(numberOfGroups).list.par.map(
                x => User.convertMemberInfoDatabaseEntryToClass(x._1._1).copy(rank = x._1._3, isUserFollowing = {if(x._2 == None){0}else{1}}, modificationTimestamp = x._1._2.getTime())
                ).seq

            implicit val writer = User.userWrites
            Ok(Json.toJson(results))
        }
      }
      else{
        BadRequest
      }
   }
    
   def getActionsAndPetitionsOfGroupOrderedByTimestampJSON(idGroup: Long, maxTimestamp: Long, number: Int) = Connection.withConnection({
     username => userID => implicit request => implicit lang =>
       getActionsAndPetitionsOfGroupOrderedByTimestampJSONsub(idGroup, maxTimestamp, number, userID);
   }, Option((request: Request[AnyContent]) => getActionsAndPetitionsOfGroupOrderedByTimestampJSONsub(idGroup, maxTimestamp, number, "-1")))
   
   def getActionsAndPetitionsOfGroupOrderedByTimestampJSONsub(idGroup: Long, maxTimestamp: Long, number: Int, userID: String) = {
     Instantiations.Tables.db.withSession {
       implicit session =>
         val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
         
         implicit val petitionWrites = Petitions.petitionWrites
         implicit val actionWrites = Actions.actionWrites
         val listOfPetitionsAndGroupMemberAssociated = Instantiations.Tables.petition.filter(x => x.creatorType === 1 && x.creatorID === idGroup && x.modificationTimestamp < maxTimestampParam)
         val listOfPetitions = Json.toJson(listOfPetitionsAndGroupMemberAssociated.sortBy(_.modificationTimestamp.desc).take(number).list.map(x => Petitions.convertPetitionDatabaseEntryToClass(x)))
         
         val listOfActionsAndGroupMemberAssociated = Instantiations.Tables.actions.filter(x => x.creatorType === 1 && x.creatorID === idGroup && x.modificationTimestamp < maxTimestampParam)
         val listOfActions = Json.toJson(listOfActionsAndGroupMemberAssociated.sortBy(_.modificationTimestamp.desc).take(number).list.map(x => Actions.convertActionDatabaseEntryToClass(x)))
         val emptyArray = Json.arr()
         val filledArray = ((emptyArray :+ listOfPetitions :+ listOfActions) apply 0).as[JsArray]
         val sortedAndFilteredActionsAndPetitions = JsArray((filledArray.value.sortBy(_ \\ "modificationTimestamp" map{_ match { case JsNumber(s) =>  s.longValue() }} head)).slice(0, number))//map(_.as[Long])
         Ok(sortedAndFilteredActionsAndPetitions)
     }
   }
    
   /*
   * Modifies description of group if the user can.
   */
  def processGroupDefinitionModification(groupID: Long) = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
      Instantiations.Tables.db.withSession {
       implicit session =>
       var modificationOk = false
       val args = request.body.asFormUrlEncoded.get
        if (args != None
            &&
            args.contains("value")
            &&
            args.get("value").get(0).toString.length < Application.maxCharacterDefinitions
            &&
            canThisUserChangeGroupDefinition(userID.toLong, groupID)){
          val userIDInt = userID.toLong
          val userDescription = args.get("value").get(0).toString
          Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
  		      val q2 = for {c <- Instantiations.Tables.groups if c.id === groupID && c.logicalDelete === 0} yield c.definition
            q2.update(userDescription)
            val statement = q2.updateStatement
            val invoker = q2.updateInvoker
            //We delete from cache 
            Cache.remove("group" + groupID)
          }
          }
	        ActionRepport.actionDone(userIDInt, 1, groupID, groupID, 10100, null)
	        Ok(Json.toJson(1))
        }
        else{
          Ok(Json.toJson(0))
        }
      }
  }, None, true)

  /*
   * Modifies name of group if the user can.
   */
  def processGroupNameModification(groupID: Long) = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
      Instantiations.Tables.db.withSession {
       implicit session =>
       var modificationOk = false
       val args = request.body.asFormUrlEncoded.get
        if (args != None
            &&
            args.contains("value")
            &&
            args.get("value").get(0).toString.length < Application.maxCharacternames
            &&
            canThisUserChangeGroupName(userID.toLong, groupID)){
          val userIDInt = userID.toLong
          val userName = args.get("value").get(0).toString
          Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
		        val q2 = for {c <- Instantiations.Tables.groups if c.id === groupID && c.logicalDelete === 0} yield c.name
            q2.update(userName)
            val statement = q2.updateStatement
            val invoker = q2.updateInvoker
            Cache.remove("group" + groupID)
            }
          }
	        ActionRepport.actionDone(userIDInt, 1, groupID, groupID, 10101, null)
	        Ok(Json.toJson(1))
        }
        else{
          Ok(Json.toJson(0))
        }
      }
  }, None, true)

  /*
   * Deletes a group defined by the "key" POSTDATA. Returns 1 if the comment has been deleted, and 0 otherwise
   */
  def processGroupDelete  = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
    var jsonToSend = 0
    val userIDLong = userID.toLong
    val args = request.body.asFormUrlEncoded.get
    if (
       args != None 
         && 
       args.contains("key") 
       &&
       canThisUserDeleteGroup(userIDLong, args.get("key").get(0).toLong)
       )
    {
      Instantiations.Tables.db.withSession { implicit session =>
        Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
          Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
          //We delete the group
          val q2 = for { c <- Instantiations.Tables.groups if (c.id === args.get("key").get(0).toLong) } yield c.logicalDelete
          q2.update(1) //We set LOGICALDELETE to 1
          val statement = q2.updateStatement
          val invoker = q2.updateInvoker
          jsonToSend = 1
          
          //We then delete all the activities associated with this group
          //The GroupCreation activity
          val q3 = for { c <- Instantiations.Tables.activity if (c.activityType === 10000 && c.activityObject === args.get("key").get(0).toLong && c.memberId === userIDLong) } yield c.logicalDelete
          q3.update(1) //We set LOGICALDELETE to 1
          val statement3 = q3.updateStatement
          val invoker3 = q3.updateInvoker
          
          //The GroupJoin activities
          val q4 = for { c <- Instantiations.Tables.activity if (c.activityType === 10001 && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
          q4.update(1) //We set LOGICALDELETE to 1
          val statement4 = q4.updateStatement
          val invoker4 = q4.updateInvoker
          
          /*
           * The Group change rank : Member, Group change rank : Leader, Group change rank : Admin, 
           * GroupAccept, GroupLeave, GroupDescriptionModification, GroupNameModification activities (only associated with this group)
           */
          val q5 = for { c <- Instantiations.Tables.activity if ((c.activityType === 10002 || c.activityType === 10003 || c.activityType === 10004 || c.activityType === 10005 || c.activityType === 10010 || c.activityType === 10100 || c.activityType === 10101) && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
          q5.update(1) //We set LOGICALDELETE to 1
          val statement5 = q5.updateStatement
          val invoker5 = q5.updateInvoker
          
          //The DebateForGroupCreation, GroupActionCreation, GroupPetitionCreation
          val q6 = for { c <- Instantiations.Tables.activity if ((c.activityType === 30010 || c.activityType === 40001 || c.activityType === 50001) && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
          q6.update(1) //We set LOGICALDELETE to 1
          val statement6 = q6.updateStatement
          val invoker6 = q6.updateInvoker
          
          //We delete from cache 
          Cache.remove("group" + args.get("key").get(0).toLong)
        }
      }
        //We unfollow this group for everybody
        //Instantiations.Tables.follow.filter(x => (x.objectFollowedType === 1 && x.objectFollowedID === args.get("key").get(0).toLong)).delete
      }
    }
    Ok(Json.toJson(jsonToSend))
  }, None, true)
  
    //(userID: Int, groupID: Int)
   /* def upload = Action(parse.temporaryFile) { request =>
        import java.io.File
        request.body.moveTo(new File("/"))
        Ok("File uploaded")
    }*/


}
