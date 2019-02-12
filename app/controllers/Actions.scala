package controllers

import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.i18n._
import play.api.i18n.I18nSupport

import java.sql.SQLTimeoutException
import play.api.Play.current
import play.api.cache.Cache

import play.api.libs.json._
import play.api.libs.functional.syntax._
import slick.driver.MySQLDriver.simple._
import slick.jdbc.{GetResult, StaticQuery => Q}
import slick.jdbc.JdbcBackend.Database
import Q.interpolation

import org.jsoup._
import org.jsoup.safety.Whitelist



object Actions extends Controller {


  //Twitter parameters
  val consumerKeyActions = ""
  val consumerSecretActions = ""
  val accessTokenActions = "-"
  val accessSecretActions = ""

  //actionsofpeople
  val consumerKeyActionsEN = ""
  val consumerSecretActionsEN = ""
  val accessTokenActionsEN = ""
  val accessSecretActionsEN = ""
  
  val consumerKeyActionsES = ""
  val consumerSecretActionsES = ""
  val accessTokenActionsES = ""
  val accessSecretActionsES = ""
  
  //variables for the octTree

  var lastUpdateOfOctTree: Long = 0
  val octTreeUpdateInterval = 60000 //Update the octTree (new actions, delete actions) every minute
  val maxNumGhostNodes = 100 //After this number of deletes, we rebuild the tree in order to keep it efficient
  var updating = false //Var signaling if the OctTree is currently being updated

  var octTree = new OctTree[models.Action]
  /*
   * Creates an action with the user posted parameters
   */
  /*
   * val ActionCreationForm = Form(
	  mapping(
	    "name" -> text,
	    "definition" -> text,
	    "positionX" -> text,
	    "positionY" -> text,
	    "timeBegin" -> text,
	    "timeEnd" -> text
	  )
   */
  /*
   * def creatorType = column[Int]("TYPE_CREATOR")
  def creatorID = column[Int]("ID_CREATOR") //ID of a creator - group or member
  def name = column[String]("NAME") // The ID, member or group, of the creator
  def definition = column[String]("DEFINITION") // This is the primary key column
  def positionX = column[Int]("POSITIONX") //X position on a map
  def positionY = column[Int]("POSITIONY") //Y position on a map
  def timeBegin = column[Long]("TIMEBEGIN")
  def timeEnd = column[Long]("TIMEEND")
  def numberJoined = column[Long]("NBREJOINED")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
   */
  //Action(id: Int, creatorType: Int, creatorID: Int, name: String,
  //definition: String, latitude: Double, longitude: Double, timeBegin: Long, timeEnd: Long, numberJoined: Long)
  def convertActionDatabaseEntryToClass(databaseEntry: models.Types.actionEntry) = {
    models.Action(databaseEntry._1.get, databaseEntry._2, databaseEntry._3, "", Application.outputHTMLText(databaseEntry._4, false), Application.outputHTMLText(databaseEntry._5, true), databaseEntry._6, databaseEntry._7, databaseEntry._8, databaseEntry._9, databaseEntry._10, databaseEntry._11, databaseEntry._12, databaseEntry._16, databaseEntry._13.getTime())
  }
  val actionWrites: Writes[models.Action] = {
    ((JsPath \ "id").write[Long] and
      (JsPath \ "creatorType").write[Int] and
      (JsPath \ "creatorID").write[Long] and
      (JsPath \ "creatorName").write[String] and
      (JsPath \ "name").write[String] and
      (JsPath \ "definition").write[String] and
      (JsPath \ "nameLocation").write[String] and
      (JsPath \ "addressLocation").write[String] and
      (JsPath \ "latitude").write[Double] and
      (JsPath \ "longitude").write[Double] and
      (JsPath \ "timeBegin").write[Long] and
      (JsPath \ "timeEnd").write[Long] and
      (JsPath \ "numberJoined").write[Long] and
      (JsPath \ "language").write[String] and
      (JsPath \ "modificationTimestamp").write[Long])(unlift(models.Action.unapply))
  }
  /*
   * Creates an action.
   * Returns the id of the created action if succeeded
   * Returns 2 if the action name already exists so the action cannot be created
   * Returns 0 if another reason prevented from the action creation
   */

  def postActionOnTwitter(language: String, actionID : Long, actionTitle: String) = {
    if (language == "fr")
      Application.postOnTwitter(actionID, actionTitle, "/action", consumerKeyActions, consumerSecretActions, accessTokenActions, accessSecretActions)
    else if (language == "en")
      Application.postOnTwitter(actionID, actionTitle, "/action", consumerKeyActionsEN, consumerSecretActionsEN, accessTokenActionsEN, accessSecretActionsEN)
    else if (language == "es")
      Application.postOnTwitter(actionID, actionTitle, "/action", consumerKeyActionsES, consumerSecretActionsES, accessTokenActionsES, accessSecretActionsES)
    
  }

  def createAction(idGroup: Long, userIDInt: Long, name: String, definition: String, namelocation: String, addresslocation: String, positionX: Double, positionY: Double, timeBegin: Long, timeEnd: Long, language: String, postActivity: Boolean) = {
    try {
    Instantiations.Tables.db.withSession { implicit session =>
            val q2 = for {c <- Instantiations.Tables.actions if c.name === scala.xml.Utility.escape(name)} yield c.id

            /*println(actionCreationForm.timeBegin + " " + actionCreationForm.timeEnd + " " +  actionCreationForm.positionX + " " +  actionCreationForm.positionY)
            println(actionCreationForm.name.length < Application.maxCharacternames)
            println(actionCreationForm.definition.length < Application.maxCharacterDefinitions)
            println(actionCreationForm.positionX != "")
            println(actionCreationForm.positionY != "")
            println(actionCreationForm.timeBegin != "")
            println(actionCreationForm.timeEnd != "")
            println(actionCreationForm.timeBegin.toLong > 0)
            println((actionCreationForm.timeEnd.toLong > actionCreationForm.timeBegin.toLong || actionCreationForm.timeEnd.toLong == -1))
            println((actionCreationForm.timeEnd.toLong > (System.currentTimeMillis()-86400000) || actionCreationForm.timeEnd.toLong == -1))
            println(Application.langs.contains(actionCreationForm.language))*/
            val actionsDoesntExists = (q2.list == Nil)
            var returned = BadRequest
            //We verify that the name doesn't exists already and that the strings arn't too long
            if (name.length < Application.maxCharacternames
              &&
              definition.length < Application.maxCharacterDefinitions
              &&
              namelocation.length < Application.maxCharacternames
              &&
              positionX != ""
              &&
              positionY != ""
              &&
              timeBegin != ""
              &&
              timeEnd != ""
              &&
              timeBegin.toLong > 0
              &&
              (timeEnd.toLong > timeBegin.toLong || timeEnd.toLong == -1)
              &&
              (timeEnd.toLong > (System.currentTimeMillis()-86400000) || timeEnd.toLong == -1) //Minus one day in order to handle all timezones
              &&
              Application.langs.contains(language)
              &&
              actionsDoesntExists){


              if (idGroup == -1){
                //We have nothing to check, all the actions are OK if created by an user
                val tupleToAdd = (None, 0, userIDInt, scala.xml.Utility.escape(name), Jsoup.clean(definition, Application.whiteListHTML), namelocation, addresslocation, positionX.toDouble, positionY.toDouble, timeBegin.toLong, timeEnd.toLong, 0L, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0, language)
                Instantiations.Tables.actions += tupleToAdd
                Thread.sleep(500)
                val createdAction = q2.list.head
                if (postActivity)
                  ActionRepport.actionDone(userIDInt, 4, createdAction, createdAction, 40000, null)

                //We follow this action
                User.followThisObject(userIDInt, 4, createdAction)
                
                postActionOnTwitter(language, createdAction, scala.xml.Utility.escape(name))

                Ok(Json.toJson(createdAction))
              }
              else{
                val idOfGroupCreator = idGroup
                if (Groups.canThisUserCreateGroupActions(userIDInt, idOfGroupCreator)){ //We verify that the user can create actions with this group
                val tupleToAdd = (None, 1, idOfGroupCreator, name, definition, namelocation, addresslocation, positionX.toDouble, positionY.toDouble, timeBegin.toLong, timeEnd.toLong, 0L, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0, language)
                  Instantiations.Tables.actions += tupleToAdd
                  val createdAction = q2.list.head
                  if (postActivity)
                    ActionRepport.actionDone(userIDInt, 1, idOfGroupCreator, createdAction, 40001, null, idOfGroupCreator)

                  //We post the action on twitter only if the group is public :
                  if (Groups.getGroupModel(idOfGroupCreator).privacy == 0)
                    postActionOnTwitter(language, createdAction, scala.xml.Utility.escape(name))

                  User.followThisObject(userIDInt, 4, createdAction)

                  Ok(Json.toJson(createdAction))
                }
                else{
                  BadRequest
                }
              }

            }
            else{
              if (!actionsDoesntExists){
                Ok(Json.toJson(2))
              }
              else {
                Ok(Json.toJson(0))
              }
              //Ok(views.html.connected.action(Messages("views.action.alreadyexists")))
              //Messages("views.action.alreadyexists")
            }
          }
    } catch {
      case sqle: SQLTimeoutException => Ok(Json.toJson(0))
    }
  }
  
  def processActionCreationForm = {
    Connection.withConnection({
      username => userID => implicit request => implicit lang => {

        models.Forms.actionCreationForm.bindFromRequest.value map { actionCreationForm =>
          createAction(actionCreationForm.idGroup.toLong, userID.toLong, actionCreationForm.name, actionCreationForm.definition, actionCreationForm.namelocation, actionCreationForm.addresslocation, actionCreationForm.positionX.toDouble, actionCreationForm.positionY.toDouble, actionCreationForm.timeBegin.toLong, actionCreationForm.timeEnd.toLong, actionCreationForm.language, true)
                
        } getOrElse {
          BadRequest
        }
      }
    }, None, true)
  }
  /*
   * Sets the right creator name to the petition, so the group name if the creator is a group and the username if the creator is an user
   */
  def associateRightCreatorname(action: models.Action, username: Option[String], group: Option[String]) = {
    if (action.creatorType == 0){//a member created it
      action.copy(creatorName = username.get)
    }
    else{
      action.copy(creatorName = group.get)
    }
  }

  /*
   * Retrieves the last 10 actions created from a certain id
   */
  def getLastActionsOrderedbyTimestampJSON(groupPrivacy: Long, maxTimestamp: Long, number: Int) = Connection.withConnection({
    username => userID => implicit request => implicit lang => {
      val userIDLong = userID.toLong

      getLastActionsOrderedbyTimestampJSONsub(userIDLong, groupPrivacy, maxTimestamp, number)
    }
  }, Option((request: Request[AnyContent]) => getLastActionsOrderedbyTimestampJSONsub(-1, groupPrivacy, maxTimestamp, number, Some(Instantiations.getLangText(request)))))

  def getLastActionsOrderedbyTimestampJSONsub(userIDLong: Long, groupPrivacy: Long, maxTimestamp: Long, number: Int, onlyShowLanguage: Option[String] = None) = {
    Instantiations.Tables.db.withSession { implicit session =>
      val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
      if (number <= controllers.Application.maxResults && number >= 0){
        var finalResults: slick.lifted.Query[(models.ActionTable, slick.lifted.Column[Option[String]], slick.lifted.Column[Option[String]], slick.lifted.Column[Option[Int]], slick.lifted.Column[Option[Long]], slick.lifted.Column[Option[Long]]),(models.Types.actionEntry, Option[String], Option[String], Option[Int], Option[Long], Option[Long]),Seq] = null

        val listOfActionsAndGroupMemberAssociated = for {
          ((action, member), group) <-
          (Instantiations.Tables.actions innerJoin Instantiations.Tables.members on
            ((action, member) =>
              action.modificationTimestamp < maxTimestampParam &&
                action.creatorID === member.id &&
                action.logicalDelete === 0)) leftJoin Instantiations.Tables.groups on ((actionMember, group) => actionMember._1.creatorID === group.id && group.logicalDelete === 0)
        } yield (action, member.username?, group.name?, group.privacy?, group.id?, member.id?)

        if (groupPrivacy == -1){ //We want the public list of actions
        //We display all the public actions (all not created by a private group)
        val filterAllPrivateActions = listOfActionsAndGroupMemberAssociated.filter(x => x._1.creatorType === 0 || (x._1.creatorType === 1 && x._4 === 0))
          var filterPrivateActions = filterAllPrivateActions
          if (userIDLong != -1){
            //We display Actions created by private groups where the user is member if we are connected
            val filterPrivateActionsByGroupsMember = for {
              (groupMember, actionsAndGroupMemberAssociated) <- Instantiations.Tables.groupMembership.filter(_.memberId === userIDLong) innerJoin listOfActionsAndGroupMemberAssociated.filter(x => x._1.creatorType === 1 && x._4 === 1) on ((groupMember, actionsAndGroupMemberAssociated) =>
                (groupMember.groupId === actionsAndGroupMemberAssociated._5))
            } yield actionsAndGroupMemberAssociated

            //We union these two lists of actions so we have all actions that the user can view
            filterPrivateActions = filterPrivateActionsByGroupsMember ++ filterPrivateActions
          }
          finalResults = filterPrivateActions
        }
        else{//We want the actions of a group
        val userStatusInGroup = Groups.userStatusInGroup(groupPrivacy, userIDLong, true)
          if (userStatusInGroup == 1 || userStatusInGroup == 3)
            finalResults = listOfActionsAndGroupMemberAssociated.filter(x => x._1.creatorType === 1 && x._1.creatorID === groupPrivacy)
          else
            finalResults = listOfActionsAndGroupMemberAssociated.filter(x => x._1.creatorType === -1) //We return nothing
        }

        //We filter private actions, created by private groups
        //val filterPrivateActions = listOfActionsAndGroupMemberAssociated.filter(x => x._1.creatorType === 0 || (x._1.creatorType === 1 && x._4 === 0))

        //We filter the request so we only have languages of the user
        val onlyShowMemberLanguagesInRequest = {
          if (groupPrivacy == -1 && userIDLong != -1)
            for {(request, memberLanguage) <- finalResults innerJoin User.requestForUserLanguages(userIDLong) on ((request, memberLanguage) => request._1.language === memberLanguage.language)} yield request
          else if (onlyShowLanguage != None)
            finalResults.filter(_._1.language === onlyShowLanguage.get)
          else
            finalResults //No lang filter for group debates
        }
        val results = onlyShowMemberLanguagesInRequest.sortBy(_._1.modificationTimestamp.desc).list.map(x => associateRightCreatorname(convertActionDatabaseEntryToClass(x._1), x._2, x._3))

        implicit val wrtier = actionWrites
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
  def getLastActionsCreateByUserOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val q2 = for {c <- Instantiations.Tables.actions.filter(x => (x.creatorType === 0 && x.creatorID === userIDInt && x.modificationTimestamp < maxTimestampParam && x.logicalDelete === 0)).sortBy(_.modificationTimestamp.desc).take(number)} yield c
          val results = q2.list.map(x => convertActionDatabaseEntryToClass(x))
          implicit val wrtier = actionWrites
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
  def canThisUserParticipateToThisAction(actionID: Long, userID: Long): Boolean = {
    Instantiations.Tables.db.withSession { implicit session =>
      //First we get the petition to display
      val actionToDisplay = for {c <- Instantiations.Tables.actions.filter(x => x.id === actionID && x.logicalDelete === 0)} yield (c.creatorType, c.creatorID)

      val result = actionToDisplay.list

      if (result != Nil && (result.head._1 == 0)){
        true
      }
      else if (result != Nil){
        val userStatusInGroup = Groups.userStatusInGroup(result.head._2, userID, true) //The user can access if it is a public group (3) or if he is in the group (1)
        if (result != Nil && result.head._1 == 1 && (userStatusInGroup == 1 || userStatusInGroup == 3)){
          true
        }
        else{
          false
        }
      }
      else{
        false
      }
    }
  }

  /*
   * Returns the action associated with the actionID.
   */
  def getAction(actionID: Long) = Connection.withConnection ({
    username => userID => implicit request => implicit lang =>
      val actionRetrieved = getActionModel(actionID)
      if (actionRetrieved != null && canThisUserParticipateToThisAction(actionID, userID.toLong)){
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
        ActionRepport.addView(4, actionID)
        Ok(views.html.connected.displayAction("", actionRetrieved))
      }
      else{
        Ok(views.html.connected.action(Messages("views.action_doesnt_exists")))
      }
    }, Option((request: Request[AnyContent]) => getActionUnconnected(actionID, request)) )

  def getActionUnconnected(actionID: Long, request: Request[AnyContent]) = {
    if (canThisUserParticipateToThisAction(actionID, -1)){
      val actionRetrieved = getActionModel(actionID)
      if (actionRetrieved != null){
        Ok(views.html.connected.displayAction("", actionRetrieved, false)(request.session, Instantiations.getLang(request))).withSession("language" -> actionRetrieved.language)
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
   * Returns the view of the action defined by actionID
   */
  def getCreateActionPage() = Connection.withConnection {
    username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.createAction(""))
  }

  /*
   * Returns the view of the action defined by actionID
   */
  def getnActionPage() = Connection.withConnection {
    username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.nAction(""))
  }

  /*
   * Boolean telling if an action exists
   */
  def actionExists(actionID: Long): Boolean = {
    Instantiations.Tables.db.withSession { implicit session =>
      Instantiations.Tables.actions.filter(action => action.id === actionID && action.logicalDelete === 0).exists.run
    }
  }

  /*
   * Returns a model.Action corresponding to the action define by the ID provided
   */
  def getActionModel(actionID: Long) = {
    Instantiations.Tables.db.withSession { implicit session =>
      val valueFromCache = Cache.getAs[models.Action]("action" + actionID)
      if (valueFromCache == None){
        val actionAndGroupMemberAssociated = for {
          ((action, member), group) <-
          (Instantiations.Tables.actions innerJoin Instantiations.Tables.members on
            ((action, member) =>
              action.id === actionID &&
                action.creatorID === member.id &&
                action.logicalDelete === 0)) leftJoin Instantiations.Tables.groups on (_._1.creatorID === _.id)
        } yield (action, member.username?, group.name?)

        //val q2 = Instantiations.Tables.actions.filter(_.id === actionID)
        val requestExecuted = actionAndGroupMemberAssociated.list
        if (requestExecuted != Nil){
          val valueToReturn = associateRightCreatorname(convertActionDatabaseEntryToClass(requestExecuted.head._1), requestExecuted.head._2, requestExecuted.head._3)
          Cache.set("action" + actionID, valueToReturn, Application.cacheDuraction)
          valueToReturn
        }
        else{
          Cache.set("action" + actionID, null, Application.cacheDuraction)
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
  def isMemberInAction(memberID: Long, actionID: Long): Boolean = {
    Instantiations.Tables.db.withSession { implicit session =>
      Instantiations.Tables.actionJoin.filter(x => x.actionId === actionID && x.memberId === memberID).exists.run
    }
  }

  /*
   * Makes the loged member joind the action defined by actionID. Returns 1 if success and 0 if failure
   */
  def joinAction() = Connection.withConnection ({
    username => userID => implicit request => implicit lang =>
      var returnedJson = 0
      Instantiations.Tables.db.withSession { implicit session =>
        val args = request.body.asFormUrlEncoded.get

        if (args != None && args.contains("actionID")){
          // TODO : VERIFY IF ACTION EXISTS AND MEMBER IS NOT ALREADY
          val actionID = args.get("actionID").get(0).toLong
          val userIDInt = userID.toLong
          val actionModel = getActionModel(actionID)
          if (actionModel != null && !isMemberInAction(userIDInt, actionID)){
            Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
                Instantiations.Tables.actionJoin += (userIDInt, actionID, 0, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())))
                sqlu"update `ACTION` set NBREJOINED=NBREJOINED+1 where ACTION.SUP_ID = $actionID".first
              }
            }

            if (actionModel.creatorType == 1){
              ActionRepport.actionDone(userIDInt, 4, actionID, actionID, 40002, null, actionModel.creatorID)
            }
            else{
              ActionRepport.actionDone(userIDInt, 4, actionID, actionID, 40002, null, -1)
            }


            //We follow this action (the user don't know that actions can be followed, it is for this action to appear in its news feed)
            User.followThisObject(userIDInt, 4, actionID)


            returnedJson = 1
          }
        }
      }
      Ok(Json.toJson(returnedJson))
  }, None, true)

  /*
   * Makes the loged member leave the action defined by actionID. Returns 1 if success and 0 if failure
   */
  def leaveAction() = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
      var returnedJson = 0
      Instantiations.Tables.db.withSession { implicit session =>
        val args = request.body.asFormUrlEncoded.get
        if (args != None && args.contains("actionID")){
          val actionID = args.get("actionID").get(0).toLong
          val userIDInt = userID.toLong
          val actionModel = getActionModel(actionID)
          val q = Instantiations.Tables.actionJoin.filter(x => (x.memberId === userIDInt && x.actionId === actionID)).delete
          if (actionModel != null && q == 1){
            Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
                sqlu"update `ACTION` set NBREJOINED=NBREJOINED-1 where ACTION.SUP_ID = $actionID".first
              }
            }
            returnedJson = 1

            //We unfollow this action (the user don't know that actions can be followed, it is for this action to appear in its news feed)
            User.unfollowThisObject(userIDInt, 4, actionID)

            if (actionModel.creatorType == 1){
              ActionRepport.actionDone(userIDInt, 4, actionID, actionID, 40003, null, actionModel.creatorID)
            }
            else{
              ActionRepport.actionDone(userIDInt, 4, actionID, actionID, 40003, null, -1)
            }
          }
        }
      }
      Ok(Json.toJson(returnedJson))
  }, None, true)

  /*
  * Retrieves the last 10 actions that the user joined
  */
  def getLastActionsJoinedOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val explicitInnerJoin = for {
            (actionJoin, action) <- Instantiations.Tables.actionJoin.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.actions on
              ((actionJoin, action) => actionJoin.memberId === userIDInt && actionJoin.actionId === action.id && actionJoin.modificationTimestamp < maxTimestampParam && action.logicalDelete === 0)
          } yield (action, actionJoin.modificationTimestamp)
          val results = explicitInnerJoin.take(number).list.par.map(x => convertActionDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime())).seq
          implicit val wrtier = actionWrites
          Ok(Json.toJson(results))
        }
        else {
          BadRequest
        }
      }
    }
  }

  /*
     * Gets the JSON of members in the group limited by numberOfGroups where the timestamp of last modification
     * is inferior to the one in parameter
     */
  def getMembersInActionOrderedbyTimestampJSON(actionID: Long, maxTimestamp: Long, number: Int) = Connection.withConnection ({
    username => userID => implicit request => implicit lang =>
      if (canThisUserParticipateToThisAction(actionID, userID.toLong))
        getMembersInActionOrderedbyTimestampJSONsub(actionID, maxTimestamp, number, userID.toLong)
      else
        BadRequest
    }, Option((request: Request[AnyContent]) => {
      if (canThisUserParticipateToThisAction(actionID, -1))
        getMembersInActionOrderedbyTimestampJSONsub(actionID, maxTimestamp, number)
      else
        BadRequest
      }))

    def getMembersInActionOrderedbyTimestampJSONsub(actionID: Long, maxTimestamp: Long, number: Int, userID: Long = -1) = {
      val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
      if (number <= Application.maxResults){
        Instantiations.Tables.db.withSession { implicit session =>
          //We create a request to get all the memberInfo we want, the timestamps of joining the action, and the rank of the member in the group
          val explicitInnerJoin = for {
      ((actionJoin, memberInfo), action) <-
		  Instantiations.Tables.actionJoin innerJoin Instantiations.Tables.membersInfo on
		  ((actionJoin, memberInfo) => actionJoin.actionId === actionID && actionJoin.memberId === memberInfo.id) leftJoin Instantiations.Tables.actions on ((tupleActionJoinMemberInfo, action) => tupleActionJoinMemberInfo._1.actionId === action.id && action.logicalDelete == 0)
		  } yield (memberInfo, actionJoin.modificationTimestamp)

		  //For each of them, we check if the user is following them
		   val explicitInnerJoin2 = User.addUserFollowingToMembersInfoRequest(explicitInnerJoin, userID)

          //We set the correct modification timestamp and the correct isUserFollowing
          val results = explicitInnerJoin2.list.par.map(
              x => User.convertMemberInfoDatabaseEntryToClass(x._1._1).copy(isUserFollowing = {if(x._2 == None){0}else{1}}, modificationTimestamp = x._1._2.getTime())
              ).seq
          implicit val writer = User.userWrites
          Ok(Json.toJson(results))
        }
      }
      else{
        BadRequest
      }
  }

  def getActionsJSON(minTimestamp: Long, maxTimestamp: Long, ha: Double, hb: Double, va: Double, vb: Double, maxNumber: Int) = Connection.withConnection ({
    username => userID => implicit request => implicit lang => {
      getActionsJSONSub(minTimestamp, maxTimestamp, ha, hb, va,vb, maxNumber)
    }
  }, Option((request: Request[AnyContent]) => {
    getActionsJSONSub(minTimestamp, maxTimestamp, ha, hb, va,vb, maxNumber)
  })
  )

  def getActionsJSONSub(minTimestamp: Long, maxTimestamp: Long, ha: Double, hb: Double, va: Double, vb: Double, maxNumber: Int) = {


    println("Call on getAction time " + System.currentTimeMillis())
    Instantiations.Tables.db.withSession {
      implicit session =>

        //every minute check the DB for new insertions

        if(System.currentTimeMillis() - lastUpdateOfOctTree > octTreeUpdateInterval && !updating) {
          updating = true
          if(octTree.numGhostNodes > maxNumGhostNodes){
            println("rebuilding tree with " + octTree.numGhostNodes)
            octTree = new OctTree[models.Action]
            println("Tree has " + octTree.numGhostNodes)
            lastUpdateOfOctTree = 0

          }
          val ts = new java.sql.Timestamp(Application.timestampToSearch(lastUpdateOfOctTree))
           /*
          val actionQuery = Instantiations.Tables.actions.filter(a => a.timeBegin >= minTimestamp && a.timeBegin <= maxTimestamp &&
            a.latitude >= ha && a.latitude <= hb && a.longitude >= va && a.longitude <= vb && a.logicalDelete === 0).sortBy(_.numberJoined.desc).take(maxNumber + 1)
          // We extract one extra entry from the DB to know if there are more than muxNumber entries
          val actions = actionQuery.list.map(x => convertActionDatabaseEntryToClass(x))
          val moreAvailable = actions.length > maxNumber
          */

          //handle insert
          val actionQuery = Instantiations.Tables.actions.filter(a => a.modificationTimestamp >  ts && a.logicalDelete === 0)
          val actions = actionQuery.list.map(x => convertActionDatabaseEntryToClass(x))
          for (testAct <- actions)
            octTree.insert(testAct.latitude,testAct.longitude,testAct.timeBegin,testAct)
          println("updated " + actions.size)



          //handle deletes + modifs
          //TODO weird: timestamps in database seem to be updated not in real time but only after an houer or so -> this number works for 2-3 hours, keep it
          val ts2Time = System.currentTimeMillis() - 36000000
          println(ts2Time +" LALALALA")
          val ts2 = new java.sql.Timestamp(Application.timestampToSearch(ts2Time))
          //val ts2 = new java.sql.Timestamp(Application.timestampToSearch(0))
          val delQuery = Instantiations.Tables.actions.filter(a => a.modificationTimestamp >=  ts2 && a.logicalDelete === 1)
          val delActions = delQuery.list.map(x => convertActionDatabaseEntryToClass(x))
          println("Res Del: " + delActions)
          for (testAct <- delActions) {
            val delit = octTree.deleteAt(testAct.latitude, testAct.longitude, testAct.timeBegin, testAct)

            println("WE DELETED : " + delit)
          }

          lastUpdateOfOctTree = System.currentTimeMillis()
          updating = false

        }



        val actions = octTree.query(ha,hb,va,vb,minTimestamp,maxTimestamp,maxNumber+1)
        val moreAvailable = actions.size > maxNumber


        implicit val wrtier = actionWrites
        val returnObject = Json.obj( "actions" -> Json.toJson (actions.take(maxNumber)), "moreAvailable" -> moreAvailable)
        Ok (returnObject)
    }
  }


  /*
* Deletes a action defined by the "key" POSTDATA. Returns 1 if the comment has been deleted, and 0 otherwise
*/
  def processActionDelete  = Connection.withConnection({
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
          var deleteRestOfActivities = false
          val creatorTypeAndIdList = (for { c <- Instantiations.Tables.actions if (c.id === args.get("key").get(0).toLong) } yield (c.creatorType, c.creatorID)).list

          if (creatorTypeAndIdList != Nil && creatorTypeAndIdList.head._1 == 1){
            //The creator is a group
            if (Groups.canThisUserDeleteGroupContent(userIDLong, creatorTypeAndIdList.head._2)){
              
              Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
                val q2 = for { c <- Instantiations.Tables.actions if (c.id === args.get("key").get(0).toLong) } yield c.logicalDelete
                q2.update(1) //We set LOGICALDELETE to 1
                val statement = q2.updateStatement
                val invoker = q2.updateInvoker
                jsonToSend = 1
  
                //We then delete the activities specific to a action creation for group
                val q5 = for { c <- Instantiations.Tables.activity if (c.activityType === 40001 && c.activityTarget === args.get("key").get(0).toLong) } yield c.logicalDelete
                q5.update(1) //We set LOGICALDELETE to 1
                val statement5 = q5.updateStatement
                val invoker5 = q5.updateInvoker
              }
            }
              deleteRestOfActivities = true
            }
          }
          else{
            //The creator is a member
            val q2 = for { c <- Instantiations.Tables.actions if (c.id === args.get("key").get(0).toLong  && c.creatorID === userIDLong) } yield c.logicalDelete
            if (q2.list != Nil){
              
              Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
                q2.update(1) //We set LOGICALDELETE to 1
                val statement = q2.updateStatement
                val invoker = q2.updateInvoker
                jsonToSend = 1
  
                //We then delete the activities specific to a action creation for member
                val q5 = for { c <- Instantiations.Tables.activity if (c.activityType === 40000 && c.activityObject === args.get("key").get(0).toLong && c.memberId === userIDLong) } yield c.logicalDelete
                q5.update(1) //We set LOGICALDELETE to 1
                val statement5 = q5.updateStatement
                val invoker5 = q5.updateInvoker
              }
              }
              deleteRestOfActivities = true
            }
          }
          if (deleteRestOfActivities){
            Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
              //We unfollow this action for everybody
              //Instantiations.Tables.follow.filter(x => (x.objectFollowedType === 4 && x.objectFollowedID === args.get("key").get(0).toLong)).delete
  
              //We then delete all the joinAction and leaveAction activities
              val q2 = for { c <- Instantiations.Tables.activity if ((c.activityType === 40002 || c.activityType === 40003) && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
              q2.update(1) //We set LOGICALDELETE to 1
              val statement2 = q2.updateStatement
              val invoker2 = q2.updateInvoker
  
              //Delete CommentYesornoopinion
              val q6 = for { c <- Instantiations.Tables.activity if (c.activityType === 60006 && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
              q6.update(1) //We set LOGICALDELETE to 1
              val statement6 = q6.updateStatement
              val invoker6 = q6.updateInvoker
            }
            }
            //We delete from cache
            Cache.remove("action" + args.get("key").get(0).toLong)
          }

        }
      }
      Ok(Json.toJson(jsonToSend))
  }, None, true)

//  import slick.jdbc.{GetResult, StaticQuery => Q}

  def testTree(actions : List[models.Action]) = {
  //  println("Got these acts: " + actions)
    if (actions != Nil) {
      println("Actions not nil")
      for (testAct <- actions)
        octTree.insert(testAct.latitude,testAct.longitude,testAct.timeBegin,testAct)

      println(octTree.query(Double.MinValue,Double.MaxValue,Double.MinValue,Double.MaxValue,Long.MinValue,Long.MaxValue))

    }
    //val bla1 = Q.queryNA("SELECT UPDATE_TIME FROM  information_schema.tables WHERE  TABLE_SCHEMA = 'dmcr'  AND TABLE_NAME = 'ACTION'").list

    /*
    Instantiations.Tables.db.withSession { implicit session =>
      val bla1 = sql"""SELECT UPDATE_TIME FROM  information_schema.tables WHERE  TABLE_SCHEMA = 'dmcr'  AND TABLE_NAME = 'ACTION' """.as[String].firstOption
      println("LAST: " + bla1 + "last" + System.currentTimeMillis())
    }*/

  }

//TODO test feature max-number so that we only look for the necessary number of actions
  class OctNode[A](val xs: Double, val ys: Double, val zs: Long, val vs:A){
    var UNW, UNE, USW, USE, LNW,LNE, LSW, LSE: Option[OctNode[A]] = None
    val x = xs
    val y = ys
    val z= zs
    var values = List(vs)
  }
  class OctTree[A]{
    var root: Option[OctNode[A]] = None
    var numNodes = 0
    var numGhostNodes = 0

    def insert(x: Double, y: Double,z: Long, value: A): Unit ={
      root = insert(root,x,y,z,value)
    }

    private def insert(current: Option[OctNode[A]], x:Double, y:Double, z:Long, value: A): Some[OctNode[A]] =
      current match {
        case None => {
          numNodes+=1
          Some(new OctNode(x, y,z, value))
        }
        //Each node has a list of values occuring at the same coordinates
          //TODO look at modification of an action if this will ever be implemented, for now not the case
        case sn@ Some(node)  =>
          if (node.x ==x && node.y == y && node.z == z) node.values = value ::node.values
          else if (x < node.x && y < node.y   && z < node.z)   node.LSW = insert(node.LSW,x,y,z,value)
          else if (x < node.x && y >= node.y  && z < node.z)   node.LNW = insert(node.LNW,x,y,z,value)
          else if (x >= node.x && y < node.y  && z < node.z)   node.LSE = insert(node.LSE,x,y,z,value)
          else if (x >= node.x && y >= node.y && z < node.z)   node.LNE = insert(node.LNE,x,y,z,value)

          else if (x < node.x && y < node.y  && z >= node.z)  node.USW = insert(node.USW,x,y,z,value)
          else if (x < node.x && y >= node.y && z >= node.z)  node.UNW = insert(node.UNW,x,y,z,value)
          else if (x >= node.x && y < node.y && z >= node.z)  node.USE = insert(node.USE,x,y,z,value)
          else if (x >= node.x && y >= node.y && z >= node.z) node.UNE = insert(node.UNE,x,y,z,value)
          else throw new Exception("Illegal state while inserting")
          sn
      }

    def query(ha: Double, hb: Double, va:Double, vb: Double, minTime:Long, maxTime:Long, maxNumber: Int = Int.MaxValue): List[A] ={
      query(root, ha, hb, va, vb, minTime, maxTime, maxNumber)
    }
    private def query(current: Option[OctNode[A]], ha: Double, hb: Double, va:Double, vb: Double, minTime:Long, maxTime:Long,maxNumber: Int): List[A] = current match{
      case None => Nil
      case sn@ Some(node) =>
        var nextList = List[A]()
        if( node.x >= ha && node.x <= hb && node.y>= va && node.y <= vb && node.z >= minTime && node.z <= maxTime) nextList = nextList :::  node.values

        if(nextList.size < maxNumber) {
          if (ha < node.x && va < node.y && minTime < node.z) nextList = nextList ::: query(node.LSW, ha, hb, va, vb, minTime, maxTime,maxNumber)
          if (ha < node.x && vb >= node.y && minTime < node.z) nextList = nextList ::: query(node.LNW, ha, hb, va, vb, minTime, maxTime,maxNumber)
          if (hb >= node.x && va < node.y && minTime < node.z) nextList = nextList ::: query(node.LSE, ha, hb, va, vb, minTime, maxTime,maxNumber)
          if (hb >= node.x && vb >= node.y && minTime < node.z) nextList = nextList ::: query(node.LNE, ha, hb, va, vb, minTime, maxTime,maxNumber)

          if (ha < node.x && va < node.y && maxTime >= node.z) nextList = nextList ::: query(node.USW, ha, hb, va, vb, minTime, maxTime,maxNumber)
          if (ha < node.x && vb >= node.y && maxTime >= node.z) nextList = nextList ::: query(node.UNW, ha, hb, va, vb, minTime, maxTime,maxNumber)
          if (hb >= node.x && va < node.y && maxTime >= node.z) nextList = nextList ::: query(node.USE, ha, hb, va, vb, minTime, maxTime,maxNumber)
          if (hb >= node.x && vb >= node.y && maxTime >= node.z) nextList = nextList ::: query(node.UNE, ha, hb, va, vb, minTime, maxTime,maxNumber)
        }
        nextList.take(maxNumber)

    }

    def deleteAt(x: Double,y:Double, z:Long, value:A): Boolean ={
      deleteAt(root,x,y,z,value)
    }
    private def deleteAt(current: Option[OctNode[A]], x: Double, y: Double, z:Long, value:A): Boolean = current match{
      case None => false
      case sn@ Some(node) =>
        if(node.x ==x && node.y ==y && node.z == z){//} node.values.contains(value)){
         // node.values = node.values.filterNot(v => v == value)
          //TODO Remove the lists of the datastructure-> our assumption is that no event can occur at same place and same time: EXACT same time
          node.values = Nil
          if(node.values.isEmpty){
            numGhostNodes+=1
          }
          true
        }
        else if (x < node.x && y < node.y   && z < node.z)  deleteAt(node.LSW,x,y,z,value)
        else if (x < node.x && y >= node.y  && z < node.z)  deleteAt(node.LNW,x,y,z,value)
        else if (x >= node.x && y < node.y  && z < node.z)  deleteAt(node.LSE,x,y,z,value)
        else if (x >= node.x && y >= node.y && z < node.z)  deleteAt(node.LNE,x,y,z,value)

        else if (x < node.x && y < node.y  && z >= node.z)  deleteAt(node.USW,x,y,z,value)
        else if (x < node.x && y >= node.y && z >= node.z)  deleteAt(node.UNW,x,y,z,value)
        else if (x >= node.x && y < node.y && z >= node.z)  deleteAt(node.USE,x,y,z,value)
        else if (x >= node.x && y >= node.y && z >= node.z) deleteAt(node.UNE,x,y,z,value)
        else throw new Exception("Illegal state while deleting")

    }
  }
}

