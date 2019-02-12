package controllers
import play.api._
import play.api.mvc._

import play.api.Play.current
import play.api.cache.Cache;

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

object Concepts extends Controller {
  
  //Twitter parameters
  val consumerKeyConcepts = ""
  val consumerSecretConcepts = ""
  val accessTokenConcepts = ""
  val accessSecretConcepts = ""
  
  //conceptthougth
  val consumerKeyConceptsEN = ""
  val consumerSecretConceptsEN = ""
  val accessTokenConceptsEN = ""
  val accessSecretConceptsEN = ""
  
   //conceptosmedita
  val consumerKeyConceptsES = ""
  val consumerSecretConceptsES = ""
  val accessTokenConceptsES = "-"
  val accessSecretConceptsES = ""
  
 /*
  * Converts a database Concept entry into a proper models.Concept class
  */ 
  def convertConceptDatabaseEntryToClass(databaseEntry: models.Types.conceptEntry) = {
    models.Concept(databaseEntry._1.get, Application.outputHTMLText(databaseEntry._2, false), Application.outputHTMLText(databaseEntry._3, true), databaseEntry._4, databaseEntry._5, databaseEntry._6, databaseEntry._10, databaseEntry._7.getTime())
  }

val conceptWrites: Writes[models.Concept] = {
  //Concept(name: String, definition: String, idCreator:Int, membersFor: Int, membersAgainst: Int)
  ((JsPath \ "id").write[Long] and
  (JsPath \ "name").write[String] and
  (JsPath \ "definition").write[String] and
  (JsPath \ "idCreator").write[Long] and
  (JsPath \ "membersFor").write[Int] and
  (JsPath \ "membersAgainst").write[Int] and
  (JsPath \ "language").write[String] and
  (JsPath \ "modificationTimestamp").write[Long])(unlift(models.Concept.unapply))
}

  def postConceptOnTwitter(language: String, conceptId: Long, conceptName: String) = {
    language match {
      case "fr" => Application.postOnTwitter(conceptId, 
          conceptName, 
          "/concept", 
          consumerKeyConcepts, 
          consumerSecretConcepts, 
          accessTokenConcepts, 
          accessSecretConcepts)
      case "es" => Application.postOnTwitter(conceptId, 
          conceptName, 
          "/concept", 
          consumerKeyConceptsES, 
          consumerSecretConceptsES, 
          accessTokenConceptsES, 
          accessSecretConceptsES)
      case "en" => Application.postOnTwitter(conceptId, 
          conceptName, 
          "/concept", 
          consumerKeyConceptsEN, 
          consumerSecretConceptsEN, 
          accessTokenConceptsEN, 
          accessSecretConceptsEN)
    }
  }

/*
 * Process the creation form concept Forms.conceptCreationForm
 */
  def processConceptCreationForm = Connection.withConnection({
     username => userID => implicit request => implicit lang =>
	   models.Forms.conceptCreationForm.bindFromRequest.value map { conceptCreationForm =>
         //We first check if the concept is already in the database
	     Instantiations.Tables.db.withSession { implicit session =>
	       //println(conceptCreationForm.definition)
	       val q2 = for {c <- Instantiations.Tables.concepts if c.name === conceptCreationForm.name} yield c.id
           if (
               conceptCreationForm.name.length < Application.maxCharacternames 
               && 
               conceptCreationForm.definition.length < Application.maxCharacterDefinitions 
               &&
               Application.langs.contains(conceptCreationForm.language)
               &&
               q2.list == Nil){
             val userIDInt = userID.toLong
             //println((for {c <- Instantiations.Tables.members if c.username === "leclem"} yield c.id ).list.head)
             //println(conceptCreationForm.name+ " " + conceptCreationForm.definition + " " + userIDInt)
	         Instantiations.Tables.concepts += (None, scala.xml.Utility.escape(conceptCreationForm.name), Jsoup.clean(conceptCreationForm.definition, Application.whiteListHTML), userIDInt, 0, 0, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0, conceptCreationForm.language)
	         val conceptID = q2.list.head
	         ActionRepport.actionDone(userIDInt, 2, conceptID, conceptID, 20000, null)
	         
	         //We follow this concept
           User.followThisObject(userIDInt, 2, conceptID)
             //We post it on our twitter account
           postConceptOnTwitter(conceptCreationForm.language, conceptID, scala.xml.Utility.escape(conceptCreationForm.name))
           Ok(Json.toJson(conceptID))
           }
           else{
             Ok(Json.toJson((Messages("views.concept_already_exists"))))
           }
	     }
       } getOrElse {
         BadRequest
       }
    }, None, true)
  
  
  def getConcept(conceptID: Long) = Connection.withConnection (
    username => userID => implicit request => implicit lang => {
      val conceptRetrieved = getConceptModel(conceptID)
      if (conceptRetrieved != null){
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
        ActionRepport.addView(2, conceptID)
        Ok(views.html.connected.displayConcept("", conceptRetrieved))
      } else if (conceptID == -1){
        Ok(views.html.connected.concept(Messages("views.concept_deleted")))
      }
      else{
        Ok(views.html.connected.concept(Messages("views.concept_doesnt_exists")))
      }
    }, Option((request: Request[AnyContent]) => getConceptUnconnected(conceptID, request)))
  
  def getConceptUnconnected(conceptID: Long, request: Request[AnyContent]) = {
    val conceptRetrieved = getConceptModel(conceptID)
    if (conceptRetrieved != null){
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
      ActionRepport.addView(2, conceptID)
      Ok(views.html.connected.displayConcept("", conceptRetrieved, false)(request.session, Instantiations.getLang(request))).withSession("language" -> conceptRetrieved.language)
    }
    else{
      Ok(views.html.index("")(Instantiations.getLang(request)))
    }
  }
  
  /*
   * Returns a model.Concept corresponding to the concept having the ID provided
   */
  def getConceptModel(conceptID: Long) = {
    Instantiations.Tables.db.withSession { implicit session =>
      val valueFromCache = Cache.getAs[models.Concept]("concept" + conceptID)
      if (valueFromCache == None){
        val q2 = Instantiations.Tables.concepts.filter(concept => concept.id === conceptID && concept.logicalDelete === 0)
        val requestExecuted = q2.list
        if (requestExecuted != Nil){
          val valueToReturn = convertConceptDatabaseEntryToClass(requestExecuted.head)
          Cache.set("concept" + conceptID, valueToReturn, Application.cacheDuraction)
          valueToReturn
        }
        else{
          Cache.set("concept" + conceptID, null, Application.cacheDuraction)
          null
        }
      }
      else{
        valueFromCache.get
      }
    }
  }
  
  /*
   * Gets the vote of the user. 0 : the user didn't vote. 
   * 1 : The user voted FOR the concept. 
   * 2 : The user voted AGAINST the concept.
   */
  def getUserVote(conceptID : Long, userIDInt: Long): Int = {
     Instantiations.Tables.db.withSession { implicit session =>
     
     val q2 = for { c <- Instantiations.Tables.conceptsOpinions if (c.conceptID === conceptID && c.voterID === userIDInt) } yield c.opinion
        val listOpinion = q2.list //Should be of length 0 or 1
        if (listOpinion == Nil){
          0
        }
        else if (listOpinion.head){
          1
        }
        else {
          2
        }
     }
  }
  
  	
  
  /*
   * Cancels the vote of the connected user on the concept defined by conceptID
   */
  def cancelVote(conceptID : Long) = Connection.withConnection ({
    username => userID => implicit request => implicit lang =>
      Instantiations.Tables.db.withSession { implicit session =>
        val userIDInt = userID.toLong
        val userConcept = getUserVote(conceptID, userIDInt)
        var cancelVoteAccepted = false
        if (userConcept == 1){
          val thread = new Thread(new Runnable {
            def run() {
              Instantiations.Tables.db.withSession { implicit session =>
                sqlu"update CONCEPT set MEMBERSFOR=MEMBERSFOR-1 where SUP_ID = $conceptID".first
              }
            }
          })
          thread.start;
          cancelVoteAccepted = true
        }
        else if (userConcept == 2){
          val thread = new Thread(new Runnable {
            def run() {
              Instantiations.Tables.db.withSession { implicit session =>
                sqlu"update CONCEPT set MEMBERSAGAINST=MEMBERSAGAINST-1 where SUP_ID = $conceptID".first
              }
            }
          }) 
          thread.start;
          cancelVoteAccepted = true
        }
        if (cancelVoteAccepted){
          //We unfollow this concept (the user don't know that concepts can be followed, it is for this action to appear in its news feed)
          User.unfollowThisObject(userIDInt, 2, conceptID)
          
          ActionRepport.actionDone(userIDInt, 2, conceptID, conceptID, 20004, null)
        }
        
        val q = Instantiations.Tables.conceptsOpinions.filter(x => (x.voterID === userIDInt && x.conceptID === conceptID)).delete
        Ok
      }
  }, None, true)
  
  /*
   * Retrieves the last 10 concepts created in JSON format
   */
  def getLastConcepts(numberOfConcepts: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        if (numberOfConcepts <= controllers.Application.maxResults && numberOfConcepts >= 0){
          val userIDLong = userID.toLong
          val request = for {c <- Instantiations.Tables.concepts.filter(_.logicalDelete === 0).sortBy(_.id.desc)} yield c

          val onlyShowMemberLanguagesInRequest =  for {(request, memberLanguage) <- request innerJoin User.requestForUserLanguages(userIDLong) on ((request, memberLanguage) => request.language === memberLanguage.language)} yield request
          
          val results = onlyShowMemberLanguagesInRequest.take(numberOfConcepts).list.par.map(x => convertConceptDatabaseEntryToClass(x)).seq
          
          implicit val wrtier = conceptWrites
          Ok(Json.toJson(results))
        }
        else{
          BadRequest
        }
      }
    }
  }
  
  /*
   * Retrieves the last 10 concepts where the user voted
   */
  def getLastConceptsParticipatedOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val explicitInnerJoin = for {
		  (conceptsOpinion, concept) <- Instantiations.Tables.conceptsOpinions.sortBy(_.dateVote.desc) innerJoin Instantiations.Tables.concepts on
		  ((conceptsOpinion, concept) => conceptsOpinion.voterID === userIDInt && conceptsOpinion.conceptID === concept.id && conceptsOpinion.dateVote < maxTimestampParam && concept.logicalDelete === 0)
		  } yield (concept, conceptsOpinion.dateVote)
          val results = explicitInnerJoin.take(number).list.par.map(x => convertConceptDatabaseEntryToClass(x._1).copy(modificationTimestamp = x._2.getTime())).seq
          implicit val wrtier = conceptWrites
          Ok(Json.toJson(results))
        } 
        else {
          BadRequest
        }
      }
    }
  }
  
  /*
   * Retrieves the last 10 concepts created by the user
   */
  def getLastConceptsCreatedByUserOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val q2 = for {c <- Instantiations.Tables.concepts.filter(x => (x.idCreator === userIDInt && x.creationTimestamp < maxTimestampParam && x.logicalDelete === 0)).sortBy(_.modificationTimestamp.desc).take(number)} yield c
          val results = q2.list.par.map(x => convertConceptDatabaseEntryToClass(x)).seq
          implicit val wrtier = conceptWrites
          Ok(Json.toJson(results))
        } 
        else {
          BadRequest
        }
      }
    }
  }
  
  /*
   * Retrieves the last 10 concepts created from a certain id
   */
  def getLastConceptsOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      val userIDLong = userID.toLong
      Instantiations.Tables.db.withSession { implicit session =>
        
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        if (number <= controllers.Application.maxResults && number >= 0){
          val q2 = for {c <- Instantiations.Tables.concepts.filter(x => (x.modificationTimestamp < maxTimestampParam && x.logicalDelete === 0))} yield c

          val onlyShowMemberLanguagesInRequest =  for {(request, memberLanguage) <- q2 innerJoin User.requestForUserLanguages(userIDLong) on ((request, memberLanguage) => request.language === memberLanguage.language)} yield request
          
          val results = onlyShowMemberLanguagesInRequest.sortBy(_.modificationTimestamp.desc).take(number).list.par.map(x => convertConceptDatabaseEntryToClass(x)).seq
          implicit val wrtier = conceptWrites
          Ok(Json.toJson(results))
        } 
        else {
          BadRequest
        }
      }
    }
  }
  
  def getMyLastConceptsOrderedbyTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection {
    username => userID => implicit request => implicit lang => {
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val userIDInt = userID.toLong
        if (number <= controllers.Application.maxResults && number >= 0){
          val q2 = for {c <- Instantiations.Tables.concepts.filter(x => (x.modificationTimestamp < maxTimestampParam && x.idCreator === userIDInt && x.logicalDelete === 0)).sortBy(_.modificationTimestamp.desc).take(number)} yield c
          val results = q2.list.par.map(x => convertConceptDatabaseEntryToClass(x)).seq
          implicit val wrtier = conceptWrites
          Ok(Json.toJson(results))
        } 
        else {
          BadRequest
        }
      }
    }
  }
  
  /*
   * Gets the vote of the user. 0 : the user didn't vote. 
   * 1 : The user voted FOR the concept. 
   * 2 : The user voted AGAINST the concept.
   */
  def getUserVoteJSON(conceptID : Long) =  Connection.withConnection {
    username => userID => implicit request => implicit lang =>
      Ok(Json.toJson(getUserVote(conceptID, userID.toLong)))
  }
  
  /*
   * Takes into account a vote for concept and returns a Json indicationg the result :
   * 11 : Never voted for this concept and voted YES
   * 12 : Never voted for this concept and voted NO
   * 21 : Already voted YES for this concept and voted YES
   * 21 : Already voted NO for this concept and voted NO
   * 6 : This concept doesn't exists
   * 0 : Is voting for the same thing he already voted
   */
  def voteForConcept(conceptID: Long, vote: Boolean) =
    Connection.withConnection ({
    username => userID => implicit request => implicit lang => 
      var returned = 0
      val userIDInt = userID.toLong
      Instantiations.Tables.db.withSession { implicit session =>
      val concept = getConceptModel(conceptID)
      if (concept != null){
        val creatorConcept = concept.idCreator
        //We check if it has not voted yet for this
        val q2 = for { c <- Instantiations.Tables.conceptsOpinions if (c.conceptID === conceptID && c.voterID === userID.toLong) } yield c.opinion
        val listOpinion = q2.list //Should be of length 0 or 1
        if (listOpinion == Nil){
          val thread = new Thread(new Runnable {
            def run() {
              Instantiations.Tables.db.withSession { implicit session =>
                Instantiations.Tables.conceptsOpinions += (userIDInt, conceptID, vote, new java.sql.Timestamp(System.currentTimeMillis()))
              }
            }
          })
          thread.start;   
          if (vote){
            val thread = new Thread(new Runnable {
                def run() {
                  Instantiations.Tables.db.withSession { implicit session =>
                    sqlu"update CONCEPT set MEMBERSFOR=MEMBERSFOR+1 where SUP_ID = $conceptID".first
                  }
                }
            })
            thread.start;
            User.modifynumberofLikes(creatorConcept, 1)
            Cache.remove("memberStats" + creatorConcept)
            
            ActionRepport.actionDone(userIDInt, 2, conceptID, conceptID, 20002, null)
        
            returned = 11
          }
          else{
            val thread = new Thread(new Runnable {
                def run() {
                  Instantiations.Tables.db.withSession { implicit session =>
                    sqlu"update CONCEPT set MEMBERSAGAINST=MEMBERSAGAINST+1 where SUP_ID = $conceptID".first
                  }
                }
            })
            thread.start;
            User.modifynumberofDislikes(creatorConcept, 1)
            Cache.remove("memberStats" + creatorConcept)
            
            ActionRepport.actionDone(userIDInt, 2, conceptID, conceptID, 20003, null)
        
            returned = 12
          }
          
          //We follow this concept
          User.followThisObject(userIDInt, 2, conceptID)
          
        }
        else{
          if (listOpinion.head != vote){
            
            //If it exists, we update the vote
            if (vote){
              Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                Instantiations.Tables.db.withSession { implicit session =>
                  sqlu"update CONCEPT set MEMBERSFOR=MEMBERSFOR+1 where SUP_ID = $conceptID".first
                  sqlu"update CONCEPT set MEMBERSAGAINST=MEMBERSAGAINST-1 where SUP_ID = $conceptID".first
                }
                Cache.remove("memberStats" + creatorConcept)
              }
              User.modifynumberofLikes(creatorConcept, 1)
              User.modifynumberofDislikes(creatorConcept, -1)
              
              ActionRepport.actionDone(userIDInt, 2, conceptID, conceptID, 20006, null)
              
              
              returned = 21
              q2.update(vote)

            val statement = q2.updateStatement
            val invoker = q2.updateInvoker

            }
            else{
              Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                Instantiations.Tables.db.withSession { implicit session =>
                  sqlu"update CONCEPT set MEMBERSFOR=MEMBERSFOR-1 where SUP_ID = $conceptID".first
                  sqlu"update CONCEPT set MEMBERSAGAINST=MEMBERSAGAINST+1 where SUP_ID = $conceptID".first
                }
                Cache.remove("memberStats" + creatorConcept)
              }
              
              User.modifynumberofLikes(creatorConcept, -1)
              User.modifynumberofDislikes(creatorConcept, 1)
                  
                    
              ActionRepport.actionDone(userIDInt, 2, conceptID, conceptID, 20005, null)
              
            returned = 22
            val thread3 = new Thread(new Runnable {
              def run() {
                  Instantiations.Tables.db.withSession { implicit session =>
                    q2.update(vote)
                }
              }
            })
            thread3.start;   

            val statement = q2.updateStatement
            val invoker = q2.updateInvoker

            }
            
            //We follow this concept
            User.followThisObject(userIDInt, 2, conceptID)
            Cache.remove("concept" + conceptID)
          }
        }
      }
      else{
        returned = 6
      }
      Ok(Json.toJson(returned)) ///TODO MORE
    }
  }, None, true)
  
  def getPopularConcepts() = {
    ///TODO
    null
  }

  /*
   * Checks if the concept defined by conceptID exists in the database
   */
  def conceptExists(conceptID: Long): Boolean = {
    Instantiations.Tables.db.withSession { implicit session =>
    val q2 = Instantiations.Tables.concepts.filter(_.id === conceptID)
      return !(q2.list == Nil)
    }
  }
  
  /*
   * Modifies concept description. Returns 1 if it succeeded and 0 if it failed
   */
  def processConceptDescriptionModification(conceptID: Long) = Connection.withConnection({
    username => userID => implicit request => implicit lang =>
      Instantiations.Tables.db.withSession {
       implicit session =>
       var modificationOk = false
       val args = request.body.asFormUrlEncoded.get
       val userIDInt = userID.toLong
       val q2 = for {c <- Instantiations.Tables.concepts if c.idCreator === userIDInt && c.id === conceptID && c.logicalDelete === 0} yield c.id
        if (args != None 
            && 
            args.contains("key") 
            &&
            args.get("key").get(0).toString.length < Application.maxCharacterDefinitions
            && 
            q2.list != Nil){
          val userFirstName = args.get("key").get(0).toString
	    	  val q2 = for {c <- Instantiations.Tables.concepts if c.id === userIDInt} yield c.definition
          q2.update(scala.xml.Utility.escape(userFirstName))
          
          //We delete from cache 
          Cache.remove("concept" + conceptID)
          
	      ActionRepport.actionDone(userIDInt, 2, conceptID, conceptID, 20001, null)
	      Ok(Json.toJson(1))
        }
        else{
          Ok(Json.toJson(0))
        }
      }
  }, None, true)
  
  /*
   * Deletes a concept defined by the "key" POSTDATA. Returns 1 if the comment has been deleted, and 0 otherwise
   */
  def processConceptDelete  = Connection.withConnection({
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
        val q2 = for { c <- Instantiations.Tables.concepts if (c.id === args.get("key").get(0).toLong && c.idCreator === userIDLong) } yield c.logicalDelete              
        if (q2.list != Nil){
          Instantiations.Tables.db.withSession { implicit session =>
            q2.update(1) //We set LOGICALDELETE to 1
            val statement = q2.updateStatement
            val invoker = q2.updateInvoker
          }
          //We then delete all the activities associated with this concept (associated with the creator)
          //The ConceptCreation activity
          Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implementation, because the old session may be closed when this thread requests will execute
            
            val q3 = for { c <- Instantiations.Tables.activity if (c.activityType === 20000 && c.activityObject === args.get("key").get(0).toLong && c.memberId === userIDLong) } yield c.logicalDelete
            q3.update(1) //We set LOGICALDELETE to 1
            val statement3 = q3.updateStatement
            val invoker3 = q3.updateInvoker
          //The ConceptDescriptionModification activity (associated with the creator)
            val q4 = for { c <- Instantiations.Tables.activity if (c.activityType === 20001 && c.activityObject === args.get("key").get(0).toLong && c.memberId === userIDLong) } yield c.logicalDelete
            q4.update(1) //We set LOGICALDELETE to 1
            val statement4 = q4.updateStatement
            val invoker4 = q4.updateInvoker
          /*
           * The VoteFor Concept /VoteAgainst Concept / CancelVote / 
           * Modif vote Votefor -> VoteAgainst /Modif vote VoteAgainst -> Votefor activities associated with this concept
           */
            val q5 = for { c <- Instantiations.Tables.activity if ((c.activityType === 20002 || c.activityType === 20003 || c.activityType === 20004 || c.activityType === 20005 || c.activityType === 20006) && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
            q5.update(1) //We set LOGICALDELETE to 1
            val statement5 = q5.updateStatement
            val invoker5 = q5.updateInvoker
          
            //The comments posts
            val q6 = for { c <- Instantiations.Tables.activity if (c.activityType === 60002 && c.activityObject === args.get("key").get(0).toLong) } yield c.logicalDelete
            q6.update(1) //We set LOGICALDELETE to 1
            val statement6 = q6.updateStatement
            val invoker6 = q6.updateInvoker
            
            Cache.remove("concept" + args.get("key").get(0).toLong)
          }
          }
          
          //We unfollow this concept for everybody
          //Instantiations.Tables.follow.filter(x => (x.objectFollowedType === 2 && x.objectFollowedID === args.get("key").get(0).toLong)).delete
          
          //We delete from cache 
          
          jsonToSend = 1
        }
      }
    }
    Ok(Json.toJson(jsonToSend))
  }, None, true)
  
}
