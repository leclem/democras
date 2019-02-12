package controllers


import play.api._

import play.api.mvc._

import play.api.mvc.Results._
import slick.driver.MySQLDriver.simple._
import play.api.i18n._

import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.Play.current
import play.api.cache.Cache;
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import slick.jdbc.{GetResult, StaticQuery => Q}
import slick.jdbc.JdbcBackend.Database
import Q.interpolation

import twitter4j.Status;
import twitter4j.Twitter;
import twitter4j.TwitterException;
import twitter4j.TwitterFactory;
import twitter4j.auth.AccessToken;
import twitter4j.auth.RequestToken;
import twitter4j.auth.OAuthAuthorization
import twitter4j.conf.ConfigurationBuilder
import org.jsoup.safety.Whitelist
import scala.io.Source
import java.util.concurrent.Semaphore

object Application extends Controller {
  //PARAMETERS OF THE PROGRAM
  val maxResults = 50 //The maximum number of results we display in a JSON

  val maxCharacternames=254
  val maxCharactersOpinions=65534
  val maxCharacterDefinitions=100000

  val maxNumberOfMessageReceivers=20

  val maxSizeOfProfilePictures=500000 //In octets

  val numOpinionsNotToRandomize = 2
  val timeIntervalForReFresh = 1000*60*60

  //
  //Groups
  /*
   * 1 : Membre
   * 2 : Leader
   * 1000 : Admin
   */
  val possibleGroupRanks: List[Int] = List(1, 2, 1000)
  val groupRanksThatCanApproveMembersApplication: List[Int] = List(2, 1000)
  val groupRanksThatCanCreatePrivateDebates: List[Int] = List(2, 1000)
  val groupRanksThatCanChangeMemberRank: List[Int] = List(1000)
  val groupRanksThatCanChangeDefinition: List[Int] = List(2, 1000)
  val groupRanksThatCanChangeName: List[Int] = List(1000)
  val groupRanksThatCanDeleteGroup: List[Int] = List(1000)
  val groupRanksThatCanDeleteGroupContent: List[Int] = List(2, 1000)
  val groupRanksThatCanCreateActions: List[Int] = List(2, 1000)
  val groupRanksThatCanCreatePetitions: List[Int] = List(2, 1000)



  val cacheDuraction = 3600 //Seconds that data from database is cached
  val cacheForNewsFeed = 60

  val langs: List[String] = List("fr", "en", "es")

  val genders: List[Int] = List(-1, 0, 1) //All the possible genders -1: not specified, 0:woman, 1: man

  val regexUsername = "([0-9A-Za-z.-_])+" //The username has to match this REGEX

  val whiteListHTML = Whitelist.none()
            .addTags("p", "b", "i", "table", "tbody", "tr", "td", "br", "ol", "li", "ul", "u", "a")
            .addAttributes("table", "class")
            .addAttributes("p", "style")
            .addAttributes("img", "src") //The authorized HTML in the forms that allow html
            //Doc : https://jsoup.org/apidocs/org/jsoup/safety/Whitelist.html

  val activateTwitterPosting = true //we have to activate it for our real server but deactivate it when we are develloping new tools

  def changeLanguage(newLanguage: String) = Connection.withConnection (
      username => userID => implicit request => implicit lang => {
        if (langs.contains(newLanguage)){
          Instantiations.Tables.db.withSession { implicit session =>
            //We change the session language as well as the user main language
            val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userID.toLong} yield c.lang
            q2.update(newLanguage)
            if (Application.langs.contains(newLanguage) && !User.requestForUserLanguages(userID.toLong).list.contains(newLanguage)){
              try{
                Instantiations.Tables.membersLanguage += (userID.toLong, newLanguage)
              }
             catch {
               case mysqlexception: Exception => {
                 //The language is already in the user languages
               }
             }
            }
          }
          Ok(Json.toJson(1)).withSession(
            "userID" -> userID,
    	      "username" -> username,
            "language" -> newLanguage
          )
        }
        else{
          Ok(Json.toJson(0))
        }
      }
   , Option((request: Request[AnyContent]) => changeLanguageDisconnected(newLanguage)))

  def changeLanguageDisconnected(newLanguage: String) = {
    if (langs.contains(newLanguage)){
      Ok(Json.toJson(1)).withSession("language" -> newLanguage)
    }
    else{
      Ok(Json.toJson(0))
    }
  }

  def index = Action {

    request =>
    Instantiations.Tables.db.withSession { implicit session =>
     // Mailer.sendEmail("clement.moutet@hec.edu", "clement.moutet@hec.edu", "test", "mail.messagesubscription")

      //User.getMemberInfoFromID(16)
    //
      /*controllers.Instantiations.Tables.voteYesOrNo.ddl.drop
      controllers.Instantiations.Tables.debateYesOrNo.ddl.drop
      controllers.Instantiations.Tables.debate.ddl.drop
      *//*
      controllers.Instantiations.Tables.conversationMessages.ddl.drop
      controllers.Instantiations.Tables.conversationMember.ddl.drop
      controllers.Instantiations.Tables.conversation.ddl.drop
       controllers.Instantiations.Tables.conversation.ddl.create
      controllers.Instantiations.Tables.conversationMember.ddl.create
      controllers.Instantiations.Tables.conversationMessages.ddl.create*/

    /*
     RESET DATABASE :*/
/*
 * Todo : créer table memberlanguage, rajouter des entrées language et logicaldelete sur toutes les entrées débat, pétition, action, concept et logicaldelete sur comment, opinion, yesornoopinion
 */
      
      /*controllers.Instantiations.Tables.membersFriendsMails.ddl.drop
      controllers.Instantiations.Tables.membersStatsMonthly.ddl.drop
      controllers.Instantiations.Tables.numberOfViews.ddl.drop
      controllers.Instantiations.Tables.mailsUnsubscribed.ddl.drop
      controllers.Instantiations.Tables.membersStats.ddl.drop
      controllers.Instantiations.Tables.debateRandomOpinion.ddl.drop
      controllers.Instantiations.Tables.notification.ddl.drop
      controllers.Instantiations.Tables.passwordForget.ddl.drop
     controllers.Instantiations.Tables.membersLanguage.ddl.drop
      controllers.Instantiations.Tables.comment.ddl.drop
      controllers.Instantiations.Tables.conversationMessages.ddl.drop
      controllers.Instantiations.Tables.conversationMember.ddl.drop
      controllers.Instantiations.Tables.conversation.ddl.drop
      controllers.Instantiations.Tables.membersProfilePicture.ddl.drop
	  controllers.Instantiations.Tables.membersExternalIds.ddl.drop
      controllers.Instantiations.Tables.follow.ddl.drop
      controllers.Instantiations.Tables.petitionJoin.ddl.drop
      controllers.Instantiations.Tables.petition.ddl.drop
      controllers.Instantiations.Tables.actionJoin.ddl.drop
      controllers.Instantiations.Tables.actions.ddl.drop
      controllers.Instantiations.Tables.activity.ddl.drop
      controllers.Instantiations.Tables.groupApplication.ddl.drop
      controllers.Instantiations.Tables.groupMembership.ddl.drop
      controllers.Instantiations.Tables.groups.ddl.drop
      controllers.Instantiations.Tables.doingAction.ddl.drop
      controllers.Instantiations.Tables.membersInfo.ddl.drop
      controllers.Instantiations.Tables.voteOpinion.ddl.drop
      controllers.Instantiations.Tables.voteYesOrNo.ddl.drop
      controllers.Instantiations.Tables.debateYesOrNo.ddl.drop
      controllers.Instantiations.Tables.debateOpinion.ddl.drop
      controllers.Instantiations.Tables.debate.ddl.drop
      controllers.Instantiations.Tables.conceptsOpinions.ddl.drop
      controllers.Instantiations.Tables.concepts.ddl.drop
      controllers.Instantiations.Tables.members.ddl.drop

     controllers.Instantiations.Tables.members.ddl.create
      controllers.Instantiations.Tables.membersInfo.ddl.create
      controllers.Instantiations.Tables.concepts.ddl.create
      controllers.Instantiations.Tables.conceptsOpinions.ddl.create
      controllers.Instantiations.Tables.doingAction.ddl.create
      controllers.Instantiations.Tables.groups.ddl.create
     controllers.Instantiations.Tables.groupMembership.ddl.create
     controllers.Instantiations.Tables.groupApplication.ddl.create
     controllers.Instantiations.Tables.debate.ddl.create
      controllers.Instantiations.Tables.debateOpinion.ddl.create
      controllers.Instantiations.Tables.debateYesOrNo.ddl.create
      controllers.Instantiations.Tables.voteYesOrNo.ddl.create
       controllers.Instantiations.Tables.voteOpinion.ddl.create
      controllers.Instantiations.Tables.activity.ddl.create
      controllers.Instantiations.Tables.actions.ddl.create
      controllers.Instantiations.Tables.actionJoin.ddl.create
      controllers.Instantiations.Tables.petition.ddl.create
      controllers.Instantiations.Tables.petitionJoin.ddl.create
      controllers.Instantiations.Tables.follow.ddl.create
      controllers.Instantiations.Tables.membersExternalIds.ddl.create
      controllers.Instantiations.Tables.membersProfilePicture.ddl.create
      controllers.Instantiations.Tables.conversation.ddl.create
      controllers.Instantiations.Tables.conversationMember.ddl.create
      controllers.Instantiations.Tables.conversationMessages.ddl.create
      controllers.Instantiations.Tables.comment.ddl.create
      controllers.Instantiations.Tables.membersLanguage.ddl.create
      controllers.Instantiations.Tables.passwordForget.ddl.create
      controllers.Instantiations.Tables.notification.ddl.create
      controllers.Instantiations.Tables.debateRandomOpinion.ddl.create
      controllers.Instantiations.Tables.membersStats.ddl.create
      controllers.Instantiations.Tables.mailsUnsubscribed.ddl.create
      controllers.Instantiations.Tables.numberOfViews.ddl.create
      controllers.Instantiations.Tables.membersStatsMonthly.ddl.create
      controllers.Instantiations.Tables.membersFriendsMails.ddl.create
      controllers.Instantiations.Tables.membersPagesLoaded.ddl.create*/
      /*
       * update userstats for installation
      Instantiations.Tables.membersInfo.list.map{ x =>
        val idmember = x._1

        val numberVotesLikeConcept = (for {(concept, conceptsOpinions) <- Instantiations.Tables.concepts.filter(_.idCreator === idmember) innerJoin Instantiations.Tables.conceptsOpinions.filter(_.opinion === true) on (
  	      (concept, conceptsOpinions) =>
  	        concept.id === conceptsOpinions.conceptID
  	    )} yield (conceptsOpinions)).length.run
  	    val numberVotesDislikeConcept = (for {(concept, conceptsOpinions) <- Instantiations.Tables.concepts.filter(_.idCreator === idmember) innerJoin Instantiations.Tables.conceptsOpinions.filter(_.opinion === false) on (
  	      (concept, conceptsOpinions) =>
  	        concept.id === conceptsOpinions.conceptID
  	    )} yield (conceptsOpinions)).length.run
  	    //println("id" + idmember + "  likes:  " + numberVotesLikeConcept)

  	    val numberVotesLikeDebateOpinion = (for {(debateOpinion, voteOpinion) <- Instantiations.Tables.debateOpinion.filter(_.idMember === idmember) innerJoin Instantiations.Tables.voteOpinion.filter(_.vote === 1) on (
  	      (debateOpinion, voteOpinion) =>
  	        debateOpinion.id === voteOpinion.idOpinion
  	    )} yield (voteOpinion)).length.run

  	    val numberVotesDisLikeDebateOpinion = (for {(debateOpinion, voteOpinion) <- Instantiations.Tables.debateOpinion.filter(_.idMember === idmember) innerJoin Instantiations.Tables.voteOpinion.filter(_.vote === 0) on (
  	      (debateOpinion, voteOpinion) =>
  	        debateOpinion.id === voteOpinion.idOpinion
  	    )} yield (voteOpinion)).length.run

  	    val numberVotesLikeDebateYesOrNo = (for {(debateYesOrNo, voteYesOrNo) <- Instantiations.Tables.debateYesOrNo.filter(_.idMember === idmember) innerJoin Instantiations.Tables.voteYesOrNo.filter(_.vote === 1) on (
  	      (debateOpinion, voteYesOrNo) =>
  	        debateOpinion.id === voteYesOrNo.idOpinion
  	    )} yield (voteYesOrNo)).length.run

  	    val numberVotesDisLikeDebateYesOrNo = (for {(debateYesOrNo, voteYesOrNo) <- Instantiations.Tables.debateYesOrNo.filter(_.idMember === idmember) innerJoin Instantiations.Tables.voteYesOrNo.filter(_.vote === 0) on (
  	      (debateOpinion, voteYesOrNo) =>
  	        debateOpinion.id === voteYesOrNo.idOpinion
  	    )} yield (voteYesOrNo)).length.run

  	    val numberFollow = Instantiations.Tables.follow.filter(x => x.objectFollowedType === 0 && x.objectFollowedID === idmember).length.run
  	    val numberOfLikes = numberVotesLikeConcept + numberVotesLikeDebateOpinion + numberVotesLikeDebateYesOrNo
  	    val numberOfDislikes = numberVotesDislikeConcept + numberVotesDisLikeDebateOpinion + numberVotesDisLikeDebateYesOrNo
  	    Instantiations.Tables.db.withSession { implicit session =>
  	      Instantiations.Tables.membersStats += (idmember, numberFollow, numberOfLikes, numberOfDislikes)
        }
  	    //println("id : " + idmember + " votesopinion " + numberVotesLikeDebateOpinion)
  	    //println("id : " + idmember + " votesconcept : " + q2.length.run)
        //val numberLikesConept = Instantiations.Tables.conceptsOpinions

      }*/


        /*CREATION OF LECLEM ACCOUNT*//*
     Instantiations.Tables.members += (None, "moutet", "leclem", "pass")
     println((for {c <- Instantiations.Tables.members if c.username === "leclem"} yield c.id ).list.head)
       val q2 = for {c <- Instantiations.Tables.members if c.username === "leclem"} yield c.id
      val userIDInt = q2.list.head
      println(userIDInt)
       val zeroLong: Long = 0
       val timestampOfBirthday = 1000000l
       val genderInt = -1
       val verified = -1
       val id_typepaper = -1

       //We first create the memberinfo entry
       val insert = (userIDInt, "clement",  "moutet",
           "clement.mouter@epfl.ch", "CITY", "PRESENTATION", "ADDRESS", new java.sql.Timestamp(timestampOfBirthday),
           genderInt, "LINK", "LOCATION", "LANG", verified, id_typepaper, "ID_PASSPORT",
           new java.sql.Timestamp(System.currentTimeMillis()),
           Some(new java.sql.Timestamp(System.currentTimeMillis())), zeroLong, 0)
       Instantiations.Tables.membersInfo += insert

       //We then create the profile picture entry
       //(id, picture_url, picture_offx, picture_offx, picturemin_zoom, picturemin_offx, picturemin_offy, picture_zoom)
       Instantiations.Tables.membersProfilePicture += (userIDInt, "picture url", -1, -1, "", -1, -1, "")

       //And then we create an external ids entry, for ids used by external companies
       val idFacebook = -1
       val idTwitter = -1
       val idGoogle = -1
       Instantiations.Tables.membersExternalIds += (userIDInt, idFacebook, idTwitter, idGoogle)
       ActionRepport.actionDone(userIDInt, 0, userIDInt, userIDInt, 0, null)*/


       /*CREATION OF LECLEM2 ACCOUNT*/
     /*Instantiations.Tables.members += (None, "moutet", "leclem2", "pass")
     println((for {c <- Instantiations.Tables.members if c.username === "leclem2"} yield c.id ).list.head)
       val q2 = for {c <- Instantiations.Tables.members if c.username === "leclem2"} yield c.id
      val userIDInt = q2.list.head
      println(userIDInt)
       val zeroLong: Long = 0
       val timestampOfBirthday = 1000000l
       val genderInt = -1
       val verified = -1
       val id_typepaper = -1

       //We first create the memberinfo entry
       val insert = (userIDInt, "clement",  "moutet",
           "clement.mouter@epfl.ch", "CITY", "PRESENTATION", "ADDRESS", new java.sql.Timestamp(timestampOfBirthday),
           genderInt, "LINK", "LOCATION", "LANG", verified, id_typepaper, "ID_PASSPORT",
           new java.sql.Timestamp(System.currentTimeMillis()),
           Some(new java.sql.Timestamp(System.currentTimeMillis())), zeroLong)
       Instantiations.Tables.membersInfo += insert

       //We then create the profile picture entry
       //(id, picture_url, picture_offx, picture_offx, picturemin_zoom, picturemin_offx, picturemin_offy, picture_zoom)
       Instantiations.Tables.membersProfilePicture += (userIDInt, "picture url", -1, -1, "", -1, -1, "")

       //And then we create an external ids entry, for ids used by external companies
       val idFacebook = -1
       val idTwitter = -1
       val idGoogle = -1
       Instantiations.Tables.membersExternalIds += (userIDInt, idFacebook, idTwitter, idGoogle)
       ActionRepport.actionDone(userIDInt, 0, userIDInt, userIDInt, 0, null)*/

      //*
     /*controllers.Instantiations.Tables.concepts.ddl.drop
    controllers.Instantiations.Tables.conceptsOpinions.ddl.drop*/
      //sqlu"DROP TABLE CONCEPTOPINIONS".first
      //sqlu"DROP TABLE CONCEPT".first
     /* controllers.Instantiations.Tables.groupMembership.ddl.drop
     controllers.Instantiations.Tables.groupApplication.ddl.drop
      controllers.Instantiations.Tables.groups.ddl.drop*/
      //
     //
      /*
      *
     controllers.Instantiations.Tables.members.ddl.create
     controllers.Instantiations.Tables.concepts.ddl.create
     controllers.Instantiations.Tables.conceptsOpinions.ddl.create
     */
    /* controllers.Instantiations.Tables.groups.ddl.create
     controllers.Instantiations.Tables.groupMembership.ddl.create
     controllers.Instantiations.Tables.groupApplication.ddl.create*/
    /* controllers.Instantiations.Tables.membersInfo.ddl.create
      controllers.Instantiations.Tables.doingAction.ddl.create
      controllers.Instantiations.Tables.debate.ddl.create
      controllers.Instantiations.Tables.debateYesOrNo.ddl.create
      controllers.Instantiations.Tables.voteYesOrNo.ddl.create*/
      /*In case a delete doesn't work, use the plain query/brute force solution :
      sqlu"DROP TABLE VOTEDEBATEYESORNO".first
      sqlu"DROP TABLE DEBATEYESORNO".first
      sqlu"DROP TABLE PETITIONJOIN".first*/
      //
    }
    /*request.session.get("language").map { language =>
      }.getOrElse {
      Ok(views.html.index("")(Instantiations.getLang(request)))
    }*/

    request.session.get("username").map { username =>
      Ok(views.html.connected.index("")(request.session, Instantiations.getLang(request)))
      }.getOrElse {
        val userAgent = request.headers.get("User-Agent").getOrElse("").toLowerCase()
        if (userAgent.contains("mobi")) {
          Ok(views.html.mobile("")(Instantiations.getLang(request)))
        } else {
          Ok(views.html.index("")(Instantiations.getLang(request)))
        }
    }
  }
  
  def getMembersSubcribed(): Long = {
    val numberOfMembers = Cache.getAs[Long]("numberOfMembers")
    if (numberOfMembers != None){
      numberOfMembers.get
    }
    else{
      Instantiations.Tables.db.withSession { implicit session =>
        val numberOfMembers: Long = Instantiations.Tables.members.length.run
        Cache.set("numberOfMembers", numberOfMembers, Application.cacheDuraction)
        numberOfMembers
      }
    }
  }

  def indexFR() = Action {
    request =>
      Instantiations.Tables.db.withSession { implicit session =>
        request.session.get("username").map { username =>
          Ok(views.html.connected.index("")(request.session, play.api.i18n.Messages.Implicits.applicationMessages(Lang("fr"), play.api.Play.current))).withSession("language" -> "fr")
        }.getOrElse {
          Ok(views.html.index("")(play.api.i18n.Messages.Implicits.applicationMessages(Lang("fr"), play.api.Play.current))).withSession("language" -> "fr")
        }
    }
  }

  def indexES() = Action {
    request =>
      Instantiations.Tables.db.withSession { implicit session =>
        request.session.get("username").map { username =>
          Ok(views.html.connected.index("")(request.session, play.api.i18n.Messages.Implicits.applicationMessages(Lang("es"), play.api.Play.current))).withSession("language" -> "es")
        }.getOrElse {
          Ok(views.html.index("")(play.api.i18n.Messages.Implicits.applicationMessages(Lang("es"), play.api.Play.current))).withSession("language" -> "es")
        }
    }
  }

  def indexEN() = Action {
    request =>
      Instantiations.Tables.db.withSession { implicit session =>
        request.session.get("username").map { username =>
          Ok(views.html.connected.index("")(request.session, play.api.i18n.Messages.Implicits.applicationMessages(Lang("en"), play.api.Play.current))).withSession("language" -> "en")
        }.getOrElse {
          Ok(views.html.index("")(play.api.i18n.Messages.Implicits.applicationMessages(Lang("en"), play.api.Play.current))).withSession("language" -> "en")
        }
    }
  }

  def indexRedirect(redirectPath: String) = Action {
    request =>
    Instantiations.Tables.db.withSession { implicit session =>
      request.session.get("username").map { username =>
      Redirect(redirectPath)
      }.getOrElse {
      Ok(views.html.index("", redirectPath)(Instantiations.getLang(request)))
    }
  }
  }
  
  def about = Connection.withConnection({
      username => userID => implicit request => implicit lang =>
      Ok(views.html.about("", true)(request.session, Instantiations.getLang(request)))
    }, Option((request: Request[AnyContent]) => {Ok(views.html.about("", false)(request.session, Instantiations.getLang(request)))}) )

  def thanksfordonation = Connection.withConnection({
      username => userID => implicit request => implicit lang =>
      Ok(views.html.thanksfordonation("", true)(request.session, Instantiations.getLang(request)))
    }, Option((request: Request[AnyContent]) => {Ok(views.html.thanksfordonation("", false)(request.session, Instantiations.getLang(request)))}) )


  def conditions = Action {
    request =>
    Ok(views.html.conditions("")(Instantiations.getLang(request)))
  }

  def subscription = Action {
    request =>
    Ok(views.html.subscription("")(Instantiations.getLang(request)))
  }

  def blog = Action {
    request =>
      request.session.get("username").map { username =>
      Ok(views.html.main(Instantiations.getLang(request), true, "Le blog de Democras", None)(play.twirl.api.Html(Source.fromURL("https://blog.democras.com").mkString))(request.session))
      }.getOrElse {
      Ok(views.html.main(Instantiations.getLang(request), false, "Le blog de Democras", None)(play.twirl.api.Html(Source.fromURL("http://blog.democras.com").mkString))(request.session))
    }
    //@(lang: Messages, connected: Boolean, title: String = "Democras", boldMenu: Option[String] = None)(content: Html)(implicit session: play.api.mvc.Session)
    
  }

  def saveMail(mail: String) = Action {
    request =>

      Instantiations.Tables.db.withSession { implicit session =>
        Instantiations.Tables.mailsUnsubscribed += mail
      }
      Ok(views.html.index("Vous avez bien été désinscrit de la liste de mail. Vous ne recevrez plus de messages de notre part.")(Instantiations.getLang(request)))
  }

  def forget(hash: String) = Action {
    request =>
      implicit val messages = Instantiations.getLang(request)
      Instantiations.Tables.db.withSession { implicit session =>
        val usernameList = Instantiations.Tables.passwordForget.filter(_.secureHash === hash).list
    	  if (usernameList != Nil && usernameList.head._3.getTime() > (System.currentTimeMillis()-86400000)){
    	    Ok(views.html.forget("", hash)(Instantiations.getLang(request)))
    	  }
    	  else{
    	    Ok(views.html.index(Messages("password_link_invalid"))(Instantiations.getLang(request)))
    	  }
      }
  }

  def mobile = Action {
    request =>
      request.session.get("username").map { username =>
        Ok(views.html.connected.index("")(request.session, Instantiations.getLang(request)))
      }.getOrElse {
        Ok(views.html.mobile("")(Instantiations.getLang(request)))
      }
  }
  
  def mobileApp = Action {
    request =>
      request.session.get("username").map { username =>
        Ok(views.html.connected.index("")(request.session, Instantiations.getLang(request)))
      }.getOrElse {
        Ok(views.html.mobile("", isApp = true)(Instantiations.getLang(request)))
      }
  }

  def press = Action {
    request =>
      Ok(views.html.press("")(Instantiations.getLang(request)))

  }

  //Connected
  def contact = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.contact(""))
    }
  }
  def invite = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.invite(""))
    }
  }
  
  def share = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.share(""))
    }
  }

  def tour = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.tour(""))
    }
  }

  def help = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.help(""))
    }
  }

  def concept(message: String = "") = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
        Ok(views.html.connected.concept(Messages(message)))
    }
  }

  def group(message: String = "") = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.group(Messages(message)))
    }
  }

  //The internal member page
  def memberInfo = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      val memberInfo = User.getMemberInfoFromID(Integer.parseInt(userID))
      Ok(views.html.connected.member("", memberInfo))
    }
  }

  //To display a member
  def memberDisplayLong(userWantedID: Long) = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      if (userID.toLong == userWantedID){
        val memberInfo = User.getMemberInfoFromID(Integer.parseInt(userID))
        Ok(views.html.connected.member("", memberInfo))
      }
      else if (User.gettUsercameFromID(userWantedID) != null){
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
        ActionRepport.addView(0, userWantedID)
	      val memberInfo = User.getMemberInfoFromID(userWantedID)
	      Ok(views.html.connected.displayMember("", memberInfo))
      }
      else{
        Redirect(routes.Application.index)
      }
    }
  }

  def memberDisplayString(userWantedString: String) = {
    //we test if the character is a number :
    def isalldigits = userWantedString forall Character.isDigit
    if (isalldigits){
      memberDisplayLong(userWantedString.toLong)
    }
    else{
      def userID = User.getMemberIdFromPseudo(userWantedString)
      memberDisplayLong(userID)
    }
  }

  //To display the debate page
  def debate(message: String = "") = {
    Connection.withConnection({
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.debate(Messages(message)))
    }, Option((request: Request[AnyContent]) => {Ok(views.html.connected.debate("", false)(request.session, Instantiations.getLang(request)))}) )
  }
  //To display a "Yes or No" debate
  def debateYEsOrNo(debateID: Integer) = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.debate(""))
    }
  }

  //To display the Actions page
  def action(message: String = "") = {
    Connection.withConnection({
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.action(Messages(message)))
    },  Option((request: Request[AnyContent]) => {Ok(views.html.connected.action("", false)(request.session, Instantiations.getLang(request)))}))
  }

  //To display the Actions page
  def actionWithLat(defaultLat: Double, defaultLong: Double, message: String = "") = {
    Connection.withConnection({
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.action(Messages(message), true, defaultLat, defaultLong))
    },  Option((request: Request[AnyContent]) => {Ok(views.html.connected.action("", false, defaultLat, defaultLong)(request.session, Instantiations.getLang(request)))}))
  }

  //To display the Petitions page
  def petition(message: String = "") = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.petition(Messages(message)))
    }
  }

  //To display the Conversation page
  def conversation = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.conversation(""))
    }
  }

  //To display the Login page
  def login = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.login(""))
    }
  }

  def settings = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.settings(""))
    }
  }

  //To display the notifications page
   //The internal member page
  def notification = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      val memberInfo = User.getMemberInfoFromID(Integer.parseInt(userID))
      Ok(views.html.connected.notification(""))
    }
  }

  //----------------------------SEARCH ----------------------
  def search = {
    Connection.withConnection{
      username => userID => implicit request => implicit lang =>
      Ok(views.html.connected.search(""))
    }
  }

  case class SearchClass(typeEntry: String, id: Long, name: String, info: String, modificationTimestamp: Long)

  val searchWrites: Writes[SearchClass] = {
      ((JsPath \ "typeEntry").write[String] and
      (JsPath \ "id").write[Long] and
      (JsPath \ "name").write[String] and
      (JsPath \ "info").write[String] and
      (JsPath \ "modificationTimestamp").write[Long])(unlift(SearchClass.unapply))
    }

  def searchNothing(maxTimestamp: Long, number: Int) = searchOrderedByTimestampJSON("", maxTimestamp: Long, number: Int)

  def searchOrderedByTimestampJSON(searchentry: String, maxTimestamp: Long, number: Int) = Connection.withConnection({
      username => userID => implicit request => implicit lang =>
        searchOrderedByTimestampJSONsub(userID, searchentry, maxTimestamp, number)
  }, Option((request: Request[AnyContent]) => searchOrderedByTimestampJSONsub("-1", searchentry, maxTimestamp, number, request)))

  def searchOrderedByTimestampJSONsub(userID: String, searchentry: String, maxTimestamp: Long, number: Int, request: Request[AnyContent] = null) = {
        Instantiations.Tables.db.withSession { implicit session =>
          if (number < maxResults) {
            val userIDLong = userID.toLong
            val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
            //For the elements that can be created by a group, we display everything except the objects created by a private group

            val actionsFiltered = for { c <- Instantiations.Tables.actions if ((c.name like "%" + searchentry + "%") || (c.definition like "%" + searchentry + "%")) && c.logicalDelete === 0 && c.modificationTimestamp < maxTimestampParam} yield c

            val privateGroupsWhereUserIsMember = for {
              (groupMembership, group) <- Instantiations.Tables.groupMembership.filter(_.memberId === userIDLong) innerJoin Instantiations.Tables.groups.filter(x => x.privacy === 1 && x.logicalDelete === 0) on ((groupMembership, group) =>
                groupMembership.groupId === group.id
                )} yield group

            /*only display action from public groups and created by member*/
            val searchQueryAction = for {
              (action, group) <- actionsFiltered leftJoin Instantiations.Tables.groups on ((action, group) =>
                  (action.creatorID === group.id ))
                } yield ("action", action.id, action.name, action.definition, action.language, action.creatorType, group.privacy?, group.logicalDelete?, action.modificationTimestamp)
            val searchQueryActionPublic = for { x <- searchQueryAction if ((x._6 === 0 || (x._6 === 1 && x._7 === 0 && x._8 === 0)))} yield (x._1, x._2, x._3, x._4, x._5, x._9)
            /* We add to it the actions from private groups where the user is member*/
            val searchQueryActionPrivate = for {
              (action, group) <- actionsFiltered.filter(_.creatorType === 1) innerJoin privateGroupsWhereUserIsMember on ((action, group) =>
                  (action.creatorID === group.id ))
                } yield ("action", action.id, action.name, action.definition, action.language, action.modificationTimestamp)
            val searchQueryActionFiltered = searchQueryActionPublic ++ searchQueryActionPrivate

            val petitionsFiltered = for { c <- Instantiations.Tables.petition if ((c.name like "%" + searchentry + "%") || (c.definition like "%" + searchentry + "%")) && c.logicalDelete === 0 && c.modificationTimestamp < maxTimestampParam} yield c
            /*only display petition from public groups*/
            val searchQueryPetition = for {
              (petition, group) <- petitionsFiltered leftJoin Instantiations.Tables.groups on ((petition, group) =>
                  (petition.creatorID === group.id ))
                } yield ("petition", petition.id, petition.name, petition.definition, petition.language, petition.creatorType,  group.privacy?, group.logicalDelete?, petition.modificationTimestamp)
            val searchQueryPetitionPublic = for { x <- searchQueryPetition if (x._6 === 0 || (x._6 === 1 && x._7 === 0 && x._8 === 0))} yield (x._1, x._2, x._3, x._4, x._5, x._9)
            /* We add to it the petitions from private groups where the user is member*/
            val searchQueryPetitionPrivate = for {
              (petition, group) <- petitionsFiltered.filter(_.creatorType === 1) innerJoin privateGroupsWhereUserIsMember on ((petition, group) =>
                  (petition.creatorID === group.id ))
                } yield ("petition", petition.id, petition.name, petition.definition, petition.language, petition.modificationTimestamp)
            val searchQueryPetitionFiltered = searchQueryPetitionPublic ++ searchQueryPetitionPrivate

            val debatesFiltered = for { c <- Instantiations.Tables.debate if ((c.question like "%" + searchentry + "%") && c.logicalDelete === 0 && c.modificationTimestamp < maxTimestampParam)} yield c
            /*only display debates from public groups*/
            val searchQueryDebates = for { (debate, group) <- debatesFiltered leftJoin Instantiations.Tables.groups on ( (debate, group) =>
              (debate.privateGroupId === group.id))
              } yield ("debate", debate.id, debate.question, debate.typeOfDebate.asColumnOf[String], debate.language, debate.privateGroupId, group.privacy?, group.logicalDelete?, debate.modificationTimestamp)
            val searchQueryDebatesPublic = (for { x <- searchQueryDebates if (x._6 === -1l || (x._7 === 0 && x._8 === 0))} yield (x._1, x._2, x._3, x._4, x._5, x._9))/*.as[(String, Long, String, String, String)]*/
            /* We add to it the debates from private groups where the user is member*/
            val searchQueryDebatesPrivate = for { (debate, group) <- debatesFiltered innerJoin privateGroupsWhereUserIsMember on ( (debate, group) =>
              (debate.privateGroupId === group.id))
              } yield ("debate", debate.id, debate.question, debate.typeOfDebate.asColumnOf[String], debate.language, debate.modificationTimestamp)
            val searchQueryDebatesFiltered = searchQueryDebatesPublic ++ searchQueryDebatesPrivate

            val searchQueryConcepts = for { c <- Instantiations.Tables.concepts if ((c.name like "%" + searchentry + "%") || (c.definition like "%" + searchentry + "%")) && c.logicalDelete === 0 && c.modificationTimestamp < maxTimestampParam } yield ("concept", c.id, c.name, c.definition, c.language, c.modificationTimestamp)

            val searchQueryGroups = for { c <- Instantiations.Tables.groups if ((c.name like "%" + searchentry + "%") || (c.definition like "%" + searchentry + "%")) && c.logicalDelete === 0 && c.modificationTimestamp < maxTimestampParam} yield ("group", c.id, c.name, c.definition, c.language, c.modificationTimestamp)

            val searchQueryMembers = for {(member, memberInfo) <- Instantiations.Tables.members innerJoin Instantiations.Tables.membersInfo on (
                (member, memberInfo) =>
                  ((member.username like "%" + searchentry + "%") ||
                      (memberInfo.firstname like "%" + searchentry + "%") ||
                      (memberInfo.name like "%" + searchentry + "%") ||
                      (memberInfo.ville like "%" + searchentry + "%") ||
                      (memberInfo.presentation like "%" + searchentry + "%") ||
                      (memberInfo.address like "%" + searchentry + "%")  )&&
                  memberInfo.modificationTimestamp < maxTimestampParam &&
                  member.id === memberInfo.id)
            } yield ("member", member.id, member.username, memberInfo.firstname ++ " " ++ memberInfo.name, memberInfo.lang, memberInfo.modificationTimestamp)

            var searchUnified = /*searchQueryActionFiltered ++ */searchQueryConcepts ++ searchQueryDebatesFiltered ++ searchQueryGroups /*++ searchQueryPetitionFiltered*/ ++ searchQueryMembers

            if (userIDLong == -1){
              val languageToSearch = Instantiations.getLangText(request)
              searchUnified = searchQueryConcepts ++ searchQueryDebatesFiltered ++ searchQueryGroups ++ searchQueryPetitionFiltered
              searchUnified = searchUnified.filter(x => x._5 === languageToSearch)
            }

            implicit val searchWritess = searchWrites
            val searchResults = searchUnified.sortBy(_._6.desc).take(number).list.par.map((x: (String, Long, String, String, String, java.sql.Timestamp)) => SearchClass(x._1, x._2, x._3, x._4, x._6.getTime())).seq
            Ok(Json.toJson(searchResults))
          }
          else{
            BadRequest
          }
        }
        /*val searchQuery = for {
		      ((petition, member), group) <-
	    	  (Instantiations.Tables.petition outerJoin Instantiations.Tables.members on
		        ((petition, member) =>
		          petition.name like "%" + searchentry + "%")) outerJoin Instantiations.Tables.groups on (_._2.name like "%" + searchentry + "%")
          } yield (petition, member.username?, group.name?)*/


  }

  def timestampToSearch(timestamp: Long) = {
    if (timestamp == -1l){
      System.currentTimeMillis() + 1000
    }
    else{
      timestamp
    }
  }

  def outputHTMLText(text: String, authoriseLineBreaks: Boolean) = {
    if (authoriseLineBreaks)
      text.replace("\n", "")
    else
      text
  }

  /*
   * Posts a tweet on a Twitter account using an app
   */
  def postTweetOnAccount(tweet: String, consumerKey: String, consumerSecret: String, accessToken: String, accessSecret: String) = {
    try {
    println("posting:" + tweet)

    if (tweet.length <= 140 && activateTwitterPosting){

      val cb = new ConfigurationBuilder();
      cb.setDebugEnabled(true).setOAuthConsumerKey(consumerKey).setOAuthConsumerSecret(consumerSecret).setOAuthAccessToken(accessToken).setOAuthAccessTokenSecret(accessSecret);
      val twitter = new TwitterFactory(cb.build()).getInstance();
      val status = twitter.updateStatus(tweet);
    }
  } catch {
     case e: Exception => println("exception postingtweet")
   }
    /*val accessTokenObj = new AccessToken(accessToken, accessSecret);
    val authorization = new OAuthAuthorization(ConfigurationContext.getInstance(), consumerKey, consumerSecret, accessToken);*/
  }


  /*
   * Posts a tweet on the right twitter account for an object
   */
  def postOnTwitter(objectId : Long, objectTitle: String, objectPath: String, consumerKey: String, consumerSecret: String, accessToken: String, accessSecret: String) = {
    Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      Instantiations.Tables.db.withSession { implicit session =>
        //We post this action on twitter
        var titleToPost = objectTitle
        if (objectTitle.length >=  116){
          titleToPost = objectTitle.slice(0, 107) + " (...)"
        }
    
        val tweet = titleToPost + " - democras.com" + objectPath + "/" + objectId //25 characters
        Application.postTweetOnAccount(tweet, consumerKey, consumerSecret, accessToken, accessSecret)
      }
    }
  }
  
  /*
   * Runs an anonymous function in parallel, in order to execute them in a separate pool
   */
  def parallelFunction(functionToParallelise: slick.driver.MySQLDriver.backend.Session => Unit){
    val thread = new Thread(new Runnable {
      def run() {
        Instantiations.Tables.db.withSession { implicit session =>
          functionToParallelise(session)
        }
      }
    })
    thread.start;
  }

}
