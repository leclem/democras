package controllers
import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.i18n._
import play.api.i18n.I18nSupport
import play.api.Play.current
import play.api.cache.Cache;
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.slick.driver.MySQLDriver.simple._
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import scala.slick.jdbc.JdbcBackend.Database
import Q.interpolation
import org.mindrot.jbcrypt.BCrypt;
import play.api.libs.mailer._
import javax.inject.Inject
import play.api.inject.guice.GuiceApplicationBuilder
import scala.util.Random
import scalaj.http.Http

import org.jsoup._
import org.jsoup.safety.Whitelist

object Mailer {
  
  val injector = new GuiceApplicationBuilder().injector
  val mailerClient:MailerClient = injector.instanceOf[MailerClient]
  
  def sendEmail(receiverName: String, receiver: String, title: String, message: String) {
    Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
      val receiverline = receiverName + "<" + receiver + ">"
      val email = Email(
        title,
        "Democras <contact@democras.com>",
        Seq(receiverline),
        // adds attachment
        /*attachments = Seq(
          AttachmentFile("attachment.pdf", new File("/some/path/attachment.pdf")),
          // adds inline attachment from byte array
          AttachmentData("data.txt", "data".getBytes, "text/plain", Some("Simple data"), Some(EmailAttachment.INLINE)),
          // adds cid attachment
          AttachmentFile("image.jpg", new File("/some/path/image.jpg"), contentId = Some(cid))
        ),*/
        // sends text, HTML or both...
        bodyText = Some(message),
        bodyHtml = Some("<html><body><p>" + message + "</p></body></html>")
      )
      mailerClient.send(email)
    }
  }
}


object Connection extends Controller{
//val mailer = new Mailer
  def processConnectionForm(redirection: String = "") = Action { implicit request =>
	 models.Forms.connectionForm.bindFromRequest.value map { connectionForm =>
	   val connectionRequest = connectionControl(connectionForm.username , connectionForm.password )
	   if (connectionRequest._1){
         //CONNECTED

	     //implicit val session = request.session
	     //implicit val lang = Instantiations.getLang(request)
	     //We check if this is the first connection
	     Instantiations.Tables.db.withSession { implicit session =>
  	     val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === connectionRequest._2} yield (c.firstconnection, c.numberOfActions, c.hasAlreadySeenInviteFriends)
  	     if (q2.list.head._1 == 0){
  	       Ok(views.html.connected.tour(Messages(connectionRequest._3)(Instantiations.getLang(request)))(request.session, Instantiations.getLang(request))).withSession(
  	         "userID" -> (connectionRequest._2 + ""),
  	         "username" -> connectionRequest._4,
  	         "language" -> Instantiations.getLangText(request)
  	       )
  	     }
  	     else{
  	       if (q2.list.head._2 > 1 && q2.list.head._3 == 0){ //If the member has not seen the page "invite my friends" and has already did an action
  	         q2.update((1, q2.list.head._2, 1))
  	         Ok(views.html.connected.share(Messages(connectionRequest._3)(Instantiations.getLang(request)), true)(request.session, Instantiations.getLang(request))).withSession(
  	         "userID" -> (connectionRequest._2 + ""),
  	         "username" -> connectionRequest._4,
  	         "language" -> Instantiations.getLangText(request)
  	         )
  	       }
  	       else{
  	         Redirect(redirection).withSession(
    	       "userID" -> (connectionRequest._2 + ""),
    	       "username" -> connectionRequest._4,
    	       "language" -> Instantiations.getLangText(request)
    	     )
  	       }
  	     }
	     }
	   }
     else{
       
         implicit val messages = Instantiations.getLang(request)
         if (redirection == "" || redirection == "/"){
           Ok(views.html.index(Messages(connectionRequest._3)))
         }
         else{
           Redirect(redirection);
         }
       }
    } getOrElse BadRequest
  }
/*
 * "firstname" -> text,
    "name" -> text,
    "gender" -> text,
    "birthday" -> text,
    "city" -> text,
    "idfb" -> text,
    "address" -> text,
    "location" -> text,
    "lang" -> text,
    "mail" -> text,
    
    "username" -> text,
    "password" -> text,
    "link" -> text,
    "presentation" -> text,
    
    "pictureu" -> text,
    "picturex" -> text,
    "picturey" -> text,
    "picturem" -> text,
    "picturemx" -> text,
    "picturemy" -> text,
    "picturemm" -> text
 */
  
  def processFBConnection(token: String, redirection: String) = Action {
    implicit request =>
    try{
      //"https://graph.facebook.com/v2.7/me?fields=id,name,email&access_token=" + token
      val result = Http("https://graph.facebook.com/v2.7/me?fields=id,name,email&access_token=" + token).asString;
      val response = Json.parse(result.body);
    
    val idFacebook = (response \ "id").as[String].toLong;
    val name = (response \ "name").as[String];
    val email = (response \ "email").as[String];
    Instantiations.Tables.db.withSession { implicit session =>
      var userIDs = Instantiations.Tables.membersExternalIds.filter(_.idfacebook === idFacebook).list
      
      if (userIDs == Nil){
        //We have to subscribe the user first
        //We search for an username
        val splittedName = name.split(" ");
        var username = splittedName(0).replaceAll("\\W", "")
        var usernameInDatabse = Instantiations.Tables.members.filter(_.username === username).list
        if (usernameInDatabse != Nil){
          var numberAfter = 1;
          var newUsername = username + "" + numberAfter
          while (usernameInDatabse != Nil){
            newUsername = username + "" + numberAfter
            usernameInDatabse = Instantiations.Tables.members.filter(_.username === newUsername).list
            numberAfter += 1
          }
          username = newUsername
        }
        processSubscriptionLoginFormsub(username, email, "KOFDKGODFBDSFEfeforf243432212332589785443orgo121323ROFKFDFFCDSOCFESDQSDQSO2E231223RO4RO4SVOXWSXKOQKDQSFDSFDOV", Instantiations.getLangText(request), Instantiations.getLang(request), idFacebook)
        userIDs = Instantiations.Tables.membersExternalIds.filter(_.idfacebook === idFacebook).list
        Instantiations.Tables.db.withSession { implicit session =>
	        val q5 = for {c <- Instantiations.Tables.membersInfo if c.id === userIDs.head._1} yield (c.firstname, c.name, c.firstconnection)
	        q5.update(splittedName(0), splittedName(1), 1)
	      }
      }
      
      val result = Http("https://api.facebook.com/restserver.php?method=auth.expireSession&format=json&access_token=" + token).asString;
      //We then connect the user
      val userId = userIDs.head._1
      val usernameOfUser = User.gettUsercameFromID(userId)
      //We check if this is the first connection
	     
  	   
  	   Redirect(redirection).withSession(
    	   "userID" -> (userId + ""),
    	   "username" -> usernameOfUser,
         "language" -> Instantiations.getLangText(request)
    	 )
    	 
    }
    } catch {
      case e: Exception => Ok(views.html.index(Messages("views.signinfberror")(Instantiations.getLang(request)))(Instantiations.getLang(request)))
    }
  }
  
  def processSubscriptionFBForm = Action { implicit request =>
	 models.Forms.subscriptionFbForm.bindFromRequest.value map { subscriptionFBForm =>
     //We first check if the user is already in the database
	   Instantiations.Tables.db.withSession { implicit session =>
	     println("Posted a valid FBSUBRSCRIPTIONPOST")
	     val q2 = for {c <- Instantiations.Tables.members if c.username === subscriptionFBForm.username} yield c.id
         if (q2.list == Nil){
	       Instantiations.Tables.members += (None, subscriptionFBForm.name, subscriptionFBForm.username, BCrypt.hashpw(subscriptionFBForm.password, BCrypt.gensalt(12)))
	       Cache.remove("numberOfMembers")
	       //val qID = for {c <- Instantiations.Tables.members if c.username === subscriptionForm.username} yield c.id
	       val userIDInt = q2.list.head
	       val zeroLong: Long = 0
	       /*(id, firstname, name, mail, ville, presentation, address, birthday,
	       gender, link, location, lang, verified, id_typepaper,
	       id_passport, modificationTimestamp, creationTimestamp?,
	       numberOfActions, complains, ldel)
           */
	       val timestampOfBirthday = subscriptionFBForm.birthday.toLong
	       val genderInt = subscriptionFBForm.gender.toInt
	       val verified = -1
	       val id_typepaper = -1
         val city  = subscriptionFBForm.city
         val address = subscriptionFBForm.address
         val link = subscriptionFBForm.link
         val presentation = subscriptionFBForm.presentation
         val location = subscriptionFBForm.location
         val plang = subscriptionFBForm.lang


	       //We first create the memberinfo entry
	       val insert = (userIDInt, subscriptionFBForm.firstname, subscriptionFBForm.name, subscriptionFBForm.mail, city, Jsoup.clean(presentation, Application.whiteListHTML), address, new java.sql.Date(timestampOfBirthday),
	           genderInt, link, location, plang, verified, id_typepaper, "ID_PASSPORT",
	           new java.sql.Timestamp(System.currentTimeMillis()),
	           Some(new java.sql.Timestamp(System.currentTimeMillis())), zeroLong, 1, 1, 0)
	       Instantiations.Tables.membersInfo += insert
         val pictureu = subscriptionFBForm.pictureu //url
         val picturex = subscriptionFBForm.picturex.toInt //offx
         val picturey = subscriptionFBForm.picturey.toInt
         val picturem = subscriptionFBForm.picturem //zoom : margin string
         val picturemx = subscriptionFBForm.picturemx.toInt //min start
         val picturemy = subscriptionFBForm.picturemy.toInt
         val picturemm = subscriptionFBForm.picturemm
	       //We then create the profile picture entry
	       //(id, picture_url, picture_offx, picture_offy, picture_zoom, picturemin_offx, picturemin_offy, picturemin_zoom)
	       //IMPORTANT Instantiations.Tables.membersProfilePicture += (userIDInt, pictureu, picturex, picturey, picturem, picturemx, picturemy, picturemm)
           Instantiations.Tables.membersProfilePicture += ((userIDInt, pictureu, picturex, picturey, picturem, picturemx, picturemy, picturemm))
	       //And then we create an external ids entry, for ids used by external companies
	       val idFacebook = subscriptionFBForm.idfb.toInt
	       val idTwitter = -1
	       val idGoogle = -1
	       Instantiations.Tables.membersExternalIds += (userIDInt, idFacebook, idTwitter, idGoogle)
	       Instantiations.Tables.membersLanguage += (userIDInt, request.session.get("language").get)
	       ActionRepport.actionDone(userIDInt, 0, userIDInt, userIDInt, 0, null)

           /*implicit val messages = Instantiations.getLang(request)
	         Ok(views.html.connected.index(Messages("subscription.successful")))*/
	         Ok(Json.toJson(1))

         }
         else{

           /*implicit val messages = Instantiations.getLang(request)
           Ok(views.html.index(Messages("subscription.username_already_exists")))*/
           Ok(Json.toJson(0))
         }
	   }
   } getOrElse BadRequest
  }
  
  /*
   * PRocess the basic subscription. Returns 1 if succeeded, 2 if username already exists, 3 if mai lalready exists
   */
  def processSubscriptionLoginForm = Action { implicit request =>
    var jsonToSend = 0
	  models.Forms.userSubcriptionLoginForm.bindFromRequest.value map { userSubcriptionLoginForm =>
	    implicit val messagespar = Instantiations.getLang(request)
      
	     jsonToSend = processSubscriptionLoginFormsub(userSubcriptionLoginForm.username, userSubcriptionLoginForm.mail, userSubcriptionLoginForm.password, userSubcriptionLoginForm.lang, messagespar, -1)

    }
    Ok(Json.toJson(jsonToSend));
  }
  
  def processSubscriptionLoginFormsub(username: String, mail: String, password: String, lang: String, messagespar: Messages, fbID: Long) = { 
      implicit val messages = messagespar
	    var jsonTOSend = 0
	    
	    Instantiations.Tables.db.withSession { implicit session =>
	      //We first check if the user is already in the database
	      val q2 = for {c <- Instantiations.Tables.members if c.username === username} yield c.id
	      val q3 = for {c <- Instantiations.Tables.membersInfo if c.mail === mail} yield c.id
	      val userIDFromMail = q3.list
	      if (q2.list != Nil){
	        jsonTOSend = 2
	      }
	      else if (userIDFromMail != Nil){
	        
	        /*
	         * If the mail already exists, either it is a connect with facebook with an already used address (fbId = his id), or just
	         * an user that wants to create a multiaccount (fbId = -1, normal)
	         */
	        val userId = userIDFromMail.head
	        val q5 = for {c <- Instantiations.Tables.membersExternalIds if c.id === userId} yield (c.idfacebook)
	        q5.update(fbID)
	        
	        jsonTOSend = 3
	      }
	      else if (Application.langs.contains(lang) && username.matches(Application.regexUsername)){
	        
	        Instantiations.Tables.members += (None, "", username, BCrypt.hashpw(password, BCrypt.gensalt(12)))
	        Cache.remove("numberOfMembers")
	        val userIDLong = q2.list.head
	        
	        //We create memberinfo
	        val insertMember = (userIDLong, "", "", mail, "", "", "", new java.sql.Date(System.currentTimeMillis()),
	           -1, "", "", lang, -1, -1, "ID_PASSPORT",
	           new java.sql.Timestamp(System.currentTimeMillis()),
	           Some(new java.sql.Timestamp(System.currentTimeMillis())), 0l, 0, 1, 0)
	        Instantiations.Tables.membersInfo += insertMember 
         
	        //We then create the profile picture entry
	        val q3 = for {c <- Instantiations.Tables.membersProfilePicture if c.id === userIDLong} yield c 
          val insertPicture = ((userIDLong, "", -1, -1, "", -1, -1, ""))
	        Instantiations.Tables.membersProfilePicture += insertPicture
          
	        Instantiations.Tables.membersStats += (userIDLong, 0l, 0l, 0l)
	        Instantiations.Tables.membersStatsMonthly += (userIDLong, 0l, 0l)
	        
  	      //And then we create an external ids entry, for ids used by external companies
  	      val idFacebook = fbID
  	      val idTwitter = -1
  	      val idGoogle = -1
  	      Instantiations.Tables.membersExternalIds += (userIDLong, idFacebook, idTwitter, idGoogle)
  	      Instantiations.Tables.membersLanguage += (userIDLong, lang)
  	      Mailer.sendEmail("", mail, Messages("mail.titlesubscription"), views.html.mails.subscription.render(username, messages).body) 
  	      jsonTOSend = 1
  	      
  	      ActionRepport.actionDone(userIDLong, 0, userIDLong, userIDLong, 0, null)
	      }
	    }
	    jsonTOSend
  }
  
  /*
   * PRocess subscription form. Returns 1 if subscription succeeded, 0 if the params are not ok
   */
  def processSubscriptionForm = Connection.withConnection({
       username => userID => implicit request => lang =>
         val userIDLong = userID.toLong
	 models.Forms.subscriptionForm.bindFromRequest.value map { subscriptionForm =>
	   implicit val messages = Instantiations.getLang(request)
	   Instantiations.Tables.db.withSession { implicit session =>
	     
	     val zeroLong: Long = 0
	     val verified = -1
	     val id_typepaper = -1
       val city  = subscriptionForm.city
       val address = subscriptionForm.address
       val link = subscriptionForm.link
       val presentation = subscriptionForm.presentation
       val location = subscriptionForm.location
       //val plangs: Seq[String] = subscriptionForm.lang
       /*
       println("eee");
	     println(subscriptionForm.birthday.toLong < System.currentTimeMillis());
    	       println(subscriptionForm.gender.toInt);
    	       println(city.length < Application.maxCharacternames);
    	       println(address.length < Application.maxCharacternames);
    	       println(link.length < Application.maxCharactersOpinions);
    	       println(presentation.length < Application.maxCharacterDefinitions);
    	       println( location.length  < Application.maxCharacternames);*/
	     if (//We do the size verifications
	         /*(allCatch subscriptionForm.birthday.toLong).isDefined &&
	         subscriptionForm.gender.isInt &&*/
	         subscriptionForm.birthday.toLong < System.currentTimeMillis() &&
	         Application.genders.contains(subscriptionForm.gender.toInt) &&
	         city.length < Application.maxCharacternames &&
	         address.length < Application.maxCharacternames &&
	         link.length < Application.maxCharactersOpinions && 
	         presentation.length < Application.maxCharacterDefinitions && 
	         location.length  < Application.maxCharacternames //&&
	         //plangs.foldLeft(true)((x, y) => x && Application.langs.contains(y) )//We test that all the lang parameters are valid
	         /*Application.langs.contains(plang)*/){
    	       
    	       
    	       //modify the subscription
	       
    	       val timestampOfBirthday = subscriptionForm.birthday.toLong
    	       val genderInt = subscriptionForm.gender.toInt
    	       Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
              Instantiations.Tables.db.withSession { implicit session =>
      	       val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userIDLong} yield c
      	       val currentSubscription = q2.list.head
      
      	       //We first create the memberinfo entry
      	       val insert = (userIDLong, subscriptionForm.firstname, subscriptionForm.name, currentSubscription._4, city, Jsoup.clean(presentation, Application.whiteListHTML), address, new java.sql.Date(timestampOfBirthday),
      	           genderInt, link, location, currentSubscription._12, verified, id_typepaper, "ID_PASSPORT",
      	           new java.sql.Timestamp(System.currentTimeMillis()),
      	           Some(new java.sql.Timestamp(System.currentTimeMillis())), zeroLong, 1, 1, 0)
      	       q2.update(insert)
               val statement = q2.updateStatement
               val invoker = q2.updateInvoker
    	       }
    	       }
             val pictureu = ""//subscriptionPicForm.pictureu //url
             val picturex = -1//subscriptionPicForm.picturex //offx
             val picturey = -1//subscriptionPicForm.picturey
             val picturem = ""//subscriptionPicForm.picturem //zoom : margin string
             val picturemx = -1//subscriptionPicForm.picturemx //min start
             val picturemy = -1//subscriptionPicForm.picturemy
             val picturemm = ""//subscriptionPicForm.picturemm
             
    	       //We then create the profile picture entry
             Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
               
                Instantiations.Tables.db.withSession { implicit session =>
              	       val q3 = for {c <- Instantiations.Tables.membersProfilePicture if c.id === userIDLong} yield c 
                       val insertPicture = ((userIDLong, pictureu, picturex, picturey, picturem, picturemx, picturemy, picturemm))
                       q3.update(insertPicture)
                       val statement2 = q3.updateStatement
                       val invoker2 = q3.updateInvoker
                }
    	       }
         
	         Ok(Json.toJson(1))
         }
         else{

           /*implicit val messages = Instantiations.getLang(request)
           Ok(views.html.index(Messages("subscription.username_already_exists")))*/
           Ok(Json.toJson(0))
         }
	   }
   } getOrElse BadRequest
  }, None, true)



  def disconnect = Action { implicit request =>

     implicit val messages = Instantiations.getLang(request)
     val userAgent = request.headers.get("User-Agent").getOrElse("").toLowerCase()
        if (userAgent.contains("mobi")) {
          Ok(views.html.mobile(Messages("disconnection.successful"))).withSession(
  	   "language" -> Instantiations.getLangText(request)
  	 )
        } else {
          Ok(views.html.index(Messages("disconnection.successful"))).withSession(
  	   "language" -> Instantiations.getLangText(request)
  	 )
        }
     
	   
  }

  /*
   * PRocess the basic subscription. Returns 1 if succeeded, 2 if username already exists, 3 if mai lalready exists
   */
  def processUserPasswordForgetForm = Action { implicit request =>
    var jsonTOSend = 0
	  models.Forms.userPasswordForgetForm.bindFromRequest.value map { userPasswordForgetForm =>
	    implicit val messages = Instantiations.getLang(request)
	    Instantiations.Tables.db.withSession { implicit session =>
  	    //We retrieve the mail if the user exists
  	    val q2 = for {(member, memberInfo) <- Instantiations.Tables.members innerJoin Instantiations.Tables.membersInfo on (
  	      (member, memberInfo) => 
  	        member.id === memberInfo.id
  	    )} yield (memberInfo.id, memberInfo.mail, member.username)
  	    
  	    //We test for the username or the mail
  	    val q3 = q2.filter(x => x._2 === userPasswordForgetForm.username || x._3 === userPasswordForgetForm.username)
  	    
  	    val memberIDAndMailList = q3.list
  	    if (memberIDAndMailList != Nil){
  	      Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
  	        
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implmentation, because the old session will be closed when this thread requests will execute
    	      //The user exists, we start the password recuperation procedure
    	      
    	      //We first delete the potential previous hash sent in the past
    	      val q = Instantiations.Tables.passwordForget.filter(_.idMember === memberIDAndMailList.head._1).delete
    	      
    	      //We store a random string associated to this user
    	      val randomString = Random.alphanumeric.take(254).mkString
    	      controllers.Instantiations.Tables.passwordForget += (memberIDAndMailList.head._1, randomString, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())))
    	      
    	      //We then mail the user so he can reset his password
    	      //views.html.mails.passwordForget.render(randomString, username).body
    	      Mailer.sendEmail("", memberIDAndMailList.head._2, Messages("mail.titlepasswordforget"), views.html.mails.passwordForget.render(randomString, memberIDAndMailList.head._3, Instantiations.getLang(request)).body) 
    	    }
  	    }
  	      jsonTOSend = 1
  	    }
	    }
	  }
    Ok(Json.toJson(jsonTOSend))
  }
  
  //userPasswordResetForm
  def processUserPasswordResetForm = Action { implicit request =>
    var jsonTOSend = 0
	  models.Forms.userPasswordResetForm.bindFromRequest.value map { userPasswordResetForm =>
	    Instantiations.Tables.db.withSession { implicit session =>
  	    val hash = userPasswordResetForm.hash
  	    val newPassword = userPasswordResetForm.newPassword
  	    val usernameRequest = Instantiations.Tables.passwordForget.filter(_.secureHash === userPasswordResetForm.hash)
  	    val usernameList = usernameRequest.list
  	    //We have to check if the hash is correct and has been issued less than one day ago
  	    if (usernameList != Nil && usernameList.head._3.getTime() > (System.currentTimeMillis()-86400000)){
  	      Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
  	        
            Instantiations.Tables.db.withSession { implicit session => //Always a new session in case of parallel implmentation, because the old session will be closed when this thread requests will execute
    	      //We can update the password, the information provided are correct
    	      val q2 = for {c <- Instantiations.Tables.members if c.id === usernameList.head._1} yield c.password
    	      q2.update(BCrypt.hashpw(newPassword, BCrypt.gensalt(12)))
    	      val statement = q2.updateStatement
            val invoker = q2.updateInvoker
            
            //We delete the hash from the database
            usernameRequest.delete
  	      }
  	    }
          jsonTOSend = 1
  	    }
	    }
	  }
    Ok(Json.toJson(jsonTOSend))
  }
  /*
   * Checks if the credentials are OK. 
   * Returns if the connection is OK, the ID of the member, the name of the message to display, and the username of the member
   */
  def connectionControl(username: String, password: String): (Boolean, Long, String, String) = {
    var passwordInDatabase: String = "";
    var passwordEncrypted: String = "";
    var returnMessage: String = "";
    var usernameFromDatabase: String = "";

    var userExists = false
    var userId = -1l
    Instantiations.Tables.db.withSession { implicit session =>
      //We check for the associated password to the user
      
        val q2 = for {(member, memberInfo) <- Instantiations.Tables.members innerJoin Instantiations.Tables.membersInfo on (
  	      (member, memberInfo) => 
  	        member.id === memberInfo.id
  	    )} yield (memberInfo.id, member.password, memberInfo.mail, member.username)
  	    
  	    //We test for the username or the mail
  	    val q3 = q2.filter(x => x._3 === username || x._4 === username)
  	    
        val resultsList = q3.list
        if (resultsList != Nil){
          passwordInDatabase = resultsList.head._2
          //passwordEncrypted = BCrypt.hashpw(password, BCrypt.gensalt(12))
          userId = resultsList.head._1
          usernameFromDatabase = resultsList.head._4
          userExists = true
        }
    }

    //If the user doesn't exists, we return false
    if (!userExists){
      return (false, userId, "connection.user_doesnt_exist", usernameFromDatabase)
    }
    //We return true if the password provided and the password in the database are the same
    if (BCrypt.checkpw(password,passwordInDatabase))
      (true, userId, "connection.successful", usernameFromDatabase)
    else
      (false, userId,"connection.unsuccessful", usernameFromDatabase)
  }

  /*
   * A method to assure the user is connected. Usage : withConnection{username => userID => implicit request => theCODE }
   */
   def withConnection(f: => String => String => Request[AnyContent] => Messages => Result, unconnectedResult: Option[Request[AnyContent] => play.api.mvc.Result] = None, isUserWriting: Boolean = false) = {
     
     Action(request =>
     try {
       request.session.get("username").map { username =>
       request.session.get("userID").map { userID =>
   
         val userIDInt = userID.toLong
         if (!request.path.contains("json"))
           User.storePageLoadedByUser(if (unconnectedResult == None){userIDInt}else{-1l}, request.remoteAddress, request.path)
         //We update the last cactivity timestamp
         Instantiations.Tables.db.withSession { implicit session =>
           var weCanContinue = !isUserWriting
           if (isUserWriting){
             Application.parallelFunction{ implicit session: slick.driver.MySQLDriver.backend.Session =>
                 sqlu"update MEMBERSINFO set NUMBER_ACTIONS=NUMBER_ACTIONS+1 where SUP_ID = $userID".first
             }
           }
           while (!weCanContinue){
             weCanContinue = true
             try {
               //We add our action
               Instantiations.Tables.doingAction += (userIDInt, new java.sql.Timestamp(System.currentTimeMillis()))
             }
             catch {
               case mysqlexception: Exception => {
                 //We check if the last action was before a certain date. If it was, we consider it is too old and we delete it
                 val q2 = for {c <- Instantiations.Tables.doingAction if c.memberId === userIDInt} yield (c.start_timestamp)
                 var results = q2.list
                 if (results != Nil && results.head.before(new java.sql.Timestamp(System.currentTimeMillis()-(1000*31)))){
                   //If there is more than one minut wainting, we cancel the preivous execution
                   val q3 = Instantiations.Tables.doingAction.filter(_.memberId === userIDInt).delete
                 }
                 Thread.sleep(1000)
                 weCanContinue = false
               }
             }
           }
         }
           try{
             //We do our function
             val result = f(username)(userID)(request)(Instantiations.getLang(request))
             //We delete our action
             Instantiations.Tables.db.withSession { implicit session =>
               if (isUserWriting){
                 println("request")
                 val q3 = Instantiations.Tables.doingAction.filter(_.memberId === userIDInt).delete
               }
             }
             result
           } catch {
               //Something didn't work in the function. We remove the "doingAction" and return an error
               case someException: Exception => {
                 Instantiations.Tables.db.withSession { implicit session =>
                   val q3 = Instantiations.Tables.doingAction.filter(_.memberId === userIDInt).delete
                   println(someException)
                   InternalServerError
                 }
               }
           }
           
      }.getOrElse {//connection.pleaseconnect
        if (unconnectedResult == None){
          Ok(views.html.index(Messages("connection.pleaseconnect")(Instantiations.getLang(request)))(Instantiations.getLang(request)))
        }
        else{
          unconnectedResult.get(request)
        }
      }
      }.getOrElse {
        if (unconnectedResult == None){
          Ok(views.html.index(Messages("connection.pleaseconnect")(Instantiations.getLang(request)))(Instantiations.getLang(request)))
        }
        else{
          unconnectedResult.get(request)
        }
      }
     }catch {
        case e: NumberFormatException => {
            BadRequest
          }
      }
      )
   }
   

}
