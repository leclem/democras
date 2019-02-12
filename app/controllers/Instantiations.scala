package controllers
import slick.driver.MySQLDriver.simple._
import play.api.i18n._
import play.api.i18n.I18nSupport
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.mvc._
import play.mvc.Http
import play.api.db._

object Instantiations{
  object Tables{
    val db = Database.forDataSource(DB.getDataSource())

    //Tables
    val members = TableQuery[models.MembersTable]
   val mailsUnsubscribed = TableQuery[models.MailsUnsubscribed]
   
    val membersInfo = TableQuery[models.MembersInfoTable]
	val membersProfilePicture = TableQuery[models.MemberProfilePictureTable]
	val membersExternalIds = TableQuery[models.MemberExternalIdsTable]
  val membersLanguage = TableQuery[models.MemberLanguageTable]
  val membersStats = TableQuery[models.MemberStatsTable]
  val membersStatsMonthly = TableQuery[models.MemberStatsMonthlyTable]
  val passwordForget = TableQuery[models.PasswordForgetTable]

    val follow = TableQuery[models.FollowTable]
	val comment = TableQuery[models.CommentsTable]

    val concepts = TableQuery[models.ConceptTable]
    val conceptsOpinions = TableQuery[models.ConceptOpinionsTable]

    val actions = TableQuery[models.ActionTable]
    val actionJoin = TableQuery[models.ActionJoinTable]

    val groups = TableQuery[models.GroupTable]
    val groupMembership = TableQuery[models.GroupMembershipTable]
    val groupApplication = TableQuery[models.GoupApplicationsTable]

    val doingAction = TableQuery[models.DoingActionTable]

    val debate = TableQuery[models.DebateTable]
	val debateYesOrNo = TableQuery[models.OpinionYesOrNoDebateTable]
	val debateOpinion = TableQuery[models.OpinionDebateTable]

	val voteYesOrNo = TableQuery[models.VoteYesOrNoTable]
	val voteOpinion = TableQuery[models.VoteOpinionTable]

  val debateRandomOpinion = TableQuery[models.DebateRandomOpinionTable]

	val petition = TableQuery[models.PetitionTable]
	val petitionJoin = TableQuery[models.PetitionJoinTable]

	val conversation = TableQuery[models.ConversationTable]
	val conversationMember = TableQuery[models.ConversationMembersTable]
	val conversationMessages = TableQuery[models.ConversationMessagesTable]

  val activity = TableQuery[models.ActivityTable]
  val notification = TableQuery[models.NotificationTable]
   
  val numberOfViews =  TableQuery[models.NumberOfViewsTable]
   
  val membersFriendsMails = TableQuery[models.MembersFriendsMailsTable]
  val membersPagesLoaded = TableQuery[models.MembersPagesLoadedTable]
    //val redirectStats = TableQuery[models.RedirectStatsTable]
  }

  val languageFR = play.api.i18n.Messages.Implicits.applicationMessages(Lang("fr"), play.api.Play.current)
  val languageEN = play.api.i18n.Messages.Implicits.applicationMessages(Lang("en"), play.api.Play.current)
  val languageES = play.api.i18n.Messages.Implicits.applicationMessages(Lang("es"), play.api.Play.current)
  //Default language : fr
  /*def getLang(implicit request: Request[AnyContent]):Messages = {
    request.session.get("language").map { language =>
      if (language == "fr")
        languageFR
      languageFR
      }.getOrElse {
      languageFR
    }
  }*/

  def getPreferedLanguage(request: RequestHeader): String = {
    var defaultLanguage = "fr"
    /*
     * en	English
en-us	English (United States)	en-gb	English (United Kingdom)
en-au	English (Australia)	en-ca	English (Canada)
en-nz	English (New Zealand)	en-ie	English (Ireland)
en-za	English (South Africa)	en-jm	English (Jamaica)
en	English (Caribbean)	en-bz	English (Belize)
en-tt
     */
    /*for (acceptedLanguage <- request.acceptLanguages){
      if (acceptedLanguage == Lang("fr") 
          || acceptedLanguage == Lang("fr-fr")
          || acceptedLanguage == Lang("fr-be")
          || acceptedLanguage == Lang("fr-ch")
          || acceptedLanguage == Lang("fr-lu")
          || acceptedLanguage == Lang("fr-mc")
          )
        return "fr"
      else if (acceptedLanguage == Lang("es")
          || acceptedLanguage == Lang("es-es")
          || acceptedLanguage == Lang("es-ar")
          || acceptedLanguage == Lang("es-bo")
          || acceptedLanguage == Lang("es-cl")
          || acceptedLanguage == Lang("es-co")
          || acceptedLanguage == Lang("es-cr")
          || acceptedLanguage == Lang("es-do")
          || acceptedLanguage == Lang("es-ec")
          || acceptedLanguage == Lang("es-es")
          || acceptedLanguage == Lang("es-gt")
          || acceptedLanguage == Lang("es-hn")
          || acceptedLanguage == Lang("es-mx")
          || acceptedLanguage == Lang("es-ni")
          || acceptedLanguage == Lang("es-pa")
          || acceptedLanguage == Lang("es-pe")
          || acceptedLanguage == Lang("es-pr")
          || acceptedLanguage == Lang("es-py")
          || acceptedLanguage == Lang("es-sv")
          || acceptedLanguage == Lang("es-uy")
          || acceptedLanguage == Lang("es-ve"))
        return "es"
      else if ( acceptedLanguage == Lang("en-us")
          || acceptedLanguage == Lang("en")
          || acceptedLanguage == Lang("en-gb")
          || acceptedLanguage == Lang("en-au")
          || acceptedLanguage == Lang("en-ca")
          || acceptedLanguage == Lang("en-nz")
          || acceptedLanguage == Lang("en-ie")
          || acceptedLanguage == Lang("en-za")
          || acceptedLanguage == Lang("en-jm")
          || acceptedLanguage == Lang("en-bz")
          || acceptedLanguage == Lang("en-tt"))
        return "en"
    }*/
    defaultLanguage
  }
  
  def getLangText(implicit request: RequestHeader):String = {
    request.session.get("language").map { language =>
      language
    }.getOrElse {
      getPreferedLanguage(request)
    }
  }

  //Default language : fr
  def getLang(implicit request: RequestHeader):Messages = {
    request.session.get("language").map { language =>
      if (language == "fr")
        languageFR
      else if (language == "es"){
        languageES}
      else if (language == "en")
        languageEN
      else
        languageFR
      }.getOrElse {
        play.api.i18n.Messages.Implicits.applicationMessages(Lang(getPreferedLanguage(request)), play.api.Play.current)
      }
  }

  def getUserMainLang(userID: Long): Messages = {
    Instantiations.Tables.db.withSession { implicit session =>
      val q2 = for {c <- Instantiations.Tables.membersInfo if c.id === userID} yield c.lang
      if (q2.list.head == "en")
        languageFR
      else if (q2.list.head == "es")
        languageES
      else
        languageEN
    }
  }

}
