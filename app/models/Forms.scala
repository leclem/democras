package models
import play.api.data._
import play.api.data.Forms._

object Forms{
    //USER CREDENTIALS
	val connectionForm = Form(
	  mapping(
	    "username" -> text,
	    "password" -> text
	  )(models.formsClasses.userCredentials.apply)(models.formsClasses.userCredentials.unapply)
	)
  //je vais faire plusieurs from pour le login
  //des qu'il est login on enregistre le début (au cas ou sa fenetre ferme ou autre)
  val subscriptionFbForm = Form(
    mapping(
    "firstname" -> text,
    "name" -> text,
    "gender" -> text, //INT : 0 : male 1 : female
    "birthday" -> text, //LONG that represent a timestamp
    "city" -> text,
    "idfb" -> text,//INT that represent a timestamp
    "address" -> text,
    "location" -> text,
    "lang" -> text, //FR or EN for the moment
    "mail" -> text,
    
    "username" -> text,
    "password" -> text,
    "link" -> text,
    "presentation" -> text,
    
    "pictureu" -> text,
    "picturex" -> text,//INT
    "picturey" -> text,//INT
    "picturem" -> text,
    "picturemx" -> text,//INT
    "picturemy" -> text,//INT
    "picturemm" -> text
    )(models.formsClasses.usersubscriptionFb.apply)(models.formsClasses.usersubscriptionFb.unapply)
  )
  
  val userSubcriptionLoginForm = Form(
	  mapping(
	    "username" -> text,
	    "password" -> text,
	    "mail" -> text,
	    "lang" -> text
	  )(models.formsClasses.userSubcriptionLogin.apply)(models.formsClasses.userSubcriptionLogin.unapply)
	)
  
	val subscriptionForm = Form(
	  mapping(
	    "firstname" -> text,
	    "name" -> text,
      "gender" -> text,
      "birthday" -> text,
      "city" -> text,
      "address" -> text,
      "link" -> text,
      "presentation" -> text,
      "location" -> text/*,
      "lang" -> seq(text)*/
	  )(models.formsClasses.userSubcription.apply)(models.formsClasses.userSubcription.unapply)
	)
	
	val userLanguageForm = Form(
	  mapping(
	    "language" -> text
	  )(models.formsClasses.userLanguage.apply)(models.formsClasses.userLanguage.unapply)
	)
	
	val userPasswordForgetForm = Form(
	  mapping(
	    "username" -> text
	  )(models.formsClasses.userPasswordForget.apply)(models.formsClasses.userPasswordForget.unapply)
	)
  
	val userPasswordResetForm = Form(
	  mapping(
	    "hash" -> text,
	    "newPassword" -> text
	  )(models.formsClasses.userPasswordReset.apply)(models.formsClasses.userPasswordReset.unapply)
	)
	
	//CONCEPTS
	val conceptCreationForm = Form(
	  mapping(
	    "name" -> text,
	    "definition" -> text,
	    "language" -> text
	  )(models.formsClasses.createConcept.apply)(models.formsClasses.createConcept.unapply)
	)

	//GROUPS
	val groupCreationForm = Form(
	  mapping(
	    "name" -> text,
	    "definition" -> text,
	    "privacy" -> text
	  )(models.formsClasses.createGroup.apply)(models.formsClasses.createGroup.unapply)
	)

	//ACTIONS
	val actionCreationForm = Form(
	  mapping(
	    "idGroup" -> text, //-1 if a member created it
	    "name" -> text,
	    "definition" -> text,
	    "namelocation" -> text,
	    "addresslocation" -> text,
	    "positionX" -> text,
	    "positionY" -> text,
	    "timeBegin" -> text,
	    "timeEnd" -> text,
	    "language" -> text
	  )(models.formsClasses.createAction.apply)(models.formsClasses.createAction.unapply)
	)

	//MEMBER INFO
	val memberInfoForm = Form(
	  mapping(
	    "firstname" -> text,
	    "name" -> text,
	    "mail" -> text,
	    "ville" -> text,
	    "presentation" -> text
	  )(models.formsClasses.userInfo.apply)(models.formsClasses.userInfo.unapply)
	)

	//DEBATE
	val debateCreationForm = Form(
	  mapping(
	    "question" -> text,
	    "debateType" -> text,
	    "reservedToGroup" -> text,
	    "language" -> text
	  )(models.formsClasses.createDebate.apply)(models.formsClasses.createDebate.unapply)
	)

	//OPINION
	val debateOpinionForm = Form(
	  mapping(
	    "comment" -> text
	  )(models.formsClasses.opinionDebate.apply)(models.formsClasses.opinionDebate.unapply)
	)

	//YES OR NO OPINON
	val debateYesOrNoForm = Form(
	  mapping(
	    "comment" -> text,
	    "opinion" -> text
	  )(models.formsClasses.yesOrNoDebate.apply)(models.formsClasses.yesOrNoDebate.unapply)
	)

	//PETITION
	val petitionCreationForm = Form(
	  mapping(
	    "idGroup" -> text, //-1 if a member created it
	    "name" -> text,
	    "definition" -> text,
	    "timeEnd" -> text,
	    "language" -> text
	  )(models.formsClasses.createPetition.apply)(models.formsClasses.createPetition.unapply)
	)

	//CONVERSATION
	val conversationCreationForm = Form(
	  mapping(
	    "receivers" -> seq(text),
	    "message" -> text
	  )(models.formsClasses.createConversation.apply)(models.formsClasses.createConversation.unapply)
	)

	val conversationSendingForm = Form(
	  mapping(
	    "id" -> text,
	    "message" -> text
	  )(models.formsClasses.sendConversation.apply)(models.formsClasses.sendConversation.unapply)
	)
    
	//COMMENTS
	val commentSendingForm = Form(
	  mapping(
	    "objectType" -> text,
	    "objectID" -> text,
	    "message" -> text
	  )(models.formsClasses.createComment.apply)(models.formsClasses.createComment.unapply)
	)
	
	val commentModificationForm = Form(
	  mapping(
	    "id" -> text,
	    "message" -> text
	  )(models.formsClasses.commentModification.apply)(models.formsClasses.commentModification.unapply)
	)
	
	//MEMBERSMAILS
	val membersMailsForm = Form(
	  mapping(
	    "mails" -> seq(text)
	  )(models.formsClasses.membersMails.apply)(models.formsClasses.membersMails.unapply)
	)
}
