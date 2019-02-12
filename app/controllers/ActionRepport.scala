package controllers
import play.api._
import play.api.mvc._
import play.api.mvc.Result
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.i18n._
import play.api.i18n.I18nSupport
import play.api.i18n.Messages.Implicits._
import slick.driver.MySQLDriver.simple._
import slick.jdbc.{GetResult, StaticQuery => Q}
import slick.jdbc.JdbcBackend.Database
import Q.interpolation


case class activityHeaders(verb: String, modificationTimestamp: Long, lang: String)

object ActionRepport extends Controller{

  /*
   *  {
    "published": "2011-02-10T15:04:55Z",
    "actor": {
      "url": "http://example.org/martin",
      "objectType" : "person",
      "id": "tag:example.org,2011:martin",
      "image": {
        "url": "http://example.org/martin/image",
        "width": 250,
        "height": 250
      },
      "displayName": "Martin Smith"
    },
    "verb": "post",
    "object" : {
      "url": "http://example.org/blog/2011/02/entry",
      "id": "tag:example.org,2011:abc123/xyz"
    },
    "target" : {
      "url": "http://example.org/blog/",
      "objectType": "blog",
      "id": "tag:example.org,2011:abc123",m
      "displayName": "Martin's Blog"
    }
  }
   */
  /*
   * {
"headers" : {
updatedTimestamp: LONG
verb: STRING
}
"actor": OBJ
"object": OBJ
target:OBJ
}
   */
  implicit val activityHeadersWrites: Writes[controllers.activityHeaders] = {
    (
      (JsPath \ "verb").write[String] and
      (JsPath \ "modificationTimestamp").write[Long] and
      (JsPath \ "lang").write[String]
    )(unlift(controllers.activityHeaders.unapply))
  }
  implicit val userWrites = User.userWrites
  implicit val groupWrites = Groups.groupWrites
  implicit val conceptWrites = Concepts.conceptWrites
  implicit val debateWrites = Debates.debateWrites
  implicit val debateYesOrNotWrites = Debates.debateYesOrNotWrites
  implicit val debateOpinionWrites = Debates.debateOpinionWrites
  implicit val actionWrites = Actions.actionWrites
  implicit val petitionWrites = Petitions.petitionWrites
  implicit val commentWrites = Comments.commentWrites
  /*
   * Converts an activity table entry from database in a JSON readable activity.
   * The format is the following :
   * {
   *   headers{
   *     verb: ""
   *     modificationTimestamp: MODIFICATIONTIMESTAMP
   *   }
   *   actor: WHODIDTHIS
   *   object: WITHWHATDIDITDOTHIS
   *   target: ONWHATDIDHEDOTHIS
   * }
   * You can find the exact definitions commented in Tables.ActivityTable
   */
  /*
  def convertIntoJSValue(x: models.Types.activityTableEntry) = Connection.withConnection ({
    username => userID => implicit request => implicit lang =>
      Ok(convertIntoJSValueSub(x, userID.toLong))
  }, Option((request: Request[AnyContent]) => {
      Ok(convertIntoJSValueSub(x))
  }))
*/
  def convertIntoJSValue(x: models.Types.activityTableEntry, currentUserID: Long = -1, notificationMode: Boolean = false): JsValue = {

    val userActor = User.getMemberInfoFromID(x._2)

    val activityObject = x._4
    val activityTarget  = x._5
    val activityType = x._6
    val modificationTimestamp = x._7.getTime()
    //println("Begin convertIntoJSValue " + System.currentTimeMillis() + " : useractor" + userActor + " activityobject " + activityObject + " activitytarget : " + activityTarget + " activityType "  + activityType)
        
    try{
   // println("Exception : useractor" + userActor + " activityobject " + activityObject + " activitytarget : " + activityTarget + " activityType "  + activityType)

    case class UnauthorizedActivity(headers : controllers.activityHeaders, actor: String, objectt: String, target: String)
        implicit val UnauthorizedActivityWrites: Writes[UnauthorizedActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[String] and
	        (JsPath \ "object").write[String] and
	        (JsPath \ "target").write[String]
	      )(unlift(UnauthorizedActivity.unapply))
    }
    val jsonToSend = activityType match {
      case 0 => {// ProfileCreation
        case class ProfileCreationActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.MemberInfo, target: models.MemberInfo)

        implicit val ProfileCreationActivityWrites: Writes[ProfileCreationActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.MemberInfo] and
	        (JsPath \ "target").write[models.MemberInfo]
	      )(unlift(ProfileCreationActivity.unapply))
	    }
        Json.toJson(ProfileCreationActivity(activityHeaders("ProfileCreation", modificationTimestamp, "all"), userActor, userActor, userActor))
      }
      case 1 | 2 | 3 | 4 | 5 | 6  => {//ProfileModification(1) or ProfileFirstNameModification  (2) or ProfileNameModification (3) or ProfileVilleModification (4) or ProfileMailModification(5)
        case class ProfileModificationActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.MemberInfo, target: models.MemberInfo)
        implicit val ProfileModificationActivityWrites: Writes[ProfileModificationActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.MemberInfo] and
	        (JsPath \ "target").write[models.MemberInfo]
	      )(unlift(ProfileModificationActivity.unapply))
	    }
        var verb: String = ""
        activityType match {
          case 1 => verb = "ProfileModification"
          case 2 => verb = "ProfileFirstNameModification"
          case 3 => verb = "ProfileNameModification"
          case 4 => verb = "ProfileVilleModification"
          case 5 => verb = "ProfileMailModification"
          case 6 => verb = "ProfilePresentationModification"
          case _ =>
        }
        Json.toJson(ProfileModificationActivity(activityHeaders(verb, modificationTimestamp, "all"), userActor, User.getMemberInfoFromID(activityObject), User.getMemberInfoFromID(activityObject)))

      }
      /*case 6 => {//Follow
        case class FollowActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.MemberInfo, target: models.MemberInfo)
        implicit val FollowActivityWrites: Writes[FollowActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.MemberInfo] and
	        (JsPath \ "target").write[models.MemberInfo]
	      )(unlift(FollowUserActivity.unapply))
	    }
        val verb="Follow"
        Json.toJson(FollowUserActivity(activityHeaders(verb, modificationTimestamp), userActor, userActor, userActor))

      }*/
      case 100 => {//FollowMember
        case class FollowMemberActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.MemberInfo, target: models.MemberInfo)
        implicit val FollowMemberActivityWrites: Writes[FollowMemberActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.MemberInfo] and
	        (JsPath \ "target").write[models.MemberInfo]
	      )(unlift(FollowMemberActivity.unapply))
	    }
        val memberFollowedObject = User.getMemberInfoFromID(activityObject)
        Json.toJson(FollowMemberActivity(activityHeaders("FollowMember", modificationTimestamp, "all"), userActor, memberFollowedObject, memberFollowedObject))

      }
      case 10000 => {//GroupCreation
        case class GroupCreationActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Group, target: models.Group)
        implicit val GroupCreationActivityWrites: Writes[GroupCreationActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Group] and
	        (JsPath \ "target").write[models.Group]
	      )(unlift(GroupCreationActivity.unapply))
	    }
        val groupObject = Groups.getGroupModel(activityObject)
        Json.toJson(GroupCreationActivity(activityHeaders("GroupCreation", modificationTimestamp, groupObject.language), userActor, groupObject, groupObject))

      }
      case 10001 | 10010 => {//GroupJoin (10001),  GroupLeave (10010)
        case class GroupJoinLeaveActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Group, target: models.MemberInfo)
        implicit val GroupJoinLeaveActivityWrites: Writes[GroupJoinLeaveActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Group] and
	        (JsPath \ "target").write[models.MemberInfo]
	      )(unlift(GroupJoinLeaveActivity.unapply))
	    }
        val groupObject = Groups.getGroupModel(activityObject)

        var verb = ""
        activityType match {
          case 10001 => verb = "GroupJoin"
          case 10010 => verb = "GroupLeave"
        }

        Json.toJson(GroupJoinLeaveActivity(activityHeaders(verb, modificationTimestamp, groupObject.language), userActor, groupObject, userActor))
      }
      case 10005 => { //GroupAccept (10005)
        case class GroupAcceptActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Group, target: models.MemberInfo)
        
        implicit val GroupAcceptActivityWrites: Writes[GroupAcceptActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Group] and
	        (JsPath \ "target").write[models.MemberInfo]
	      )(unlift(GroupAcceptActivity.unapply))
	      }
        val groupObject = Groups.getGroupModel(activityObject)
        val userAccepted = User.getMemberInfoFromID(activityTarget)
        Json.toJson(GroupAcceptActivity(activityHeaders("GroupAccept", modificationTimestamp, groupObject.language), userActor, groupObject, userAccepted))
      }
      case 10002 | 10003 | 10004 => {//Group change rank : Member (10002) / Leader (10003) / Admin (10004)
        case class GroupChangeRankActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Group, target: models.MemberInfo)
        implicit val GroupChangeRankActivityWrites: Writes[GroupChangeRankActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Group] and
	        (JsPath \ "target").write[models.MemberInfo]
	      )(unlift(GroupChangeRankActivity.unapply))
	    }
        val groupObject = Groups.getGroupModel(activityObject)
        val memberWhoseRankChanged = User.getMemberInfoFromID(activityTarget)
        var verb = ""
        activityType match {
          case 10002 => verb = "ChangeRankMember"
          case 10003 => verb = "ChangeRankLeader"
          case 10004 => verb = "ChangeRankAdmin"
          case _ =>
        }
        Json.toJson(GroupChangeRankActivity(activityHeaders(verb, modificationTimestamp, groupObject.language), userActor, groupObject, memberWhoseRankChanged))
      }

      case 10100 | 10101 => { //GroupDescriptionModification
        case class GroupChangeDescriptionActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Group, target: models.Group)
        implicit val GroupChangeRankDescriptionWrites: Writes[GroupChangeDescriptionActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Group] and
	        (JsPath \ "target").write[models.Group]
	      )(unlift(GroupChangeDescriptionActivity.unapply))
	    }
        var verb = ""
        activityType match {
          case 10100 => var verb = "GroupDescriptionModification"
          case 10101 => var verb = "GroupNameModification"
        }
        val groupObject = Groups.getGroupModel(activityObject)
        Json.toJson(GroupChangeDescriptionActivity(activityHeaders(verb, modificationTimestamp, groupObject.language), userActor, groupObject, groupObject))
      }
      case 20000 | 20001 => {//ConceptCreation (20000) or ConceptDescriptionModification(20001)
        case class ConceptCreationModificationActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Concept, target: models.Concept)
        implicit val ConceptCreationModificationActivityWrites: Writes[ConceptCreationModificationActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Concept] and
	        (JsPath \ "target").write[models.Concept]
	      )(unlift(ConceptCreationModificationActivity.unapply))
	    }
        var verb = ""
        activityType match {
          case 20000 => verb = "ConceptCreation"
          case 20001 => verb = "ConceptDescriptionModification"
        }
        val conceptObject = Concepts.getConceptModel(activityObject)
        Json.toJson(ConceptCreationModificationActivity(activityHeaders(verb, modificationTimestamp, conceptObject.language), userActor, conceptObject, conceptObject))

      }
      case 20002 | 20003 | 20004 | 20005 | 20006 => {//VoteFor Concept (20002) or VoteAgainst Concept (20003) or CancelVote (20004) or Modif vote Votefor -> VoteAgainst (20005) or Modif vote VoteAgainst -> Votefor (20006)
        case class VoteConceptActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Concept, target: models.Concept)
        implicit val VoteConceptActivityWrites: Writes[VoteConceptActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Concept] and
	        (JsPath \ "target").write[models.Concept]
	      )(unlift(VoteConceptActivity.unapply))
	    }
        var verb = ""
        activityType match {
          case 20002 => verb = "VoteForConcept"
          case 20003 => verb = "VoteAgainstConcept"
          case 20004 => verb = "CancelVoteConcept"
          case 20005 => verb = "ModifVoteForToAgainstConcept"
          case 20006 => verb = "ModifVoteAgainstToForConcept"
        }
        val conceptObject = Concepts.getConceptModel(activityObject)
        Json.toJson(VoteConceptActivity(activityHeaders(verb, modificationTimestamp, conceptObject.language), userActor, conceptObject, conceptObject))
      }
      case 30000 => {//DebateCreation
        case class DebateCreationActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Debate, target: models.Debate)
        implicit val DebateCreationActivityWrites: Writes[DebateCreationActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Debate] and
	        (JsPath \ "target").write[models.Debate]
	      )(unlift(DebateCreationActivity.unapply))
	    }

        val debateObject = Debates.getDebateObject(activityObject,currentUserID)
        Json.toJson(DebateCreationActivity(activityHeaders("DebateCreation", modificationTimestamp, debateObject.language), userActor, debateObject, debateObject))

      }
      case 30001 => {//DebateYesOrNoParticipation
        case class DebateYesOrNotOpinionActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Debate, target: models.DebateYesOrNot)
        implicit val DebateYesOrNotOpinionActivityWrites: Writes[DebateYesOrNotOpinionActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Debate] and
	        (JsPath \ "target").write[models.DebateYesOrNot]
	      )(unlift(DebateYesOrNotOpinionActivity.unapply))
	    }
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
        ActionRepport.addView(6, activityTarget)
        val debateObject = Debates.getDebateObject(activityObject, currentUserID)
        val yesOrNoOpinionObject = Debates.getYesOrNoOpinionObject(activityTarget)
        Json.toJson(DebateYesOrNotOpinionActivity(activityHeaders("DebateYesOrNoParticipation", modificationTimestamp, debateObject.language), userActor, debateObject, yesOrNoOpinionObject))

      }
      case 30002 | 30003 | 30004 | 30005 => {//Vote for an YesOrNoOpinion
        //VoteFor YesOrNoOpinion (30002) / VoteAgainst YesOrNoOpinion (30003) / Modif VoteFor -> VoteAgainst YesOrNoOpinion (30004) / Modif VoteAgainst -> Votefor YesOrNoOpinion (30005)
        case class DebateYesOrNotOpinionVoteActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Debate, target: models.DebateYesOrNot)
        implicit val DebateYesOrNotOpinionActivityWrites: Writes[DebateYesOrNotOpinionVoteActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Debate] and
	        (JsPath \ "target").write[models.DebateYesOrNot]
	      )(unlift(DebateYesOrNotOpinionVoteActivity.unapply))
	    }

        val debateObject = Debates.getDebateObject(activityObject, currentUserID)
        val yesOrNoOpinionObject = Debates.getYesOrNoOpinionObject(activityTarget)
        var verb = ""
        activityType match {
          case 30002 => verb = "VoteForYesOrNoOpinion"
          case 30003 => verb = "VoteAgainstYesOrNoOpinion"
          case 30004 => verb = "ModifVoteForToAgainstYesOrNoOpinion"
          case 30005 => verb = "ModifVoteAgainstToForYesOrNoOpinion"
        }

        Json.toJson(DebateYesOrNotOpinionVoteActivity(activityHeaders(verb, modificationTimestamp, debateObject.language), userActor, debateObject, yesOrNoOpinionObject))
      }
      case 30010 => {//Debate creation for a group
        case class DebateCreationForGroupActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Group, target: models.Debate)
        implicit val DebateCreationForGroupActivityWrites: Writes[DebateCreationForGroupActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Group] and
	        (JsPath \ "target").write[models.Debate]
	      )(unlift(DebateCreationForGroupActivity.unapply))
	    }
          val groupObject = Groups.getGroupModel(activityObject)
          val debateObject = Debates.getDebateObject(activityTarget, currentUserID)
        //userActor.id == x._2 
        //This activity is special : not everybody can see it, only allowed members (that are members of the group)
        Json.toJson(DebateCreationForGroupActivity(activityHeaders("DebateCreationForGroup", modificationTimestamp, debateObject.language), userActor, groupObject, debateObject))
        
      }
      case 30015 => {
        case class DebateOpinionParticipationActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Debate, target: models.DebateOpinion)
        implicit val DebateOpinionParticipationActivityWrites: Writes[DebateOpinionParticipationActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Debate] and
	        (JsPath \ "target").write[models.DebateOpinion]
	      )(unlift(DebateOpinionParticipationActivity.unapply))
	    }
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
        ActionRepport.addView(7, activityTarget)
        val debateObject = Debates.getDebateObject(activityObject, currentUserID)
        val debateOpinionObject = Debates.getDebateOpinionObject(activityTarget)
        Json.toJson(DebateOpinionParticipationActivity(activityHeaders("DebateOpinionParticipation", modificationTimestamp, debateObject.language), userActor, debateObject, debateOpinionObject))

      }
      case 30016 | 30017 | 30018 | 30019 => {
        case class DebateOpinionParticipationActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Debate, target: models.DebateOpinion)
        implicit val DebateOpinionParticipationActivityWrites: Writes[DebateOpinionParticipationActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Debate] and
	        (JsPath \ "target").write[models.DebateOpinion]
	      )(unlift(DebateOpinionParticipationActivity.unapply))
	    }
        var verb = ""
        activityType match {
          case 30016 => verb = "VoteForOpinion"
          case 30017 => verb = "VoteAgainstOpinion"
          case 30018 => verb = "ModifVoteForToAgainstOpinion"
          case 30019 => verb = "ModifVoteAgainstToForOpinion"
        }

        val debateObject = Debates.getDebateObject(activityObject, currentUserID)
        val debateOpinionObject = Debates.getDebateOpinionObject(activityTarget)
        Json.toJson(DebateOpinionParticipationActivity(activityHeaders(verb, modificationTimestamp, debateObject.language), userActor, debateObject, debateOpinionObject))

      }
      case 30020 => { //Post yes or no opinion on opinion
        case class DebateYesOrNoOpinionParticipationOnOpinionActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.DebateOpinion, target: models.DebateYesOrNot)
        implicit val DebateYesOrNoOpinionParticipationOnOpinionWrites: Writes[DebateYesOrNoOpinionParticipationOnOpinionActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.DebateOpinion] and
	        (JsPath \ "target").write[models.DebateYesOrNot]
	      )(unlift(DebateYesOrNoOpinionParticipationOnOpinionActivity.unapply))
        }
        
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
        ActionRepport.addView(7, activityObject)
        ActionRepport.addView(6, activityTarget)
        
	      val debateOpinionObject = Debates.getDebateOpinionObject(activityObject)
	      val debateYesAndNoOpinionObject = Debates.getYesOrNoOpinionObject(activityTarget)
	      Json.toJson(DebateYesOrNoOpinionParticipationOnOpinionActivity(activityHeaders("PostYesOrNoOpinionOnOpinion", modificationTimestamp, Debates.getDebateObject(debateOpinionObject.idDebate, currentUserID).language), userActor, debateOpinionObject, debateYesAndNoOpinionObject))

      }
      case 40000 => { //Action Creation by user
        case class ActionCreationActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Action, target: models.Action)
        implicit val ActionCreationActivityWrites: Writes[ActionCreationActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Action] and
	        (JsPath \ "target").write[models.Action]
	      )(unlift(ActionCreationActivity.unapply))
	    }
        val actionObject = Actions.getActionModel(activityObject)
        Json.toJson(ActionCreationActivity(activityHeaders("ActionCreation", modificationTimestamp, actionObject.language), userActor, actionObject, actionObject))

      }
      case 40001 => { //Action Creation by group
        case class ActionCreationForGroupActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Group, target: models.Action)
        implicit val ActionCreationForGroupActivityWrites: Writes[ActionCreationForGroupActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Group] and
	        (JsPath \ "target").write[models.Action]
	      )(unlift(ActionCreationForGroupActivity.unapply))
	    }
        val groupObject = Groups.getGroupModel(activityObject)
        val actionObject = Actions.getActionModel(activityTarget)
        Json.toJson(ActionCreationForGroupActivity(activityHeaders("ActionCreationForGroup", modificationTimestamp, actionObject.language), userActor, groupObject, actionObject))

      }
      case 40002 | 40003 => { //Action Join - Action leave
        case class ActionJoinActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Action, target: models.Action)
        implicit val ActionJoinWrites: Writes[ActionJoinActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Action] and
	        (JsPath \ "target").write[models.Action]
	      )(unlift(ActionJoinActivity.unapply))
	    }
        var verb = ""
        
        activityType match {
          case 40002 => verb = "JoinAction"
          case 40003 => verb = "LeaveAction"
        }
        
        val actionObject = Actions.getActionModel(activityObject)
        Json.toJson(ActionJoinActivity(activityHeaders(verb, modificationTimestamp, actionObject.language), userActor, actionObject, actionObject))
      }
      case 50000 => { //Petition Creation by user
        case class PetitionCreationActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Petition, target: models.Petition)
        implicit val PetitionCreationActivityWrites: Writes[PetitionCreationActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Petition] and
	        (JsPath \ "target").write[models.Petition]
	      )(unlift(PetitionCreationActivity.unapply))
	    }
        val petitionObject = Petitions.getPetitionModel(activityObject)
        Json.toJson(PetitionCreationActivity(activityHeaders("PetitionCreation", modificationTimestamp, petitionObject.language), userActor, petitionObject, petitionObject))

      }
      case 50001 => { //Petition Creation by group
        case class PetitionCreationForGroupActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Group, target: models.Petition)
        implicit val PetitionCreationForGroupActivityWrites: Writes[PetitionCreationForGroupActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Group] and
	        (JsPath \ "target").write[models.Petition]
	      )(unlift(PetitionCreationForGroupActivity.unapply))
	    }
        val groupObject = Groups.getGroupModel(activityObject)
        val petitionObject = Petitions.getPetitionModel(activityTarget)
        Json.toJson(PetitionCreationForGroupActivity(activityHeaders("PetitionCreationForGroup", modificationTimestamp, petitionObject.language), userActor, groupObject, petitionObject))

      }
      case 50002 | 50003=> { //Petition Join
        case class PetitionJoinActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Petition, target: models.Petition)
        implicit val PetitionJoinWrites: Writes[PetitionJoinActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Petition] and
	        (JsPath \ "target").write[models.Petition]
	      )(unlift(PetitionJoinActivity.unapply))
	    }
        var verb = ""
        activityType match {
          case 50002 => verb = "PetitionJoin"
          case 50003 => verb = "PetitionLeave"
        }
        val petitionObject = Petitions.getPetitionModel(activityObject)
        Json.toJson(PetitionJoinActivity(activityHeaders(verb, modificationTimestamp, petitionObject.language), userActor, petitionObject, petitionObject))
      }
      case 60001 => { //CommentGroup
        case class CommentGroupActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Group, target: models.Comment)
        implicit val CommentGroupWrites: Writes[CommentGroupActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Group] and
	        (JsPath \ "target").write[models.Comment]
	      )(unlift(CommentGroupActivity.unapply))
	      }
        
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
        ActionRepport.addView(8, activityTarget)
        
        val groupObject = Groups.getGroupModel(activityObject)
        val commentObject = Comments.getCommentObject(activityTarget)
        //val conceptOwner = User.getMemberInfoFromID(activityTarget)
        Json.toJson(CommentGroupActivity(activityHeaders("CommentGroup", modificationTimestamp, groupObject.language), userActor, groupObject, commentObject))
      }
      case 60002 => { //CommentConcept
        case class CommentConceptActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Concept, target: models.Comment)
        implicit val CommentConceptWrites: Writes[CommentConceptActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Concept] and
	        (JsPath \ "target").write[models.Comment]
	      )(unlift(CommentConceptActivity.unapply))
	      }
        
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
        ActionRepport.addView(2, activityObject)
        ActionRepport.addView(8, activityTarget)
        
        val conceptObject = Concepts.getConceptModel(activityObject)
        val commentObject = Comments.getCommentObject(activityTarget)
        //val conceptOwner = User.getMemberInfoFromID(activityTarget)
        Json.toJson(CommentConceptActivity(activityHeaders("CommentConcept", modificationTimestamp, conceptObject.language), userActor, conceptObject, commentObject))
      }
      case 60004 => { //CommentAction
        case class CommentActionActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Action, target: models.Comment)
        implicit val CommentActionWrites: Writes[CommentActionActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Action] and
	        (JsPath \ "target").write[models.Comment]
	      )(unlift(CommentActionActivity.unapply))
	      }
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
        ActionRepport.addView(4, activityObject)
        ActionRepport.addView(8, activityTarget)
        
        val actionObject = Actions.getActionModel(activityObject)
        val commentObject = Comments.getCommentObject(activityTarget)
        //val actionOwner = User.getMemberInfoFromID(activityTarget)
        Json.toJson(CommentActionActivity(activityHeaders("CommentAction", modificationTimestamp, actionObject.language), userActor, actionObject, commentObject))
      }
      case 60005 => { //CommentPetition
        case class CommentPetitionActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.Petition, target: models.Comment)
        implicit val CommentPetitionWrites: Writes[CommentPetitionActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.Petition] and
	        (JsPath \ "target").write[models.Comment]
	      )(unlift(CommentPetitionActivity.unapply))
	      }
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
        ActionRepport.addView(5, activityObject)
        ActionRepport.addView(8, activityTarget)
        
        val petitionObject = Petitions.getPetitionModel(activityObject)
        val commentObject = Comments.getCommentObject(activityTarget)
        //val petitionOwner = User.getMemberInfoFromID(activityTarget)
        Json.toJson(CommentPetitionActivity(activityHeaders("CommentPetition", modificationTimestamp, petitionObject.language), userActor, petitionObject, commentObject))
      }
      case 60006 => { //CommentYesorno
        case class CommentYesOrNoActivity(headers : controllers.activityHeaders, actor: models.MemberInfo, objectt: models.DebateYesOrNot, target: models.Comment)
        implicit val CommentYesOrNoWrites: Writes[CommentYesOrNoActivity] = {
	      (
	        (JsPath \ "headers").write[controllers.activityHeaders] and
	        (JsPath \ "actor").write[models.MemberInfo] and
	        (JsPath \ "object").write[models.DebateYesOrNot] and
	        (JsPath \ "target").write[models.Comment]
	      )(unlift(CommentYesOrNoActivity.unapply))
	      }
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
        ActionRepport.addView(6, activityObject)
        ActionRepport.addView(8, activityTarget)
        
        val yesornoObject = Debates.getYesOrNoOpinionObject(activityObject)
        val commentObject = Comments.getCommentObject(activityTarget)
        
        Json.toJson(CommentYesOrNoActivity(activityHeaders("CommentYesOrNo", modificationTimestamp, Debates.getDebateObject(yesornoObject.idDebate, currentUserID).language), userActor, yesornoObject, commentObject))
      
      }
      case _ => {
        println("Error : actuality doesnt exists : " + activityType)
        null
      }
    }
    Instantiations.Tables.db.withSession { implicit session =>
     // println((jsonToSend \ "headers" \ "lang").get.toString)
      //println(User.requestForUserLanguages(currentUserID).list.map(x => "\"" + x._2.toString + "\""))
      val language = (jsonToSend \ "headers" \ "lang").get.toString

      //println(User.requestForUserLanguages(currentUserID).list.map(x => "\"" + x._2.toString + "\"").contains(language))
      if (notificationMode || language == "all" || User.requestForUserLanguages(currentUserID).list.map(x => "\"" + x._2.toString + "\"").contains(language))
        jsonToSend
      else
        Json.toJson(1) //The activity is in a language the user don't understand
    }
    }
    catch {
      case e: Exception => {
        println("Exception " + e + " : useractor" + userActor + " activityobject " + activityObject + " activitytarget : " + activityTarget + " activityType "  + activityType)
        Json.toJson(1)
                //We do nothing, because the user has already received the notification
       }
    }
  }

  def getActionsDoneOfMemberOrderedByTimestampJSON(memberID: Long, maxTimestamp: Long, number: Int) = controllers.Connection.withConnection  {
    username => userID => implicit request => lang =>
    if (number <= controllers.Application.maxResults && number >= 0){
      Instantiations.Tables.db.withSession { implicit session =>
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        val q2 = for {c <- Instantiations.Tables.activity.filter(x => (x.memberId === memberID && x.logicalDelete === 0 && x.groupPrivacy === -1l && x.modificationTimestamp < maxTimestampParam))} yield c
        
        //We also want to see the private activities that the user can see
        val allActivities = (q2 ++ privateAndPublicGroupActivitiesOfMember(userID.toLong, memberID, maxTimestamp, false)).sortBy(_.modificationTimestamp.desc)
        val listJsValues = allActivities.groupBy(x=>x).map(_._1).sortBy(_.modificationTimestamp.desc).take(number).list.map(x => convertIntoJSValue(x, userID.toLong))

        Ok(Json.toJson(listJsValues))
      }
    }
    else{
      BadRequest
    }
  }

  /*
   * The user did this action.
   */
  /*def actionDone(userIDInt: Int, activityObject: Int, activityTarget: Int, activityType: Int, additionalInfo: String){
    ///TODO : notifications
    addOnActivityTable(userIDInt, activityObject, activityTarget,  activityType)
  }*/
  /*
   * The user did this action.
   */
  /* objecttypes
   * 0 membre
1 groupe
2 concept
3 debate
4 action
5 petitions
6 opinion on debates
   */
  def getUserNewsFeedOrderedByTimestampJSON(maxTimestamp: Long, number: Int) = Connection.withConnection{
    username => userID => implicit request => lang =>
      if (number <= controllers.Application.maxResults && number >= 0){
        val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
        Instantiations.Tables.db.withSession { implicit session =>
          val userIDLong = userID.toLong
          //We have to take what the user follows, and print all the activities related to it
          //If the user follows a member, it also has to print activities with the memberid corresponding to the member followed, so the join has two conditions
        /*  TODO : reset this when we have more members, it filters newsfeed following followsval explicitInnerJoin = for {
		    (activity, follow) <- Instantiations.Tables.activity.filter(_.modificationTimestamp < maxTimestampParam) innerJoin Instantiations.Tables.follow.filter(x => x.memberId === userIDLong) on
		    ((activity, follow) =>
		      (follow.objectFollowedType === 0 && activity.memberId === follow.objectFollowedID
		      ||
		      activity.activityObjectType === follow.objectFollowedType && activity.activityObject === follow.objectFollowedID)
		      &&
		      activity.memberId =!= userIDLong
		      &&
		      activity.logicalDelete === 0 
		      &&
		      activity.groupPrivacy === -1l //We take only the public activities
		      &&
		      (//We only display some kinds of activities, because not all are relevant for a news feed
		        activity.activityType === 1
		        ||
		        activity.activityType === 2
		        ||
		        activity.activityType === 3
		        ||
		        activity.activityType === 4
		        ||
		        activity.activityType === 5
		        ||
		        activity.activityType === 6
		        ||
		        activity.activityType === 10000
		        ||
		        activity.activityType === 10001
		        ||
		        activity.activityType === 10005
		        ||
		        activity.activityType === 10010
		        ||
		        activity.activityType === 10100
		        ||
		        activity.activityType === 10101
		        ||
		        activity.activityType === 20000
		        ||
		        activity.activityType === 20001
		        ||
		        activity.activityType === 30000
		        ||
		        activity.activityType === 30001
		        ||
		        activity.activityType === 30010
		        ||
		        activity.activityType === 30015
		        ||
		        activity.activityType === 40000
		        ||
		        activity.activityType === 40001
		        ||
		        activity.activityType === 40002
		        ||
		        activity.activityType === 40003
		        ||
		        activity.activityType === 50000
		        ||
		        activity.activityType === 50001
		        ||
		        activity.activityType === 50002
		        ||
		        activity.activityType === 50003
		        ||
		        activity.activityType === 60001
		        ||
		        activity.activityType === 60002
		        ||
		        activity.activityType === 60004
		        ||
		        activity.activityType === 60005
		        ||
		        activity.activityType === 60006
		      )
		    )
		    } yield activity */
        val explicitInnerJoin = Instantiations.Tables.activity.filter(activity => 
          activity.modificationTimestamp < maxTimestampParam 
          &&
		      activity.memberId =!= userIDLong
		      &&
		      activity.logicalDelete === 0 
          && 
          activity.groupPrivacy === -1l //We take only the public activities
		      &&
		      (//We only display some kinds of activities, because not all are relevant for a news feed
		        /*activity.activityType === 1
		        ||
		        activity.activityType === 2
		        ||
		        activity.activityType === 3
		        ||
		        activity.activityType === 5
		        ||*/
		        activity.activityType === 6
		        ||
		        activity.activityType === 10000
		        ||
		        activity.activityType === 10001
		        ||
		        activity.activityType === 10005
		        ||
		        activity.activityType === 10010
		        ||
		        activity.activityType === 10100
		        ||
		        activity.activityType === 10101
		        ||
		        activity.activityType === 20000
		        ||
		        activity.activityType === 20001
		        ||
		        activity.activityType === 20002
		        ||
		        activity.activityType === 20003
		        ||
		        activity.activityType === 30000
		        ||
		        activity.activityType === 30001
		        ||
		        activity.activityType === 30002
		        ||
		        activity.activityType === 30003
		        ||
		        activity.activityType === 30004
		        ||
		        activity.activityType === 30010
		        ||
		        activity.activityType === 30015
		        ||
		        activity.activityType === 30016
		        ||
		        activity.activityType === 30017
		        ||
		        activity.activityType === 30018
		        ||
		        activity.activityType === 30019
		        ||
		        activity.activityType === 30020
		        ||
		        activity.activityType === 40000
		        ||
		        activity.activityType === 40001
		        ||
		        activity.activityType === 40002
		        ||
		        activity.activityType === 40003
		        ||
		        activity.activityType === 50000
		        ||
		        activity.activityType === 50001
		        ||
		        activity.activityType === 50002
		        ||
		        activity.activityType === 50003
		        ||
		        activity.activityType === 60001
		        ||
		        activity.activityType === 60002
		        ||
		        activity.activityType === 60004
		        ||
		        activity.activityType === 60005
		        ||
		        activity.activityType === 60006))
		    println(number)
		    //We now have to add the activities where the user is not authorized
        val allactivities = explicitInnerJoin ++ privateAndPublicGroupActivitiesOfMember(userIDLong, userIDLong, maxTimestamp, true)

		    val listJsValues = allactivities./*MYSQL DISTINCT WORKAROUND*/groupBy(x=>x).map(_._1).sortBy(_.modificationTimestamp.desc).take(number).list.map(x => convertIntoJSValue(x, userID.toLong))

		    Ok(Json.toJson(listJsValues))
          }
      }
      else{
        BadRequest
      }
  }
  
  /*
   * Retrieves all the activities of group that are public or that are private but that the user is allowed to see.
   * Mynewsfeed boolean is set to true if we want the user news feed so to discard every activity of him, and false if we want the activities of a member so only display his activities
   */
  def privateAndPublicGroupActivitiesOfMember(userID: Long, memberLookedID: Long, maxTimestamp: Long, mynewsFeed: Boolean) = {
    val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
    val groupsWhereUserIsMember = for {
    		  (groupMembership, group) <- Instantiations.Tables.groupMembership.filter(_.memberId === userID) innerJoin Instantiations.Tables.groups.filter(_.logicalDelete === 0) on
    		  ((groupMembership, group) => 
    		    groupMembership.groupId === group.id 
    		    )
        } yield group.id
   /*  TODO : delete this when we have more members*/
	 val groupsThatArePublic = for { c <- Instantiations.Tables.groups if (c.privacy === 0 && c.logicalDelete === 0) } yield c.id
	 
	 val groupsAllowed = groupsThatArePublic ++ groupsWhereUserIsMember
	 /*  TODO : end of delete, also replace groupsAllowed by groupsWhereUserIsMember downside */
	 
   val activitiesOfUserGroup = for {
     (activity, groupId) <- Instantiations.Tables.activity.filter(x => x.logicalDelete === 0 && x.modificationTimestamp < maxTimestampParam) innerJoin groupsAllowed on 
       ((activity, groupId) =>
    	   activity.groupPrivacy === groupId &&
    	   ((activity.memberId === memberLookedID && !mynewsFeed ) || (activity.memberId =!= userID && mynewsFeed))
    	 )
     } yield activity
    activitiesOfUserGroup
  }
/*
  def getAuthorizedActivity(userID: Long)= Instantiations.Tables.db.withSession { implicit session =>
    
    val groupsWhereUserIsMember = for {
		  (groupMembership, group) <- Instantiations.Tables.groupMembership.sortBy(_.modificationTimestamp.desc) innerJoin Instantiations.Tables.groups on
		  ((groupMembership, group) => 
		    groupMembership.memberId === userID &&
		    groupMembership.groupId === group.id && 
		    group.logicalDelete === 0
		    )
    } yield group
    val activitiesDebates = for {(activity, debate) <- Instantiations.Tables.activity innerJoin Instantiations.Tables.debate on
      ((activity, debate) =>
        activity.activityObjectType === 3 && //only for debates
        activity.activityObject === debate.id
        )
    }
    0
}*/
  /*
   * PARALLEL FUNCTION
   * Records the action done by an user, creates an activity and a notification. The type of actions can be seen in the Tables => ActivityTable object
   */
  def actionDone(userIDInt: Long, activityObjectType: Int, activityObject: Long, activityTarget: Long, activityType: Int, additionalInfo: String, groupPrivacy: Long = -1l){
    val thread = new Thread(new Runnable {
      def run() {
        Instantiations.Tables.db.withSession { implicit session =>
          val activityID = addOnActivityTable(userIDInt, activityObjectType, activityObject, activityTarget,  activityType, groupPrivacy)
          addNotification(userIDInt, activityID, activityObjectType, activityObject, activityTarget, activityType)
        }
      }
    })
    thread.start;  
  }
  
  def addNotification(userIDInt: Long, activityID: Long, activityObjectType: Int, activityObject: Long, activityTarget: Long, activityType: Int) = {
    Instantiations.Tables.db.withSession { implicit session =>
     try { 
     var newNotification = false
     var mailToSend = ("", "")  //First we add a notification to the owner of the object
     activityType match {
       
        case 100 => /*FollowMember*/ {
          //We only send a notification and mail to the followed member
          Instantiations.Tables.notification += (activityObject, activityID, 0, 0)
          val memberFollowedInfo = User.getMemberInfoFromID(activityObject)
          val memberFollowingInfo = User.getMemberInfoFromID(userIDInt)
          val memberFollowingStats = User.getUserStatsObject(userIDInt)
          implicit val messages = Instantiations.getUserMainLang(memberFollowedInfo.id)
          Mailer.sendEmail("", 
                    memberFollowedInfo.mail, 
                    Messages("mail.followMember.message") + " " + memberFollowingInfo.username, 
                    views.html.mails.mailFollowMember.render("", memberFollowingInfo, memberFollowingStats, Instantiations.getUserMainLang(memberFollowedInfo.id)).body
                    )
        }
        case 30001 /*DebateYesOrNoParticipation*/ => {
          
          //We send a mail to each of the debate participants
          //We get the opinion posted
          val yesOrNoOpinionObject =  Debates.getYesOrNoOpinionObject(activityTarget)
          //We get the debates participants
          val participantsOfDebate = Debates.getParticipantsOfDebate(activityObject, userIDInt).map(x => User.getMemberInfoFromID(x))
          //We send a mail to each of them
          participantsOfDebate.map{ memberInfo => 
            if (memberInfo.id != userIDInt) {
              Instantiations.Tables.notification += (memberInfo.id, activityID, 0, 0)
              if (User.getUserSendMail(memberInfo.id) == 1){
                implicit val messages = Instantiations.getUserMainLang(memberInfo.id)
                Mailer.sendEmail("", 
                    memberInfo.mail, 
                    yesOrNoOpinionObject.username + " " + Messages("mail.postopinion.message") + " " + Debates.getDebateObject(yesOrNoOpinionObject.idDebate, userIDInt).question, 
                    views.html.mails.postYesOrNoOpinion.render("", yesOrNoOpinionObject, userIDInt, Instantiations.getUserMainLang(memberInfo.id)).body
                    )
              }
            }
          }
        }
        case 30015 /*DebateOpinionParticipation*/ => {
          //We send a mail to each of the debate participants
          //We get the opinion posted
          val opinionObject =  Debates.getDebateOpinionObject(activityTarget)
          //We get the debates participants
          val participantsOfDebate = Debates.getParticipantsOfDebate(activityObject, userIDInt).map(x => User.getMemberInfoFromID(x))
          //We send a mail to each of them
          participantsOfDebate.map{ memberInfo => 
            if (memberInfo.id != userIDInt) {
              Instantiations.Tables.notification += (memberInfo.id, activityID, 0, 0)
              if (User.getUserSendMail(memberInfo.id) == 1){
                implicit val messages = Instantiations.getUserMainLang(memberInfo.id)
                Mailer.sendEmail("", memberInfo.mail, opinionObject.username + " " + Messages("mail.postopinion.message") + " " + opinionObject.debateQuestion, views.html.mails.postOpinion.render("", opinionObject, Instantiations.getUserMainLang(memberInfo.id)).body)
              }
            }
          }
        }
        case 30020 /*PostYesOrNoOpinionOnOpinion*/ => {
          val opinionObject = Debates.getDebateOpinionObject(activityObject)
          val memberToNotify = opinionObject.idMember 
          
          if (memberToNotify != userIDInt){
            Instantiations.Tables.notification += (memberToNotify, activityID, 0, 0)
            implicit val messages = Instantiations.getUserMainLang(memberToNotify)
            if (User.getUserSendMail(memberToNotify) == 1){
              val userInfo = User.getMemberInfoFromID(memberToNotify)
              //newNotification = true
              //views.html.mails.mailActivities.render("hehe").body
              mailToSend = (Messages("mail.yesornoopinionOnOpinion.title"), views.html.mails.yesornoopinionOnOpinion.render("", opinionObject, Instantiations.getUserMainLang(memberToNotify)).body)  //First we add a notification to the owner of the object
              Mailer.sendEmail("", userInfo.mail, mailToSend._1, mailToSend._2)
            }
          }
        }
        case 60001 /*CommentGroup*/ => {
          val groupObject = Groups.getGroupModel(activityObject)
          val membersToNotify = Groups.getMembersInGroupOrderedByTimestampRequest(activityObject, -1).list
          membersToNotify.par.map(x => User.convertMemberInfoDatabaseEntryToClass(x._1)).map{ userInfo =>
            implicit val messages = Instantiations.getUserMainLang(userInfo.id)
            if (userInfo.id != userIDInt){
              Instantiations.Tables.notification += (userInfo.id, activityID, 0, 0)
              if (User.getUserSendMail(userInfo.id) == 1){
                newNotification = true
                mailToSend = (Messages("mail.commentOnGroup.title"), views.html.mails.commentOnObject.render("", groupObject, Instantiations.getUserMainLang(userInfo.id)).body)  //First we add a notification to the owner of the object
                Mailer.sendEmail("", userInfo.mail, mailToSend._1, mailToSend._2)
              }
            }
          }
        }
        case 60002 /*CommentConcept*/ => {
          val conceptObject = Concepts.getConceptModel(activityObject)
          val memberToNotify = conceptObject.idCreator
          implicit val messages = Instantiations.getUserMainLang(memberToNotify)
          if (memberToNotify != userIDInt){
            Instantiations.Tables.notification += (memberToNotify, activityID, 0, 0)
            if (User.getUserSendMail(memberToNotify) == 1){
              val userInfo = User.getMemberInfoFromID(memberToNotify)
              newNotification = true
              mailToSend = (Messages("mail.commentOnConcept.title"), views.html.mails.commentOnObject.render("", conceptObject, Instantiations.getUserMainLang(memberToNotify)).body)  //First we add a notification to the owner of the object
              Mailer.sendEmail("", userInfo.mail, mailToSend._1, mailToSend._2)
            }
          }
        }
        case 60004 /*CommentAction*/ =>{
          val actionObject = Actions.getActionModel(activityObject)
          val memberToNotify = actionObject.creatorID
          implicit val messages = Instantiations.getUserMainLang(memberToNotify)
          if (actionObject.creatorType == 0 && memberToNotify != userIDInt){//We only notify for actions created by an user
            Instantiations.Tables.notification += (memberToNotify, activityID, 0, 0)
            if (User.getUserSendMail(memberToNotify) == 1){
              val userInfo = User.getMemberInfoFromID(memberToNotify)
              newNotification = true
              mailToSend = (Messages("mail.commentOnAction.title"), views.html.mails.commentOnObject.render("", actionObject, Instantiations.getUserMainLang(memberToNotify)).body)  //First we add a notification to the owner of the object
              Mailer.sendEmail("", userInfo.mail, mailToSend._1, mailToSend._2)
            }
          }
        }
        case 60005 /*CommentPetition*/ => {
          val petitionObject = Petitions.getPetitionModel(activityObject)
          val memberToNotify = petitionObject.creatorID
          implicit val messages = Instantiations.getUserMainLang(memberToNotify)
          if (petitionObject.creatorType == 0 && memberToNotify != userIDInt){//We only notify for petitions created by an user
            Instantiations.Tables.notification += (memberToNotify, activityID, 0, 0)
            if (User.getUserSendMail(memberToNotify) == 1){
              val userInfo = User.getMemberInfoFromID(memberToNotify)
              newNotification = true
              mailToSend = (Messages("mail.commentOnPetition.title"), views.html.mails.commentOnObject.render("", petitionObject, Instantiations.getUserMainLang(memberToNotify)).body)  //First we add a notification to the owner of the object
              Mailer.sendEmail("", userInfo.mail, mailToSend._1, mailToSend._2)
            }
          }
        }
        case 60006 /*CommentYesornoopinion*/ => {
          val yesOrNoOpinionObject = Debates.getYesOrNoOpinionObject(activityObject)
          val memberToNotify = yesOrNoOpinionObject.idMember
          implicit val messages = Instantiations.getUserMainLang(memberToNotify)
          if (memberToNotify != userIDInt){
            Instantiations.Tables.notification += (memberToNotify, activityID, 0, 0)
            if (User.getUserSendMail(memberToNotify) == 1){
              val userInfo = User.getMemberInfoFromID(memberToNotify)
              newNotification = true
              mailToSend = (Messages("mail.commentOnYesOrNoOpinion.title"), views.html.mails.commentOnObject.render("", yesOrNoOpinionObject, Instantiations.getUserMainLang(memberToNotify)).body)  //First we add a notification to the owner of the object
              Mailer.sendEmail("", userInfo.mail, mailToSend._1, mailToSend._2)
            }
          }
        }
        case _ =>
      }
      if (newNotification){
        //Then we add a notification to each user that commented the object
        val q2 = for {c <- Instantiations.Tables.comment if c.objectCommentedType === activityObjectType.toLong && c.objectCommentedID === activityObject && c.logicalDelete === 0} yield c.idSender
        q2.list.foreach{x => 
          println(x + " " + userIDInt)
          if (x != userIDInt){
            try{
              Instantiations.Tables.notification += (x, activityID, 0, 0)
              Mailer.sendEmail("", User.getMemberInfoFromID(x).mail, mailToSend._1, mailToSend._2)
            }
            catch {
              case e: com.mysql.jdbc.exceptions.jdbc4.MySQLIntegrityConstraintViolationException => {
                //We do nothing, because the user has already received the notification
              }
            }
          }
        }
      }
    } catch {
     case e: Exception => println("exception")
   }
    }
    
  }
  
  /*
   * Returns 1 if the user has new notifications and 0 if not
   */
  def hasUserNewNotificationsJSON = Connection.withConnection{
    username => userID => implicit request => implicit lang =>
      val userIDLong = userID.toLong
      Instantiations.Tables.db.withSession { implicit session =>
        val q5 = for { c <- Instantiations.Tables.notification if (c.memberNotified === userIDLong && c.sawStatus === 0) } yield c.sawStatus
        if (q5.list != Nil)
          Ok(Json.toJson(1))
        else
          Ok(Json.toJson(0))
      }
  }
  
  /*
   * Gets all the notifications of an user
   */
  def getNotificationsOfUserJSON(maxTimestamp: Long, number: Int) = Connection.withConnection{
    username => userID => implicit request => implicit lang =>
      val userIDLong = userID.toLong
      val maxTimestampParam = new java.sql.Timestamp(Application.timestampToSearch(maxTimestamp))
      
      Instantiations.Tables.db.withSession { implicit session =>
        val notificationsOfUser = Instantiations.Tables.notification.filter(x => x.memberNotified === userIDLong && x.logicalDelete === 0)
        val q2 = for {
  		    (notification, activity) <- notificationsOfUser innerJoin Instantiations.Tables.activity.filter(_.logicalDelete === 0) on 
  		    ((notification, activity) => notification.activityID === activity.id && activity.modificationTimestamp < maxTimestampParam)
        } yield (activity, notification.sawStatus)
        val activityAndSeen = q2.sortBy(_._1.modificationTimestamp.desc).take(number).list
        
        val jsonToSend = activityAndSeen.map(x => convertIntoJSValue(x._1, userID.toLong, true).as[JsObject] + ("saw" -> Json.toJson(x._2)))
        
        //We set every notification unseen of the user to "seen"
        val q5 = for { c <- Instantiations.Tables.notification if (c.memberNotified === userIDLong && c.sawStatus === 0) } yield c.sawStatus
        q5.update(1) //We set "saw" to 1
        val statement5 = q5.updateStatement
        val invoker5 = q5.updateInvoker
        
        Ok(Json.toJson(jsonToSend))
      }
  }
  
  /*
   * Adds the action on the activity table
   */
  def addOnActivityTable(userIDInt: Long, activityObjectType: Int, activityObject: Long, activityTarget: Long, activityType: Int, groupPrivacy: Long): Long = {
  	Instantiations.Tables.db.withSession { implicit session =>
  	  (Instantiations.Tables.activity returning Instantiations.Tables.activity.map(_.id)) += ((None, userIDInt, activityObjectType, activityObject, activityTarget,activityType, new java.sql.Timestamp(System.currentTimeMillis()), Option(new java.sql.Timestamp(System.currentTimeMillis())), 0, groupPrivacy)) //We add it t the activity of the member
  	}
  }
  
/* 
   * PARALLEL FUNCTION
   * Adds a view on an object
   * TYPES OF THE OBJECTS : 
   * 0 MEMBER
   * 1 GROUP
   * 2 CONCEPT
   * 3 DEBATE
   * 4 ACTION
   * 5 PETITION
   * 6 YESORNOOPINION
   * 7 OPINION
   */
  
  def addView(objectType: Long, objectID: Long){
    val thread = new Thread(new Runnable {
      def run() {
        Instantiations.Tables.db.withSession { implicit session =>
          val viewsObject = Instantiations.Tables.numberOfViews.filter(x => x.typeOfObject === objectType && x.idOfObject === objectID).list
          if (viewsObject != Nil)
            sqlu"update NUMBEROFVIEWS set NUMBEROFVIEWS=NUMBEROFVIEWS+1 where ID_OBJECT = $objectID AND TYPE_OBJECT = $objectType".first
          else
            Instantiations.Tables.numberOfViews += (objectType, objectID, 1)
        }
      }
    })
    thread.start;   
  }
  /* 
   * PARALLEL FUNCTION
   * Deletes a view on an object
   * TYPES OF THE OBJECTS : 
   * 0 MEMBER
   * 1 GROUP
   * 2 CONCEPT
   * 3 DEBATE
   * 4 ACTION
   * 5 PETITION
   * 6 YESORNOOPINION
   * 7 OPINION
   */
  def delView(objectType: Long, objectID: Long){
    val thread = new Thread(new Runnable {
      def run() {
        Instantiations.Tables.db.withSession { implicit session =>
        val viewsObject = Instantiations.Tables.numberOfViews.filter(x => x.typeOfObject === objectType && x.idOfObject === objectID).list
        if (viewsObject != Nil)
          sqlu"update NUMBEROFVIEWS set NUMBEROFVIEWS=NUMBEROFVIEWS-1 where ID_OBJECT = $objectID AND TYPE_OBJECT = $objectType".first
        }
      }
    })
    thread.start;  
  }

}
