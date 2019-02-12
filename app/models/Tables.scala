package models

import slick.driver.MySQLDriver.simple._
import java.sql.Timestamp

object Types {
  type memberInfoEntry = (Long, String, String, String, String, String, String, java.sql.Date, Int, String, String, String, Int, Int, String, java.sql.Timestamp, Option[java.sql.Timestamp], Long, Int, Int, Int)
  type memberProfilePictureEntry = (Long, String, Int, Int, String, Int, Int, String)
  type memberStatsEntry = (Long, Long, Long, Long)
  type conceptEntry = (Option[Long], String, String, Long, Int, Int, java.sql.Timestamp, Option[java.sql.Timestamp], Int, String)
  type groupEntry = (Option[Long], String, String, Int, Int, java.sql.Timestamp, Option[java.sql.Timestamp], Int, String)
  type debateEntry = (Option[Long], Long, String, Int, Long, Long, java.sql.Timestamp, Option[java.sql.Timestamp], Int, String)
  type debateYesOrNoEntry = (Option[Long], Long, String, Long, Int, Int, Int, Int, Long, Long, java.sql.Timestamp, Option[java.sql.Timestamp], Int)
  type debateOpinionEntry = (Option[Long], Long, String, Long, Int, Int, Int, Int, Long, Long, java.sql.Timestamp, Option[java.sql.Timestamp], Int)
  type petitionEntry = (Option[Long], Int, Long, String, String, Long, Long, java.sql.Timestamp, Option[java.sql.Timestamp], Int, String)
  type activityTableEntry = (Option[Long], Long, Int, Long, Long, Int, java.sql.Timestamp, Option[java.sql.Timestamp], Int, Long)
  type actionEntry = (Option[Long], Int, Long, String, String, String, String, Double, Double, Long, Long, Long, java.sql.Timestamp, Option[java.sql.Timestamp], Int, String)
  type followEntry = (Long, Int, Long, Long, java.sql.Timestamp, Option[java.sql.Timestamp])
  type conversation = (Option[Long], String, java.sql.Timestamp, Option[java.sql.Timestamp])
  type conversationMemberEntry = (Long, Long, Boolean, java.sql.Timestamp, Option[java.sql.Timestamp])
  type conversationMessagesEntry = (Option[Long], Long, Long, String, java.sql.Timestamp, Option[java.sql.Timestamp])
  type commentEntry = (Option[Long], Long, Long, Long, String, java.sql.Timestamp, Option[java.sql.Timestamp], Int)
  type memberFriendsMails = (Long, String, Long, java.sql.Timestamp, Option[java.sql.Timestamp])
}
//Here we define all our Tables, and the Case classes to represent them (to be abble to pass all the data easily in the views)

abstract class EntryClass(){
  val id: Long
}

class DoingActionTable(tag: Tag) extends Table[(Long, java.sql.Timestamp)](tag, "DOINGACTION") {
  
  def memberId = column[Long]("ID_MEMBER", O.PrimaryKey) // This is the primary key column
  def start_timestamp = column[java.sql.Timestamp]("START_TIMESTAMP")
  def * = (memberId, start_timestamp)
}

class MembersTable(tag: Tag) extends Table[(Option[Long], String, String, String)](tag, "MEMBERS") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def name = column[String]("SUP_NAME")
  def username = column[String]("USERNAME")
  def password = column[String]("PASSWORD")
  
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id.?, name, username, password)
}

class MailsUnsubscribed(tag: Tag) extends Table[(String)](tag, "MAILSUNSUBSCRIBED") {
  
  def mail = column[String]("MAIL")
  def * = (mail)
}

class MembersInfoTable(tag: Tag) extends Table[Types.memberInfoEntry](tag, "MEMBERSINFO") {
//(Long, String, String, String, String, String, String, java.sql.Timestamp, 
  //Int, String, String, String, Int, java.sql.Timestamp, Option[java.sql.Timestamp], Long, Int, Int)
  def id = column[Long]("SUP_ID", O.PrimaryKey) // This is the primary key column
  def firstname = column[String]("SUP_FIRSTNAME")
  def name = column[String]("SUP_NAME")
  def mail = column[String]("MAIL")
  def ville = column[String]("VILLE")
  def presentation = column[String]("PRESENTATION", O.DBType("MEDIUMTEXT"))
  def address = column[String]("ADDRESS")
  /*def idfacebook = column[Long]("ID_FACEBOOK") //id facebook
  def idtwitter = column[Long]("ID_TWITTER") //id twitter
  def idgoogle  = column[Long]("ID_GOOGLE")*/ //id google
  def birthday = column[java.sql.Date]("BIRTHDAY_TIMESTAMP")
  def gender = column[Int]("GENDER")
  def link = column[String]("LINK")
  def location = column[String]("LOCATION")
  def lang = column[String]("LANG")
  def verified = column[Int]("VERIFIED")
  def id_typepaper = column[Int]("IDTYPEPAPER")
  def id_passport = column[String]("ID_PASSPORT")
  
  
  def modificationTimestamp = column[java.sql.Timestamp]("LAST_ACTIVITY_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("SUBSCRIPTION_TIMESTAMP")
  def numberOfActions = column[Long]("NUMBER_ACTIONS")
  def firstconnection = column[Int]("FIRSTCONNECTION") //0 if the user was not connected yet, 1 if it already performed the connections steps
  def sendMail = column[Int]("SENDMAIL") //1 if the user is ok for us to send him mail, 0 if not
  def hasAlreadySeenInviteFriends = column[Int]("INVITEFRIENDS") //0 if the user has not yet seen the sendmail screen, 1 if he already did
  
  def foreignKeyidMember = foreignKey("fk_idMemberMemberInfo", id, controllers.Instantiations.Tables.members)(_.id)

  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id, firstname, name, mail, ville, presentation, address, birthday, gender, link, location, lang, verified, id_typepaper, id_passport, modificationTimestamp, creationTimestamp?, numberOfActions, firstconnection, sendMail, hasAlreadySeenInviteFriends)
}

case class MemberInfo(id: Long, username: String, firstname: String, name: String, mail: String, ville: String, presentation: String, rank: Int, isUserFollowing: Int, modificationTimestamp: Long) extends EntryClass


class MemberExternalIdsTable(tag: Tag) extends Table[(Long, Long, Long, Long)](tag, "MEMBERSEXTERNALIDS")  {
  def id = column[Long]("SUP_ID", O.PrimaryKey) // This is the primary key column
  def idfacebook = column[Long]("ID_FACEBOOK") //id facebook
  def idtwitter = column[Long]("ID_TWITTER") //id twitter
  def idgoogle  = column[Long]("ID_GOOGLE") //id google
  
  def foreignKeyidMember = foreignKey("fk_idMemberMemberExternalIds", id, controllers.Instantiations.Tables.members)(_.id)

  def * = (id, idfacebook, idtwitter, idgoogle)

}

/*
 * Stores information about profile pictures
 */
class MemberProfilePictureTable(tag: Tag) extends Table[Types.memberProfilePictureEntry](tag, "MEMBERSPROFILEPICTURE")  {
  def id = column[Long]("SUP_ID", O.PrimaryKey) // This is the primary key column
  def picture_url = column[String]("PICTURE_URL")
  def picture_offx = column[Int]("PICTURE_OFFX")
  def picture_offy = column[Int]("PICTURE_OFFY")
  def picturemin_zoom = column[String]("PICTURE_ZOOM")
  def picturemin_offx = column[Int]("PICTUREMIN_OFFX")
  def picturemin_offy = column[Int]("PICTUREMIN_OFFY")
  def picture_zoom = column[String]("PICTUREMIN_ZOOM")
  
  def foreignKeyidMember = foreignKey("fk_idMemberMemberProfilePicture", id, controllers.Instantiations.Tables.members)(_.id)

  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id, picture_url, picture_offx, picture_offy, picturemin_zoom, picturemin_offx, picturemin_offy, picture_zoom)
}

case class MemberProfilePicture(id: Long, picture_url: String, picture_offx: Int, picture_offy: Int, picturemin_offx: Int, picturemin_offy: Int)

/*
 * Stores the langs of each user
 */
class MemberLanguageTable(tag: Tag) extends Table[(Long, String)](tag, "MEMBERSLANGUAGE")  {
  def idMember = column[Long]("IDMEMBER") // This is the primary key column
  def language = column[String]("LANGUAGE")
  
  def foreignKeyidMember = foreignKey("fk_idMemberMemberLAng", idMember, controllers.Instantiations.Tables.members)(_.id)
  
  def pk = primaryKey("pk_MEMBERLANGUAGETABLE", (idMember, language))
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (idMember, language)
}

 case class MemberLanguages(id: Long, languages: Seq[String])

 /*
 * Stores the stats of each user
 */
class MemberStatsTable(tag: Tag) extends Table[Types.memberStatsEntry](tag, "MEMBERSTATS")  {
  def idMember = column[Long]("IDMEMBER", O.PrimaryKey) // This is the primary key column
  def numberFollowers = column[Long]("FOLLOWERS")
  def numberLikes = column[Long]("LIKES")
  def numberDislikes = column[Long]("DISLIKES")
  
  def foreignKeyidMember = foreignKey("fk_idMemberMemberStats", idMember, controllers.Instantiations.Tables.members)(_.id)
  
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (idMember, numberFollowers, numberLikes, numberDislikes)
}
 
 case class MemberStats(idMember: Long, numberFollowers: Long, numberLikes: Long, ranking: Long, totalNumberMembers: Long)
 
 /*
 * Stores the langs of each user
 */
class MemberStatsMonthlyTable(tag: Tag) extends Table[(Long, Long, Long)](tag, "MEMBERSTATSMONTHLY")  {
  def idMember = column[Long]("IDMEMBER", O.PrimaryKey) // This is the primary key column
  def numberLikes = column[Long]("LIKES")
  def numberDislikes = column[Long]("DISLIKES")
  
  def foreignKeyidMember = foreignKey("fk_idMemberMemberStatsMonthly", idMember, controllers.Instantiations.Tables.members)(_.id)
  
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (idMember, numberLikes, numberDislikes)
}
 
 //case class MemberStatsMonthly(idMember: Long, numberLikes: Long, ranking: Long, totalNumberMembers: Long)
 
 /*
 * Stores the langs of each user
 */
class PasswordForgetTable(tag: Tag) extends Table[(Long, String, java.sql.Timestamp, Option[java.sql.Timestamp])](tag, "PASSWORDFORGET")  {
  def idMember = column[Long]("IDMEMBER") // This is the primary key column
  def secureHash = column[String]("SECUREHASH")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def foreignKeyidMember = foreignKey("fk_idMemberMemberLAng", idMember, controllers.Instantiations.Tables.members)(_.id)
  
  def pk = primaryKey("pk_MEMBERPASSWORDFORGET", (idMember))
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (idMember, secureHash, modificationTimestamp, creationTimestamp?)
}
 
//case class MemberLanguages(languages: List[String])

class ConceptTable(tag: Tag) extends Table[models.Types.conceptEntry](tag, "CONCEPT") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def name = column[String]("NAME")
  def definition = column[String]("DEFINITION", O.DBType("MEDIUMTEXT"))
  def idCreator = column[Long]("IDCREATOR")
  def membersFor = column[Int]("MEMBERSFOR")
  def membersAgainst = column[Int]("MEMBERSAGAINST")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def logicalDelete = column[Int]("LOGICALDELETE")
  def language = column[String]("LANGUAGE")
  
  def foreignKeyidCreatorMember = foreignKey("fk_idCreatorConcept", idCreator, controllers.Instantiations.Tables.members)(_.id)
  
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id.?, name, definition, idCreator, membersFor, membersAgainst, modificationTimestamp, creationTimestamp?, logicalDelete, language)
}
case class Concept(id: Long, name: String, definition: String, idCreator:Long, membersFor: Int, membersAgainst: Int, language: String, modificationTimestamp: Long) extends EntryClass


/*
 * Table to store who voted for or against a specific concept
 */
class ConceptOpinionsTable(tag: Tag) extends Table[(Long, Long, Boolean, java.sql.Timestamp)](tag, "CONCEPTOPINIONS") {
  def voterID = column[Long]("SUP_ID_VOTER") // This is the primary key column
  def conceptID = column[Long]("ID_CONCEPT") // This is the primary key column
  def opinion = column[Boolean]("OPINION")
  def dateVote = column[java.sql.Timestamp]("DATEVOTE")
  
  def pk = primaryKey("pk_CONCEPTOPINIONS", (voterID, conceptID))
  def foreignKeyconceptID = foreignKey("fk_conceptIDConceptOpinion", conceptID, controllers.Instantiations.Tables.concepts)(_.id)
  def foreignKeyidMember = foreignKey("fk_memberConceptOpinion", voterID, controllers.Instantiations.Tables.members)(_.id)
 
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (voterID, conceptID, opinion, dateVote)
}

class GroupTable(tag: Tag) extends Table[Types.groupEntry](tag, "GROUP") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def name = column[String]("NAME")
  def definition = column[String]("DEFINITION", O.DBType("MEDIUMTEXT"))
  def numberMembers = column[Int]("NBREMEMBERS")
  /*
   * 0 : public : everybody can go
   * 1 : private : necessary approbation
   */
  def privacy = column[Int]("RESERVEDTOGROUP")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def logicalDelete = column[Int]("LOGICALDELETE")
  def language = column[String]("LANGUAGE")
  
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id.?, name, definition, numberMembers, privacy, modificationTimestamp, creationTimestamp?, logicalDelete, language)
}

case class Group(id: Long, name: String, definition:String, numberMembers: Int, privacy: Int, language: String, modificationTimestamp: Long) extends EntryClass

class GoupApplicationsTable(tag: Tag) extends Table[(Option[Long], Long, Long, java.sql.Timestamp, Option[java.sql.Timestamp])](tag, "GROUPAPPLICATION") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc)
  def idMember = column[Long]("IDMEMBER")
  def idGroup = column[Long]("IDGROUP")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def foreignKeyidMember = foreignKey("fk_memberIdGroupApplication", idMember, controllers.Instantiations.Tables.members)(_.id)
  def foreignKeyidGroup = foreignKey("fk_groupIdGroupApplication", idGroup, controllers.Instantiations.Tables.groups)(_.id)
  
  def * = (id.?, idMember, idGroup, modificationTimestamp, creationTimestamp?)
  
}

class DebateTable(tag: Tag) extends Table[Types.debateEntry](tag, "DEBATE") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def idCreator = column[Long]("IDCREATOR")
  def question = column[String]("QUESTION")
  /*
   * Type of debate : 
   * 0 YES OR NO
   * 1 OPINION
   */
  def typeOfDebate = column[Int]("TYPEOFDEBATE")
  def privateGroupId = column[Long]("PRIVATEGROUPID") //-1 if the debate is not private
  def numberOfParticipations = column[Long]("NUMBEROFPARTICIPATIONS")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def logicalDelete = column[Int]("LOGICALDELETE")
  def language = column[String]("LANGUAGE")
  
  def foreignKeyidMember = foreignKey("fk_memberDebate", idCreator, controllers.Instantiations.Tables.members)(_.id)
 
  
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id.?, idCreator, question, typeOfDebate, privateGroupId, numberOfParticipations, modificationTimestamp, creationTimestamp?, logicalDelete, language)
}

case class Debate(id: Long, idCreator: Long, question:String, typeOfDebate: Int, numberOfParticipations: Long, language: String, privateGroupId: Long, isUserFollowing: Int, modificationTimestamp: Long) extends EntryClass




/*
 * Table to store the opinions of people on debates
 */

class OpinionDebateTable(tag: Tag) extends Table[Types.debateOpinionEntry](tag, "DEBATEOPINION") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def idDebate = column[Long]("IDDEBATE") // This is the primary key column
  def comment = column[String]("COMMENT", O.DBType("TEXT"))
  def idMember = column[Long]("IDMEMBER")
  def votefor = column[Int]("VOTEFOR")
  def voteagainst = column[Int]("VOTEAGAINST")
  def numberofyesorno = column[Int]("NUMBEROFYESORNO")
  def popularity = column[Int]("POPULARITY")
  def popularityTimestamp = column[Long]("POPULARITYTIMESTAMP")
  
  def idObjectUP = column[Long]("IDObjectUP")//The ID of the object where this opinion is attached
  
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def logicalDelete = column[Int]("LOGICALDELETE")
  
  def foreignKeyidMember = foreignKey("fk_memberDebateOpinion", idMember, controllers.Instantiations.Tables.members)(_.id)
  def foreignKeyidDebate = foreignKey("fk_idDebateDebateOpinion", idDebate, controllers.Instantiations.Tables.debate)(_.id)
 
  
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id?, idDebate, comment, idMember, votefor, voteagainst, numberofyesorno, popularity, popularityTimestamp, idObjectUP, modificationTimestamp, creationTimestamp?, logicalDelete)
}

case class DebateOpinion(id: Long, idDebate: Long, debateQuestion: String, comment:String, idMember: Long, username: String, votefor: Int, voteagainst: Int, numberofyesorno: Int, popularity: Long, modificationTimestamp: java.sql.Timestamp, creationTimestamp: java.sql.Timestamp) extends EntryClass

class VoteOpinionTable(tag: Tag) extends Table[(Long, Long, Int)](tag, "VOTEDEBATEOPINION") {
  def idOpinion = column[Long]("IDDEBATE") 
  def idMember = column[Long]("IDMEMBER") 
  def vote = column[Int]("VOTE") 
  
  def pk = primaryKey("pk_voteYEsOrNoTable", (idOpinion, idMember))
  def foreignKeyidMember = foreignKey("fk_memberVoteOpinion", idMember, controllers.Instantiations.Tables.members)(_.id)
  def foreignKeyidOpinion = foreignKey("fk_opinionVoteOpinion", idOpinion, controllers.Instantiations.Tables.debateOpinion)(_.id)
  
   // Every table needs a * projection with the same type as the table's type parameter
  def * = (idOpinion, idMember, vote)

}

/*
 * Table to store the yes or no opinions of people on debates
 */

class OpinionYesOrNoDebateTable(tag: Tag) extends Table[Types.debateYesOrNoEntry](tag, "DEBATEYESORNO") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def idDebate = column[Long]("IDDEBATE") // This is the primary key column
  def comment = column[String]("COMMENT", O.DBType("TEXT"))
  def idMember = column[Long]("IDMEMBER")
  def opinion = column[Int]("OPINION") //0 : againt - 1 : for
  def votefor = column[Int]("VOTEFOR")
  def voteagainst = column[Int]("VOTEAGAINST")
  def popularity = column[Int]("POPULARITY")
  def popularityTimestamp = column[Long]("POPULARITYTIMESTAMP")
  
  def idObjectUP = column[Long]("IDObjectUP")//The ID of the object where this opinion is attached
  
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def logicalDelete = column[Int]("LOGICALDELETE")
  
  def foreignKeyidMember = foreignKey("fk_memberDebateYesOrNo", idMember, controllers.Instantiations.Tables.members)(_.id)
  def foreignKeyidDebate = foreignKey("fk_idDebateDebateYesOrNo", idDebate, controllers.Instantiations.Tables.debate)(_.id)
 
  
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id?, idDebate, comment, idMember, opinion, votefor, voteagainst, popularity, popularityTimestamp, idObjectUP, modificationTimestamp, creationTimestamp?, logicalDelete)
}

case class DebateYesOrNot(id: Long, idDebate: Long, comment:String, idMember: Long, username: String, opinion: Int, votefor: Int, voteagainst: Int, popularity: Long, modificationTimestamp: java.sql.Timestamp, creationTimestamp: java.sql.Timestamp) extends EntryClass


/*
 * Table that stores the votes on yes or no opinions (that can be comments on opinion debates as well as replies on
 * yes or no debates)
 */
class VoteYesOrNoTable(tag: Tag) extends Table[(Long, Long, Int)](tag, "VOTEDEBATEYESORNO") {
  def idOpinion = column[Long]("IDDEBATE") 
  def idMember = column[Long]("IDMEMBER") 
  def vote = column[Int]("VOTE") 
  
  def pk = primaryKey("pk_voteYEsOrNoTable", (idOpinion, idMember))
  def foreignKeyidMember = foreignKey("fk_memberVoteYesOrNo", idMember, controllers.Instantiations.Tables.members)(_.id)
  def foreignKeyidOpinion = foreignKey("fk_opinionVoteYesOrNo", idOpinion, controllers.Instantiations.Tables.debateYesOrNo)(_.id)
  
   // Every table needs a * projection with the same type as the table's type parameter
  def * = (idOpinion, idMember, vote)

}

/*
 * Table that stores the current random opinions to be displayed every 3 opinions for each member
 */
class DebateRandomOpinionTable(tag: Tag) extends Table[(Long, Long, Int, Long, java.sql.Timestamp, Option[java.sql.Timestamp])](tag, "DEBATERANDOMOPINION") {
  def idDebate = column[Long]("IDDEBATE") 
  def idMember = column[Long]("IDMEMBER") 
  def position = column[Int]("POSITION") 
  def idOpinion = column[Long]("IDOPINION") 
  
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP") //Update randomTable if This TS is older than one hour
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def pk = primaryKey("pk_voteYEsOrNoTable", (idDebate, idMember, position))
  def foreignKeyidMember = foreignKey("fk_memberVoteYesOrNo", idMember, controllers.Instantiations.Tables.members)(_.id)
  def foreignKeyidOpinion = foreignKey("fk_idDetdate", idDebate, controllers.Instantiations.Tables.debate)(_.id)
  
   // Every table needs a * projection with the same type as the table's type parameter
  def * = (idDebate, idMember, position, idOpinion, modificationTimestamp, creationTimestamp?)


}

/*
 * Stores the membership of the users to a group
 */
class GroupMembershipTable(tag: Tag) extends Table[(Long, Long, Int, java.sql.Timestamp, Option[java.sql.Timestamp])](tag, "GROUPMEMBERSHIP") {
  
  def memberId = column[Long]("ID_MEMBER") // This is the primary key column
  def groupId = column[Long]("ID_GROUP") // This is the primary key column
  def roleGroup = column[Int]("ROLE") //1 - Member / 2 - Leader / 1000 - Admin
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def pk = primaryKey("pk_GROUPMEMBERSHIP", (memberId, groupId))
  def foreignKeyconceptID = foreignKey("fk_groupIDGroupMemberShip", groupId, controllers.Instantiations.Tables.groups)(_.id)
  def foreignKeyidMember = foreignKey("fk_memberGroupMemberShip", memberId, controllers.Instantiations.Tables.members)(_.id)
  def * = (memberId, groupId, roleGroup, modificationTimestamp, creationTimestamp?)
}

case class GroupMembership(memberId: Long, groupID: Long, rank: Int, modificationTimestamp: Long)

/*
 * Stores the activity of a member to display in his page
 */
class ActivityTable(tag: Tag) extends Table[Types.activityTableEntry](tag, "ACTIVITY") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def memberId = column[Long]("ID_MEMBER") // The id of the member that did this activity
  def activityObjectType = column[Int]("ID_TYPE") // The type of the activity object
  def activityObject = column[Long]("ID_OBJECT") // With what what did he do this (the id varies followoing the activity type ex : if the activity is about groups, the id will be a groupID, if about a member, a memberID)
  def activityTarget = column[Long]("ID_TARGET") // On what did he do this (the id varies too)
  /* ACTIVITY TYPE : TELLS WHAT KIND OF ACTIVITY THE MEMBER DID
   * 00000 ProfileCreation //memberID : The id of the new account / Object: The id of the new account / Target : The id of the new account
   * 00001 ProfileModification //memberID : The id of the modified account / Object: The id of the modified account / Target : The id of the modified account
   * 00002 ProfileFirstNameModification //memberID : The id of the modified account / Object: The id of the modified account / Target : The id of the modified account
   * 00003 ProfileNameModification //memberID : The id of the modified account / Object: The id of the modified account / Target : The id of the modified account
   * 00004 ProfileVilleModification //memberID : The id of the modified account / Object: The id of the modified account / Target : The id of the modified account
   * 00005 ProfileMailModification //memberID : The id of the modified account / Object: The id of the modified account / Target : The id of the modified account
   * 00006 ProfilePresentationModification //memberID : The id of the modified account / Object: The id of the modified account / Target : The id of the modified account
   * 00100 FollowMember //memberID : The id of the users that follows / Object: The type of the member followed / Target : The id of the member followed
   * 
   * 10000-GroupCreation //memberID : The member who created the group / Object: the group / Target : The group
   * 10001-GroupJoin //memberID : The member who joined the group / Object: the group / Target : The member who joined the group
   * 10002-Group change rank : Member //memberID : The member who changed the rank/ Object: the group / Target : The member that got his rank changed
   * 10003-Group change rank : Leader
   * 10004-Group change rank : Admin
   * 10005-GroupAccept //memberID : The member who accepted the other member in the group / Object: the group / Target : The member accepted
   * 10010-GroupLeave //memberID : The member who leaved the group / Object: the group / Target : The member who leaved the group
   * 10100-GroupDescriptionModification //memberID : The member who changed the group description / Object: the group / Target : The member group
   * 10101-GroupNameModification //memberID : The member who changed the group name / Object: the group / Target : The member group
  
   * 
   * 20000-ConceptCreation //memberID : User that created the concept / Object: the concept / Target : the concept
   * 20001-ConceptDescriptionModification //memberID : User that created the concept / Object: the concept / Target : the concept
   * 20002-VoteFor Concept //memberID : User that voted for the concept / Object: the concept / Target : the concept
   * 20003-VoteAgainst Concept //memberID : User that voted for the concept / Object: the concept / Target : the concept
   * 20004-CancelVote //memberID : User that cancelled the vote for the concept / Object: the concept / Target : the concept
   * 20005-Modif vote Votefor -> VoteAgainst //memberID : User that modified the vote for the concept / Object: the concept / Target : the concept
   * 20006-Modif vote VoteAgainst -> Votefor //memberID : User that modified the vote for the concept / Object: the concept / Target : the concept
   * 
   * 30000-DebateCreation //memberID : The member who created the debate / Object: the debate / Target : The debate
   
   * 30001-DebateYesOrNoParticipation //memberID : The member who gave his opinion / Object: the id of the yes or no debate / Target : The id of the yes or no opinion
   * 30002-VoteFor Yes Or No Opinion //memberID : The member who gave his opinion / Object: the id of the yes or no debate of the opinion  / Target : The id of the yes or no opinion that is judged
   * 30003-VoteAgainst Yes Or No Opinion //memberID : The member who gave his opinion / Object: the id of the yes or no debate of the opinion  / Target : The id of the yes or no opinion that is judged
   * 30004-Modif VoteFor -> VoteAgainst Yes Or No Opinion //memberID : The member who changed his opinion / Object: the id of the yes or no debate of the opinion / Target : The id of the yes or no opinion that is judged
   * 30005-Modif VoteAgainst -> Votefor Yes Or No Opinion //memberID : The member who changed his opinion / Object: the id of the yes or no debate of the opinion / Target : The id of the yes or no opinion that is judged
   
   * 30010-DebateForGroupCreation //memberID : The member who created the debate / Object: the group of this debate / Target : The debate
   
   * 30015-DebateOpinionParticipation //memberID : The member who gave his opinion / Object: the id of the debate / Target : The id of the opinion
   * 30016-VoteFor Opinion //memberID : The member who gave his opinion / Object: the id of the debate of the opinion  / Target : The id of the opinion that is judged
   * 30017-VoteAgainst Opinion //memberID : The member who gave his opinion / Object: the id of the debate of the opinion  / Target : The id of the opinion that is judged
   * 30018-Modif VoteFor -> VoteAgainst Opinion //memberID : The member who changed his opinion / Object: the id of the debate of the opinion / Target : The id of the opinion that is judged
   * 30019-Modif VoteAgainst -> Votefor Opinion //memberID : The member who changed his opinion / Object: the id of the debate of the opinion / Target : The id of the opinion that is judged
   * 30020-PostYesOrNoOpinionOnOpinion //memberID : The member who posted his opinion / Object: the id of the opinion where the yes or no opinion was posted / Target : The id of the yes or no opinion
   
   * 40000 ActionCreation //memberID : The id of the user that create the action / Object: The id of the action/ Target : The id of the action created
   * 40001-GroupActionCreation //memberID : The member who create the action with the group / Object: the group / Target : The id of the action
   * 40002-JoinAction //memberID : The member who join the action / Object: the action / Target :the action
   * 40003-LeaveAction //memberID : The member who leave action / Object: The action / Target : The action
  
   * * 
   * 50000-PetitionCreation //memberID : The member who create the petition / Object: The petition id / Target : The petition id 
   * 50001-GroupPetitionCreation //memberID : The member who create the petition with the group / Object: The group / Target : The petition id 
   * 50002-PetitionJoin //memberID : The member who join petition / Object: The petition id / Target : The petition
   * 50003-PetitionLeave //memberID : The member who leave petition / Object: The petition id / Target : The petition
  

   * 60001-CommentGroup //memberID : The member that commented / Object: The group id / Target : The comment id
   * 60002-CommentConcept //memberID : The member that commented / Object: The concept id / Target : The comment id
   * 60004-CommentAction //memberID : The member that commented / Object: The action id / Target : The comment id
   * 60005-CommentPetition //memberID : The member that commented / Object: The petition id / Target : The comment id
   * 60006-CommentYesornoopinion //memberID : The member that commented / Object: The yesorno opinion id / Target : The comment id
   * * */
  def activityType = column[Int]("ACTIVITY_TYPE")
  //def activityComment = column[String]("COMMENT")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def logicalDelete = column[Int]("LOGICALDELETE")
  
  def groupPrivacy = column[Long]("GROUPPRIVACY") //-1 if public and the id of the group if not
  
  def foreignKeyidMember = foreignKey("fk_ActivityMember", memberId, controllers.Instantiations.Tables.members)(_.id)
  def * = (id?, memberId, activityObjectType, activityObject, activityTarget, activityType, modificationTimestamp, creationTimestamp?, logicalDelete, groupPrivacy)
}

/*
 * Table containing the notifications.
 */
class NotificationTable(tag: Tag) extends Table[(Long, Long, Int, Int)](tag, "NOTIFICATIONS") {
  def memberNotified = column[Long]("MEMBERNOTIFIED") //The member that received the notification
  def activityID = column[Long]("ACTIVITYID") //The activity that the member is notified about
  def sawStatus = column[Int]("SAWSTATUS") //0 if the notification has not been seen, 1 if yes
  def logicalDelete = column[Int]("LOGICALDELETE")
  
  def pk = primaryKey("pk_ACTIONJOIN", (memberNotified, activityID))
  def * = (memberNotified, activityID, sawStatus, logicalDelete)
}

class ActionTable(tag: Tag) extends Table[Types.actionEntry](tag, "ACTION") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc) // This is the primary key column
  /* 0 : it is a member who created the action - 
   * 1 : it is a group who created the action
   * */
  def creatorType = column[Int]("TYPE_CREATOR") 
  def creatorID = column[Long]("ID_CREATOR") //ID of a creator - group or member
  def name = column[String]("NAME") // The ID, member or group, of the creator
  def definition = column[String]("DEFINITION", O.DBType("MEDIUMTEXT")) // This is the primary key column
  def nameLocation = column[String]("NAMELOCATION")
  def addressLocation = column[String]("ADDRESS")
  def latitude = column[Double]("LATITUDE") //X position on a map
  def longitude = column[Double]("LONGITUDE") //Y position on a map
  def timeBegin = column[Long]("TIMEBEGIN")
  def timeEnd = column[Long]("TIMEEND")
  def numberJoined = column[Long]("NBREJOINED")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def logicalDelete = column[Int]("LOGICALDELETE")
  def language = column[String]("LANGUAGE")
  
  def * = (id?, creatorType, creatorID, name, definition, nameLocation, addressLocation, latitude, longitude, timeBegin, timeEnd, numberJoined, modificationTimestamp, creationTimestamp?, logicalDelete, language)
}

class ActionJoinTable(tag: Tag) extends Table[(Long, Long, Int, java.sql.Timestamp, Option[java.sql.Timestamp])](tag, "ACTIONJOIN") {
  def memberId = column[Long]("ID_MEMBER") // This is the primary key column
  def actionId = column[Long]("ID_ACTION") // This is the primary key column
  def roleAction = column[Int]("ROLE") //0 - Creator - 1 - joined
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
 
  def pk = primaryKey("pk_ACTIONJOIN", (memberId, actionId))
  def foreignKeymemberId = foreignKey("fk_memberIdActionJoin", memberId, controllers.Instantiations.Tables.members)(_.id)
  def foreignKeyactionId = foreignKey("fk_actionIdActionJoin", actionId, controllers.Instantiations.Tables.actions)(_.id)
  
  def * = (memberId, actionId, roleAction, modificationTimestamp, creationTimestamp?)
}

/*
class FollowMemberTable(tag: Tag) extends Table[(Int, Int, Long, java.sql.Timestamp, Option[java.sql.Timestamp])](tag, "FOLLOWMEMBER") {
  def memberFollowingId = column[Int]("ID_FOLLOWING") // This is the primary key column
  def memberFollowedId = column[Int]("ID_FOLLOWED") // This is the primary key column
  def strenghOfLink = column[Long]("ID_FOLLOWED") // This is the strengh of the following (an user follows several people but prefer some ones to others)
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
 
  def pk = primaryKey("pk_FOLLOWUSER", (memberFollowingId, memberFollowedId))

  def * = (memberFollowingId, memberFollowedId, strenghOfLink, modificationTimestamp, creationTimestamp?)
}
*/
case class Action(id: Long, creatorType: Int, creatorID: Long, creatorName: String, name: String, definition: String, nameLocation: String, addressLocation: String, latitude: Double, longitude: Double, timeBegin: Long, timeEnd: Long, numberJoined: Long, language: String, modificationTimestamp: Long) extends EntryClass

class PetitionTable(tag: Tag) extends Table[Types.petitionEntry](tag, "PETITION") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc) // This is the primary key column
  /* 0 : it is a member who created the action - 
   * 1 : it is a group who created the action
   * */
  def creatorType = column[Int]("TYPE_CREATOR") 
  def creatorID = column[Long]("ID_CREATOR") //ID of a creator - group or member
  def name = column[String]("NAME") // The ID, member or group, of the creator
  def definition = column[String]("DEFINITION", O.DBType("MEDIUMTEXT")) // This is the primary key column
  def timeEnd = column[Long]("TIMEEND")
  def numberJoined = column[Long]("NBREJOINED")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def logicalDelete = column[Int]("LOGICALDELETE")
  def language = column[String]("LANGUAGE")
  
  def * = (id?, creatorType, creatorID, name, definition, timeEnd, numberJoined, modificationTimestamp, creationTimestamp?, logicalDelete, language)
}
case class Petition(id: Long, creatorType: Int, creatorID: Long, creatorName: String, name: String, definition: String, timeEnd: Long, numberJoined: Long, language: String, modificationTimestamp: Long) extends EntryClass


class PetitionJoinTable(tag: Tag) extends Table[(Long, Long, Int, java.sql.Timestamp, Option[java.sql.Timestamp])](tag, "PETITIONJOIN") {
  def memberId = column[Long]("ID_MEMBER") // This is the primary key column
  def petitionId = column[Long]("ID_PETITION") // This is the primary key column
  def roleAction = column[Int]("ROLE") //0 - Creator - 1 - joined
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
 
  def pk = primaryKey("pk_PETITIONJOIN", (memberId, petitionId))
  def foreignKeymemberId = foreignKey("fk_memberIdPetitionJoin", memberId, controllers.Instantiations.Tables.members)(_.id)
  def foreignKeyactionId = foreignKey("fk_actionIdPetitionJoin", petitionId, controllers.Instantiations.Tables.petition)(_.id)
  
  def * = (memberId, petitionId, roleAction, modificationTimestamp, creationTimestamp?)
}

/*
 * Tables that stores who follows what
 */

class FollowTable(tag: Tag) extends Table[Types.followEntry](tag, "FOLLOW") {
  def memberId = column[Long]("ID_MEMBER") // This is the primary key column
  def objectFollowedType = column[Int]("TYPE_OBJECTFOLLOWED")
  def objectFollowedID = column[Long]("ID_OBJECTFOLLOWED") 
  def strenghOfLink = column[Long]("STRENGH_OF_LINK")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
 
  def foreignKeymemberId = foreignKey("fk_memberIdFollow", memberId, controllers.Instantiations.Tables.members)(_.id)
  
  def pk = primaryKey("pk_FOLLOW", (memberId, objectFollowedType, objectFollowedID))
  def * = (memberId, objectFollowedType, objectFollowedID, strenghOfLink, modificationTimestamp, creationTimestamp?)
}
case class Follow(memberId: Long, objectFollowedType: Int, objectFollowedID: Long, strenghOfLink: Long, modificationTimestamp: Long)
/*
 * Table to store which user is in which conversation
 */
class ConversationTable(tag: Tag) extends Table[Types.conversation](tag, "CONVERSATION") {
  def id = column[Long]("ID_CONVERSATION", O.PrimaryKey, O.AutoInc) // This is the primary key column
  def conversationString = column[String]("STRING_CONVERSATION") 
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")

  def * = (id?, conversationString, modificationTimestamp, creationTimestamp?)
}

case class Conversation(id: Long, conversationString: String, modificationTimestamp: Long)

/*
 * Table to store which user is in which conversation
 */
class ConversationMembersTable(tag: Tag) extends Table[Types.conversationMemberEntry](tag, "CONVERSATIONMEMBER") {
  def memberId = column[Long]("ID_MEMBER") // This is the primary key column
  def conversationId = column[Long]("ID_CONVERSATION") 
  def newMessages = column[Boolean]("NEWMESSAGES") 
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
 
  def pk = primaryKey("pk_FOLLOWUSER", (memberId, conversationId))
  def foreignKeymemberId = foreignKey("fk_memberIdMemberInConversation", memberId, controllers.Instantiations.Tables.members)(_.id)
  def foreignKeyConversationId = foreignKey("fk_conversationIdMemberInConversation", conversationId, controllers.Instantiations.Tables.conversation)(_.id)
  
  def * = (memberId, conversationId, newMessages, modificationTimestamp, creationTimestamp?)
}

case class ConversationMember(conversationId: Long, conversationString: String, newMessages: Boolean, lastSenderID: Long, lastSenderUsername: String, lastMessage: String, modificationTimestamp: Long)

/*
 * Table to store which user is in which conversation
 */
class ConversationMessagesTable(tag: Tag) extends Table[Types.conversationMessagesEntry](tag, "CONVERSATIONMESSAGES") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc)
  def idConversation = column[Long]("ID_CONVERSATION") // This is the primary key column
  def idSender = column[Long]("IDSENDER") 
  def message = column[String]("MESSAGE_CONVERSATION", O.DBType("TEXT")) 
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")

  def * = (id?, idConversation, idSender, message, modificationTimestamp, creationTimestamp?)
}
case class ConversationMessage(senderId: Long, senderName: String, senderFirstname: String, senderUsername: String, message: String, modificationTimestamp: Long)

/*
 * Table to store which user is in which conversation
 */
class CommentsTable(tag: Tag) extends Table[Types.commentEntry](tag, "COMMENTS") {
  def id = column[Long]("SUP_ID", O.PrimaryKey, O.AutoInc)
  def idSender = column[Long]("IDSENDER")
  def objectCommentedType = column[Long]("TYPE_OBJECTFOLLOWED") 
  def objectCommentedID = column[Long]("ID_OBJECTFOLLOWED") 
  def message = column[String]("MESSAGE_CONVERSATION", O.DBType("TEXT")) 
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def logicalDelete = column[Int]("LOGICALDELETE")
  
  def * = (id?, idSender, objectCommentedType, objectCommentedID, message, modificationTimestamp, creationTimestamp?, logicalDelete)
}
case class Comment(id: Long, senderId: Long, senderName: String, message: String, modificationTimestamp: Long)

/*
 * Table containing the notifications.
 */
class NumberOfViewsTable(tag: Tag) extends Table[(Long, Long, Long)](tag, "NUMBEROFVIEWS") {
  def typeOfObject = column[Long]("TYPE_OBJECT") //The member that received the notification
  def idOfObject = column[Long]("ID_OBJECT") //The activity that the member is notified about
  def numberOfViews = column[Long]("NUMBEROFVIEWS") //0 if the notification has not been seen, 1 if yes
  
  def pk = primaryKey("pk_NUMBEROFVIEWS", (typeOfObject, idOfObject))
  def * = (typeOfObject, idOfObject, numberOfViews)
}

/*
 * Table containing the notifications.
 */
class MembersFriendsMailsTable(tag: Tag) extends Table[Types.memberFriendsMails](tag, "MEMBERFRIENDSMAILS") {
  def id = column[Long]("SUP_ID")
  def friendEmail = column[String]("ID_OBJECT") //The email of the friend
  def sentMail = column[Long]("SENTMAIL") //The email of the friend
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def pk = primaryKey("pk_MEMBERSFRIENDS", (id, friendEmail))
  def * = (id, friendEmail, sentMail, modificationTimestamp, creationTimestamp?)
}
case class MembersFriendsMails(friendMail: String, modificationTimestamp: Long, optionalUserId: Long, optionalUsername: String)

/*
 * Table containing the notifications.
 */
class MembersPagesLoadedTable(tag: Tag) extends Table[(Long, String, String, Option[java.sql.Timestamp])](tag, "MEMBERSPAGESLOADED") {
  def id_member = column[Long]("ID_MEMBER")
  def ip_member = column[String]("IP_MEMBER") 
  def page_url = column[String]("ID_OBJECT") 
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def * = (id_member, ip_member, page_url, creationTimestamp?)
}

/*
class RedirectStatsTable(tag: Tag) extends Table[(Int, Long, String, Long, java.sql.Timestamp, Option[java.sql.Timestamp])](tag, "REDIRECTSTATS") {
  def objectType = column[Int]("TYPE")
  def objectID = column[Long]("ID")
  def urlFrom = column[String]("URLFROM")
  def numberOfTimesAccessed = column[Long]("NUMBER")
  def modificationTimestamp = column[java.sql.Timestamp]("MODIFICATION_TIMESTAMP")
  def creationTimestamp = column[java.sql.Timestamp]("CREATION_TIMESTAMP")
  
  def pk = primaryKey("pk_FOLLOWUSER", (objectType, objectID, urlFrom))
  
  def * = (objectType, objectID, urlFrom, numberOfTimesAccessed, modificationTimestamp, creationTimestamp?)
}*/
