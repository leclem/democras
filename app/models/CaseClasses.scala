package models.formsClasses
//Case classes for forms
//Users
case class userCredentials(username: String, password: String)
case class usersubscriptionFb(firstname: String, name: String, gender: String, birthday: String, city: String, idfb: String, address: String,  location: String, lang: String, mail: String, username: String, password: String, link: String, presentation: String, pictureu: String, picturex: String, picturey: String, picturem: String, picturemx: String, picturemy: String, picturemm: String)
case class userSubcriptionLogin(username: String, password: String, mail: String, lang: String)
case class userSubcription(firstname: String, name: String, gender: String, birthday: String, city: String, address: String, link: String, presentation: String, location: String/*, lang: Seq[String]*/)
case class userSession(username: String, id: String)
case class userInfo(firstname: String, name: String, mail: String, ville: String, presentation: String)
case class userPasswordForget(username: String)
case class userPasswordReset(hash: String, newPassword: String)
case class userLanguage(language: String)

//Concepts
case class createConcept(name: String, definition: String, language: String)

//Actions
case class createAction(idGroup: String, name: String, definition: String, namelocation: String, addresslocation: String, positionX: String, positionY: String, timeBegin: String, timeEnd: String, language: String)

//Groups
case class createGroup(name: String, definition: String, privacy: String)

//Debates
case class createDebate(question: String, debateType: String, reservedToGroup: String, language: String)
case class yesOrNoDebate(comment: String, opinion: String)
case class opinionDebate(comment: String) 

//Petitions
case class createPetition(idGroup: String, name: String, definition: String, timeEnd: String, language: String)

//Conversations
case class createConversation(receivers: Seq[String], message: String)
case class sendConversation(id: String, message: String)

//Comments
case class createComment(objecType: String, objectID: String, message: String)
case class commentModification(id: String, message: String)

//Membersmails
case class membersMails(mails: Seq[String])