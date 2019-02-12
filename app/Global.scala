import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.Logger
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.i18n._
import play.api.i18n.I18nSupport

object Global extends GlobalSettings {
    override def onHandlerNotFound(request: RequestHeader) = {
    /*request.session if you want to get the session */
    implicit val messages = controllers.Instantiations.getLang(request)
    Future.successful(request.session.get("username").map { username =>
      NotFound(views.html.notfound(Messages("page_not_found"))(controllers.Instantiations.getLang(request)))
      }.getOrElse {
      NotFound(views.html.notfound(Messages("page_not_found"))(controllers.Instantiations.getLang(request)))
    })
  }
}
