package controllers


import models.ChatRoom
import play.api.libs.json.JsValue
import play.api.mvc.{Action, Controller, WebSocket}
import play.api.libs.concurrent.Execution.Implicits._


object ChatController extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.chat())
  }

  def ws(username: String): WebSocket[JsValue, JsValue] = WebSocket.tryAccept[JsValue] { request  =>
    ChatRoom.join(username).map{ io =>
      Right(io)
    }.recover{ case e => Left(Ok(e.toString))}
  }
}
