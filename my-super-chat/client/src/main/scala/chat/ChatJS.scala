package chat

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import js.Dynamic.{global => g}
import org.scalajs.dom
import org.scalajs.dom.html.Div

import scalatags.JsDom._
import all._
import org.scalajs.jquery.{JQuery, jQuery => $}

object ChatJS extends js.JSApp {

  val maxMessages = 20

  var assetsDir: String = ""
  var wsBaseUrl: String = ""

  var client: Option[ChatClient] = None

  def signInPanel: TypedTag[Div] = div(id:="signInPanel"){
    form(`class`:="form-inline", "role".attr:="form")(
      div(id:="usernameForm", `class`:="form-group")(
        div(`class`:="input-group")(
          div(`class`:="input-group-addon", raw("&#9786;")),
          input(id:="username", `class`:="form-control", `type`:="text", placeholder:="Enter username")
        )
      ),
      span(style:="margin:0px 5px"),
      button(`class`:="btn btn-default", onclick:={ () =>
        val input = $("#username").value().toString.trim
        if (input == "") {
          $("#usernameForm").addClass("has-error")
          dom.alert("Invalid username")
        } else {
          $("#usernameForm").removeClass("has-error")
          $("#title").addClass("hide")
          client = ChatClient.connect(wsBaseUrl, input).map{ c =>
            $("#loginAs").text(s"Login as: ${c.username}")
            $("#username").value("")
            $("#signInPanel").addClass("hide")
            $("#chatPanel").removeClass("hide")
            c
          }
        }
        false
      })("Sign in")
    )
  }

  def chatPanel: TypedTag[Div] = div(id:="chatPanel", `class`:="hide")(
    div(`class`:="row", style:="margin-bottom: 10px;")(
      div(`class`:="col-md-12", style:="text-align: right;")(
        span(id:="loginAs", style := "padding: 0px 10px;"),
        button(`class`:="btn btn-default", onclick:={ () =>
          singOut
        }, "Sign out")
      )
    ),
    div(`class` := "panel panel-default")(
      div(`class` := "panel-heading")(
        h3(`class` := "panel-title")("Chat Room")
      ),
      div(`class` := "panel-body")(
        div(id := "messages")
      ),
      div(`class` := "panel-footer")(
        textarea(id:="message", `class` := "form-control message", placeholder := "Say something")
      )
    )
  )

  def createMessage(msg: String, username: String, avatar: String): TypedTag[Div] = {
    div(`class`:=s"row message-box${if(username == client.map(_.username).getOrElse(""))"-me" else ""}")(
      div(`class`:="col-md-2")(
        div(`class`:="message-icon")(
          img(src:=s"$assetsDir/images/avatars/$avatar", `class`:="img-rounded"),
          div(username)
        )
      ),
      div(`class`:="col-md-10")(raw(msg))
    )
  }

  def singOut = {
    client.foreach(_.close())
    $("#title").removeClass("hide")
    $("#signInPanel").removeClass("hide")
    $("#chatPanel").addClass("hide")
    $("#messages").html("")
  }

  trait ChatClient {

    val username: String

    def send(msg: String)

    def close()
  }

  object ChatClient {

    def connect(url: String, username: String): Option[ChatClient] = {
      try {
        if (g.window.WebSocket.toString != "undefined") {
          Some(new WSChatClient(url, username))
        } else None
      } catch {
        case e: Throwable =>
          dom.alert("Unable to connect: " + e)
          None
      }
    }

    def receive(e: dom.MessageEvent): Any = {
      val msgElem = dom.document.getElementById("messages")
      val data = js.JSON.parse(e.data.toString)
      if (data.error.toString != "undefined") {
        dom.alert(data.error.toString)
        singOut
      } else {
        val user = data.user.name.toString
        val avatar = data.user.avatar.toString
        val message = data.message.toString
        msgElem.appendChild(createMessage(message, user, avatar).render)
        if (msgElem.childNodes.length >= maxMessages)
          msgElem.removeChild(msgElem.firstChild)
        msgElem.scrollTop = msgElem.scrollHeight
      }
    }
  }

  class WSChatClient(url: String, val username: String) extends ChatClient {
    val socket = new dom.WebSocket(url + username)
    socket.onmessage = ChatClient.receive _

    override def send(msg: String): Unit = {
      val json = js.JSON.stringify(js.Dynamic.literal(text=$("#message").value()))
      socket.send(json)
    }

    override def close(): Unit = socket.close()

  }

  def ready: JQuery = {
    $("#message").keypress((e: dom.KeyboardEvent) => {
      if(!e.shiftKey && e.keyCode == 13) {
        e.preventDefault()
        client.foreach{_.send($("#message").value().toString)}
        $("#message").value("")
      }
    })
  }

  @JSExport
  override def main(): Unit = {}

  @JSExport
  def show(settings: js.Dynamic): JQuery = {
    assetsDir = settings.assetsDir.toString
    wsBaseUrl = settings.wsBaseUrl.toString

    val content = dom.document.getElementById("content")
    content.appendChild(signInPanel.render)
    content.appendChild(chatPanel.render)
    ready
  }
}