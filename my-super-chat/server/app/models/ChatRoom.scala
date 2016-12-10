package models

import java.io.{File, FileFilter}

import akka.actor._
import com.google.common.html.HtmlEscapers
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps

import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._

import akka.util.Timeout
import akka.pattern.ask

import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.{Logger=>log}
import scala.util.Random

trait ChatUserStore {

  def get(username: String): Option[User]

  def list(): List[User]

  def save(user: User): Boolean

  def remove(username: String): Boolean

}

object ChatUserMemStore extends ChatUserStore {

  import scala.collection.mutable.{Map => MutableMap}

  val users = MutableMap.empty[String, String]

  override def get(username: String): Option[User] =
    users.get(username).map{User(username, _)}

  override def list(): List[User] =
    users.toList.map{e => User(e._1, e._2)}

  override def save(user: User): Boolean =
    users.put(user.username, user.avatar).exists { s => true }

  override def remove(username: String): Boolean =
    users.remove(username).exists { s => true }
}

class ChatRoom extends Actor {

  val (chatEnumerator, chatChannel) = Concurrent.broadcast[JsValue]

  def receive: PartialFunction[Any, Unit] = {
    case Join(username) =>
      ChatRoom.store.get(username).map{ user =>
        sender ! CannotConnect("This username is already used")
      }.getOrElse{
        val user = User(username, ChatRoom.randomAvatar())
        ChatRoom.store.save(user)
        sender ! Connected(user, chatEnumerator)
        self ! NotifyJoin(user)
      }

    case NotifyJoin(user) =>
      notifyAll("join", user, "has entered the room")

    case Talk(user, text) =>
      println(user.username + " is talking: " + text)
      notifyAll("talk", user, text)

    case Quit(user) =>
      ChatRoom.store.remove(user.username)
      notifyAll("quit", user, "has left the room")
  }

  def notifyAll(kind: String, user: User, text: String) {
    def userToJson(user: User) = Json.obj("name" -> user.username, "avatar" -> user.avatar)
    val msg = JsObject(
      Seq(
        "kind" -> JsString(kind),
        "user" -> userToJson(user),
        "message" -> JsString(text),
        "members" -> JsArray(ChatRoom.store.list().map(userToJson))
      )
    )
    chatChannel.push(msg)
  }
}


object ChatRoom {

  implicit val timeout = Timeout(1 second)

  val store = ChatUserMemStore

  val rand = new Random()

  val fileFilter = new FileFilter {
    override def accept(file: File): Boolean = file.getName.endsWith(".png")
  }

  val avatars = play.Play.application().getFile("/public/images/avatars").listFiles(fileFilter).map{f => f.getName}

  lazy val default = Akka.system.actorOf(Props[ChatRoom])

  def randomAvatar(): String =
    avatars(rand.nextInt(avatars.length))

  def join(username: String): Future[(Iteratee[JsValue, _], Enumerator[JsValue])] = {
    (default ? Join(username)).map{
      case Connected(user, enumerator) =>
        val iteratee = Iteratee.foreach[JsValue] { event =>
          talk(user, (event \ "text").as[String])
        }.map { _ =>
          default ! Quit(user)
        }
        (iteratee, enumerator)

      case CannotConnect(error) =>
        val iteratee = Done[JsValue,Unit]((),Input.EOF)
        val enumerator =  Enumerator[JsValue](JsObject(Seq("error" -> JsString(error))))
          .andThen(Enumerator.enumInput(Input.EOF))
        (iteratee, enumerator)
    }

  }

  def talk(user: User, msg: String): Unit =
    default ! Talk(user, HtmlEscapers.htmlEscaper().escape(msg).replace("\n", "<br/>"))

  def talk(username: String, msg: String): Unit =
    ChatRoom.store.get(username).map{ user =>
      talk(user, msg)
    }.getOrElse{log.warn(s"Username: $username not found")}
}

case class User(username: String, avatar: String)

case class Join(username: String)
case class Quit(user: User)
case class Talk(user: User, text: String)
case class NotifyJoin(user: User)

case class Connected(user: User, enumerator:Enumerator[JsValue])
case class CannotConnect(msg: String)
