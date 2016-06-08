package rapture.mail

import rapture.core._
import rapture.io._
import rapture.net._
import rapture.uri._
import rapture.html._
import rapture.mime._

import scala.reflect._
import scala.reflect.macros._
import scala.language.experimental.macros

import java.net.URL

object Macros {

  def mailtoMacro(c: Context)(constants: c.Expr[List[String]])(variables: c.Expr[List[String]]): c.Expr[MailtoUri] = {
    import c.universe._
    
    reify {
      new MailtoUri(constants.splice.zip(variables.splice :+ "").map { case (a, b) => a+b}.mkString)
    }
  }
}


trait `Smtp#send` extends MethodConstraint
trait `Smtp#sendTo` extends MethodConstraint

object Mailto {
  private val Regex = """^mailto:([_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,}))$""".r

  def parse(s: String): MailtoUri = s match {
    case Regex(address, _, _, _) => new MailtoUri(address)
  }
}

case class MailtoUri(email: String) {
  override def toString = s"mailto:$email"
}

case class HtmlEmail(html: HtmlDoc, plainText: String, inlines: List[Inline])

case class Inline(name: String, content: HttpUrl)
case class Attachment(name: String, content: HttpUrl)

object `package` {


  implicit class MailEnrichedUriContext(uri: UriContext.type) {
    def mailto(constants: List[String])(variables: List[String]) = macro Macros.mailtoMacro
    def cid(constants: List[String])(variables: List[String]) = PathLink(s"cid:${constants.mkString}")
  }

  case class Recipient(email: String, name: String = "") {
    override def toString = if(name == "") email else s""""${name}" <${email}>"""
  }

  object Smtp {
    private val Regex = """^smtp:\/\/([^:]*)(:([1-9][0-9]*))?\/?$""".r

    def parse(s: String) = s match {
      case Regex(host, _, port) => new Smtp(host, Option(port).map(_.toInt).getOrElse(25))
    }
  }

  trait Mailable[T] {
    def content(t: T): String
    def htmlContent(t: T): Option[(String, List[Inline])]
    def attachments(t: T) = Nil
  }

  implicit object StringMailable extends Mailable[String] {
    def content(t: String) = t
    def htmlContent(t: String) = None
  }

  implicit object HtmlMailable extends Mailable[HtmlEmail] {
    def content(t: HtmlEmail) = t.plainText
    def htmlContent(t: HtmlEmail) = Some((t.html.format, t.inlines))
  }

  trait AddressedMailable[T] extends Mailable[T] {
    def sender(t: T): Recipient
    def recipients(t: T): List[Recipient]
    def ccRecipients(t: T): List[Recipient] = Nil
    def subject(t: T): String
  }

  case class Smtp(hostname: String, port: Int = 25) {

    def sendTo[Mail: Mailable](sender: Recipient, recipients: Seq[Recipient],
        ccRecipients: Seq[Recipient] = Nil, subject: String, mail: Mail)(implicit mode: Mode[`Smtp#sendTo`]): Unit = {
      sendmail(sender.toString, recipients.map(_.toString), ccRecipients.map(_.toString), subject,
          ?[Mailable[Mail]].content(mail), ?[Mailable[Mail]].htmlContent(mail), ?[Mailable[Mail]].attachments(mail))
    }

    def send[Mail: AddressedMailable](mail: Mail)(implicit mode: Mode[`Smtp#send`]) = {
      val am = ?[AddressedMailable[Mail]]
      sendmail(am.sender(mail).toString, am.recipients(mail).map(_.toString),
          am.ccRecipients(mail).map(_.toString), am.subject(mail), am.content(mail), None, Nil)
    }

    def sendmail(from: String, to: Seq[String], cc: Seq[String], subject: String,
        bodyText: String, bodyHtml: Option[(String, Seq[Inline])],
        attachments: Seq[Attachment])(implicit mode: Mode[_]):
        mode.Wrap[Unit, Exception] = mode wrap {
    
      import javax.mail._
      import javax.mail.internet._
      import javax.activation._
      
      val props = System.getProperties()
      props.put("mail.smtp.host", hostname)
      props.put("mail.smtp.port", port.toString)
      val session = Session.getDefaultInstance(props, null)
      val msg = new MimeMessage(session)
      msg.setFrom(new InternetAddress(from))
      for(r <- to) msg.addRecipient(Message.RecipientType.TO, new InternetAddress(r))
      for(r <- cc) msg.addRecipient(Message.RecipientType.CC, new InternetAddress(r))
      msg.setSubject(subject)

      bodyHtml match {
        case Some((html, inlines)) => {
          var top = new MimeMultipart("alternative")
          val textPart = new MimeBodyPart()
          textPart.setText(bodyText, "UTF-8")
          top.addBodyPart(textPart)

          val htmlPart = new MimeBodyPart()
          htmlPart.setContent(html, "text/html;charset=UTF-8")
          top.addBodyPart(htmlPart)

          if(inlines.length > 0) {
            val body = new MimeBodyPart()
            body.setContent(top)
            top = new MimeMultipart("related")
            top.addBodyPart(body)
            inlines.foreach { case Inline(name, content) =>
              val relPart = new MimeBodyPart()
              relPart.setDisposition(Part.INLINE)
              relPart.setHeader("Content-ID", "<"+name+">")
              val ds = new URLDataSource(new URL(content.link.toString))
              //ds.setFileTypeMap(MimeTypes.mimeTypesMap)
              relPart.setDataHandler(new DataHandler(ds))
              top.addBodyPart(relPart)
            }
          }

          /*if(attachments.length > 0) {
            val body = new MimeBodyPart()
            body.setContent(top)
            top = new MimeMultipart("mixed")
            top.addBodyPart(body)
            for(a <- attachments) {
              val attPart = new MimeBodyPart()
              attPart.setDisposition(Part.ATTACHMENT)
              attPart.setFileName(a._1)
              val src = new FileDataSource(a._3) {
                override def getContentType() = a._2
              }
              attPart.setDataHandler(new DataHandler(src))
              top.addBodyPart(attPart)
            }
          }*/
          msg.setContent(top)
        }
        case None => {
          if(attachments.length > 0) {
            /*val body = new MimeBodyPart()
            body.setText(bodyText, "UTF-8")
            val top = new MimeMultipart("mixed")
            top.addBodyPart(body)
            for(a <- attachments) {
              val attPart = new MimeBodyPart()
              attPart.setDisposition(Part.ATTACHMENT)
              attPart.setFileName(a._1)
              val src = new FileDataSource(a._3) {
                override def getContentType() = a._2
              }
              attPart.setDataHandler(new DataHandler(src))
              top.addBodyPart(attPart)
            }
            msg.setContent(top)*/
          } else {
            msg.setText(bodyText, "UTF-8")
          }
        }
      }
      Transport.send(msg)
    }
  }
}


