/*
  Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.

  The primary distribution site is
  
    http://rapture.io/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  
    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is
  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and limitations under the License.
 */

package rapture.mail

import rapture.base._
import rapture.core._
import rapture.fs._
import rapture.net._
import rapture.uri._
import rapture.html._
import rapture.mime._
import rapture.codec._

import scala.reflect._
import scala.language.experimental.macros

import java.net.URL

object Macros {

  def mailtoMacro(c: BlackboxContext)(constants: c.Expr[List[String]])(
      variables: c.Expr[List[String]]): c.Expr[MailtoUri] = {
    import c.universe._

    reify {
      new MailtoUri(constants.splice.zip(variables.splice :+ "").map { case (a, b) => a + b }.mkString)
    }
  }
  
  def smtpMacro(c: BlackboxContext)(constants: c.Expr[List[String]])(
      variables: c.Expr[List[String]]): c.Expr[Smtp] = {
    import c.universe._
    import compatibility._
    
    val start = constants.tree match {
      case Apply(_, constants) =>
        val Literal(Constant(start: String)) = constants.head
        start
    }
    if(!start.startsWith("//")) c.abort(c.enclosingPosition, "this is not a valid SMTP URI")
    val rest = start.substring(2)

    val newName = termName(c, freshName(c)("ctx"))

    c.Expr[Smtp](q"""{
      val $newName = List($constants, $variables :+ "").transpose.flatten.mkString.substring(2).split(":")
      _root_.rapture.mail.Smtp($newName(0), new _root_.rapture.core.EnrichedString(if($newName.length > 1) $newName(1) else "25").as[
          _root_.scala.Int](_root_.rapture.core.StringParser.intParser,
          _root_.rapture.core.modes.returnOption()).getOrElse(25))
    }""")
  }
}

trait `Smtp#send` extends MethodConstraint
trait `Smtp#sendmail` extends MethodConstraint
trait `send` extends MethodConstraint
trait `sendTo` extends MethodConstraint

object Mailto {
  private val Regex =
    """^mailto:([_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,}))$""".r

  def parse(s: String): MailtoUri = s match {
    case Regex(address, _, _, _) => new MailtoUri(address)
  }
}

object MailtoUri {
  implicit val mailtoUriLinkable: UriCapable[MailtoUri] = new UriCapable[MailtoUri] {
    def uri(mu: MailtoUri): Uri =
      Uri("mailto", mu.email)
  }
}

case class MailtoUri(email: String) {
  override def toString = s"mailto:$email"
}

case class HtmlEmail(html: HtmlDoc, inlines: List[Inline], attachments: List[Attachment])

case class AddressedHtmlEmail(email: HtmlEmail, from: Recipient, subject: String, to: Seq[Recipient], cc: Seq[Recipient], bcc: Seq[Recipient])

case class Inline(name: String, content: HttpUrl)

sealed trait Attachment { def name: String }
case class UrlAttachment(name: String, content: HttpUrl) extends Attachment
case class FileAttachment(name: String, content: FsUrl) extends Attachment
case class DataAttachment(name: String, content: Bytes) extends Attachment

case class Recipient(email: String, name: String = "") {
  override def toString = if (name == "") email else s""""${name}" <${email}>"""
}

object Smtp {
  private val Regex = """^smtp:\/\/([^:]*)(:([1-9][0-9]*))?\/?$""".r

  def parse(s: String) = s match {
    case Regex(host, _, port) => new Smtp(host, Option(port).map(_.toInt).getOrElse(25))
  }
}

case class MailEnrichedUriContext(uri: UriContext.type) {
  def mailto(constants: List[String])(variables: List[String]): MailtoUri = macro Macros.mailtoMacro
  def smtp(constants: List[String])(variables: List[String]): Smtp = macro Macros.smtpMacro
  def cid(constants: List[String])(variables: List[String]): PathLink = PathLink(s"cid:${List(constants, variables :+ "").transpose.flatten.mkString}")
}

object Mailable {
  implicit val stringMailable: Mailable[String] = new Mailable[String] {
    def content(t: String) = t
    def htmlContent(t: String) = None
    def attachments(t: String): Seq[Attachment] = Nil
  }

  implicit def htmlMailable(implicit conv: HtmlToPlainTextConverter): Mailable[HtmlEmail] = new Mailable[HtmlEmail] {
    def content(t: HtmlEmail) = conv.convert(t.html)
    def htmlContent(t: HtmlEmail) = Some((t.html.format, t.inlines))
    def attachments(t: HtmlEmail) = t.attachments
  }

}

trait Mailable[T] {
  def content(t: T): String
  def htmlContent(t: T): Option[(String, List[Inline])]
  def attachments(t: T): Seq[Attachment]
}

object `package` {

  implicit def mailEnrichedUriContext(uri: UriContext.type): MailEnrichedUriContext = MailEnrichedUriContext(uri)

  implicit class SendToCapability[T](msg: T) {
    def sendTo(to: Seq[Recipient], from: Recipient, subject: String, cc: Seq[Recipient] = Seq(), bcc: Seq[Recipient] = Seq())
        (implicit mailable: Mailable[T], smtpServer: Smtp, mode: Mode[`sendTo`]):
        mode.Wrap[SendReport, SendAddressException with SendException] = mode.wrap {
      smtpServer.sendmail(to, from, subject, cc, bcc, msg)
    }
  }

  implicit class SendCapability[T](msg: T) {
    def send()(implicit smtpServer: Smtp, sendable: Sendable[T], mode: Mode[`send`]): mode.Wrap[SendReport, SendAddressException with SendException] =
      smtpServer.sendmail(sendable.to(msg), sendable.from(msg), sendable.subject(msg), sendable.cc(msg), sendable.bcc(msg), msg)(sendable.mailable(msg), mode.generic)
  }
}

case class Smtp(hostname: String, port: Int = 25) {

  def sendmail[Mail: Mailable](to: Seq[Recipient],
                             from: Recipient,
                             subject: String,
                             cc: Seq[Recipient],
                             bcc: Seq[Recipient],
                             mail: Mail)(implicit mode: Mode[`Smtp#sendmail`]): mode.Wrap[SendReport, SendAddressException with SendException] =
    doSendmail(to.map(_.toString),
             from.toString,
             subject,
             cc.map(_.toString),
             bcc.map(_.toString),
             ?[Mailable[Mail]].content(mail),
             ?[Mailable[Mail]].htmlContent(mail),
             ?[Mailable[Mail]].attachments(mail))

  def doSendmail(to: Seq[String],
               from: String,
               subject: String,
               cc: Seq[String],
               bcc: Seq[String],
               bodyText: String,
               bodyHtml: Option[(String, Seq[Inline])],
               attachments: Seq[Attachment])(implicit mode: Mode[_]): mode.Wrap[SendReport, SendAddressException with SendException] = mode wrap {

    import javax.mail._
    import javax.mail.internet._
    import javax.activation._

    val props = System.getProperties()
    props.put("mail.smtp.host", hostname)
    props.put("mail.smtp.port", port.toString)
    val session = Session.getDefaultInstance(props, null)
    val msg = new MimeMessage(session)
    msg.setFrom(new InternetAddress(from))
    for (r <- to) msg.addRecipient(Message.RecipientType.TO, new InternetAddress(r))
    for (r <- cc) msg.addRecipient(Message.RecipientType.CC, new InternetAddress(r))
    msg.setSubject(subject)

    def source(a: Attachment) = a match {
      case FileAttachment(name, file) =>
        val parts = name.split("\\.").to[List]
        val extension = if (parts.length < 2) Nil else List(parts.last)
        val mime = extension.flatMap(MimeTypes.extension).headOption.getOrElse(MimeTypes.`text/plain`)
        new FileDataSource(file.javaFile) {
          override def getContentType() = mime.name
        }
      case DataAttachment(name, data) =>
        val parts = name.split("\\.").to[List]
        val extension = if (parts.length < 2) Nil else List(parts.last)
        val mime = extension.flatMap(MimeTypes.extension).headOption.getOrElse(MimeTypes.`text/plain`)
        new DataSource {
          override def getContentType() = mime.name
          def getOutputStream() = throw new java.io.IOException()
          def getInputStream() = new java.io.ByteArrayInputStream(data.bytes)
          def getName = name
        }
      case UrlAttachment(name, url) =>
        new URLDataSource(new URL(url.link.toString))
    }

    bodyHtml match {
      case Some((html, inlines)) => {
        var top = new MimeMultipart("alternative")
        val textPart = new MimeBodyPart()
        textPart.setText(bodyText, "UTF-8")
        top.addBodyPart(textPart)

        val htmlPart = new MimeBodyPart()
        htmlPart.setContent(html, "text/html;charset=UTF-8")
        top.addBodyPart(htmlPart)

        if (inlines.length > 0) {
          val body = new MimeBodyPart()
          body.setContent(top)
          top = new MimeMultipart("related")
          top.addBodyPart(body)
          inlines.foreach {
            case Inline(name, content) =>
              val relPart = new MimeBodyPart()
              relPart.setDisposition(Part.INLINE)
              relPart.setHeader("Content-ID", "<" + name + ">")
              val ds = new URLDataSource(new URL(content.link.toString))
              //ds.setFileTypeMap(MimeTypes.mimeTypesMap)
              relPart.setDataHandler(new DataHandler(ds))
              top.addBodyPart(relPart)
          }
        }

        if (attachments.length > 0) {
          val body = new MimeBodyPart()
          body.setContent(top)
          top = new MimeMultipart("mixed")
          top.addBodyPart(body)
          for (a <- attachments) {
            val attPart = new MimeBodyPart()
            attPart.setDisposition(Part.ATTACHMENT)
            attPart.setFileName(a.name)
            attPart.setDataHandler(new DataHandler(source(a)))
            top.addBodyPart(attPart)
          }
        }
        msg.setContent(top)
      }
      case None => {
        if (attachments.length > 0) {
          val body = new MimeBodyPart()
          body.setText(bodyText, "UTF-8")
          val top = new MimeMultipart("mixed")
          top.addBodyPart(body)
          for (a <- attachments) {
            val attPart = new MimeBodyPart()
            attPart.setDisposition(Part.ATTACHMENT)
            attPart.setFileName(a.name)
            attPart.setDataHandler(new DataHandler(source(a)))
            top.addBodyPart(attPart)
          }
          msg.setContent(top)
        } else {
          msg.setText(bodyText, "UTF-8")
        }
      }
    }
    Transport.send(msg)
    SendReport()
  }
}

case class SendReport()

object Sendable {
  implicit def addressedHtmlEmailSendable(implicit m: Mailable[HtmlEmail]): Sendable[AddressedHtmlEmail] = new Sendable[AddressedHtmlEmail] {
    def to(t: AddressedHtmlEmail): Seq[Recipient] = t.to
    def cc(t: AddressedHtmlEmail): Seq[Recipient] = t.cc
    def bcc(t: AddressedHtmlEmail): Seq[Recipient] = t.bcc
    def from(t: AddressedHtmlEmail): Recipient = t.from
    def subject(t: AddressedHtmlEmail): String = t.subject
    def mailable(t: AddressedHtmlEmail): Mailable[AddressedHtmlEmail] = new Mailable[AddressedHtmlEmail] {
      def content(t: AddressedHtmlEmail): String = m.content(t.email)
      def htmlContent(t: AddressedHtmlEmail): Option[(String, List[Inline])] = m.htmlContent(t.email)
      def attachments(t: AddressedHtmlEmail): Seq[Attachment] = m.attachments(t.email)
    }
  }
}

trait Sendable[T] {
  def to(t: T): Seq[Recipient]
  def cc(t: T): Seq[Recipient]
  def bcc(t: T): Seq[Recipient]
  def from(t: T): Recipient
  def subject(t: T): String
  def mailable(t: T): Mailable[T]
}

case class SendAddressException(invalid: Set[Recipient], validSent: Set[Recipient],
    validUnsent: Set[Recipient]) extends RuntimeException("the email could not be sent to all addresses")

case class SendException() extends RuntimeException("the email could not be sent")

