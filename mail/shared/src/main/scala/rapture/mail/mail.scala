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
import rapture.io._
import rapture.uri._
import rapture.html._
import rapture.mime._
import rapture.codec._

import scala.reflect._
import scala.language.experimental.macros

object Macros {

  def mailtoMacro(c: BlackboxContext)(constants: c.Expr[List[String]])(
      variables: c.Expr[List[String]]): c.Expr[MailtoUri] = {
    import c.universe._

    c.Expr(q"""
      new _root_.rapture.mail.MailtoUri(_root_.scala.List($constants,
          $variables :+ "").transpose.flatten.mkString)
    """)
  }

  def contactMacro(c: BlackboxContext)(variables: c.Expr[String]*): c.Expr[Contact] = {
    import c.universe._

    val literals = c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) => rawParts.map {
        case Literal(Constant(s: String)) => s
      }
    }

    val mock = literals.mkString("x")

    val result = if(Contact.parse(mock).isDefined) c.Expr(q"""
      _root_.rapture.mail.Contact.parse(_root_.scala.List($literals,
          _root_.scala.List(..$variables, "")).transpose.flatten.mkString).get
    """) else c.abort(c.enclosingPosition, "this is not a valid email address")


    result
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

case class HtmlEmail(html: HtmlDoc, inlines: List[Annex[Attachable]], attachments: List[Annex[Attachable]])

object Attachable {
  implicit def attachable[Res: HasResourceName: HasContentType: Reader.ForBytes] = new Attachable[Res] {
    def resourceName(res: Res): String = res.resourceName
    def contentType(res: Res): MimeTypes.MimeType = res.contentType
    def bytes(res: Res): Bytes = res.slurp[Byte]
  }
}

trait Attachable[Res] {
  def resourceName(res: Res): String
  def contentType(res: Res): MimeTypes.MimeType
  def bytes(res: Res): Bytes
}

case class Envelope(
  subject: String,
  from: Contact,
  to: SeqParameter[Contact],
  cc: SeqParameter[Contact] = Nil,
  bcc: SeqParameter[Contact] = Nil) {

  def insert(mailable: Annex[Mailable], attachments: Annex[Attachable]*) =
    EmailMessage(from, to.elements, cc.elements, bcc.elements, subject, mailable, attachments)
}

object EmailMessage {
  implicit val emailMessageSendable: Sendable[EmailMessage] = new Sendable[EmailMessage] {
    def to(env: EmailMessage): Seq[Contact] = env.to
    def cc(env: EmailMessage): Seq[Contact] = env.cc
    def bcc(env: EmailMessage): Seq[Contact] = env.bcc
    def from(env: EmailMessage): Contact = env.from
    def subject(env: EmailMessage): String = env.subject
    def content(env: EmailMessage): String = env.mailable(_.content)
    def htmlContent(env: EmailMessage): Option[(String, Seq[Annex[Attachable]])] = env.mailable(_.htmlContent)
    def attachments(env: EmailMessage): Seq[Annex[Attachable]] = env.mailable(_.attachments)
  }
}

case class EmailMessage(from: Contact, to: Seq[Contact], cc: Seq[Contact], bcc: Seq[Contact], subject: String, mailable: Annex[Mailable], attachments: Seq[Annex[Attachable]]*)

class ContactStringContext(sc: StringContext) {
  def contact(variables: String*): Contact = macro Macros.contactMacro
}
object Contact {
  private val EmailWithName =
    """^([^<]*) <([0-9a-zA-Z]([-.\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\w]*[0-9a-zA-Z]\.)+[a-zA-Z]{2,})>$""".r
  
  private val JustEmail =
    """^([0-9a-zA-Z]([-.\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\w]*[0-9a-zA-Z]\.)+[a-zA-Z]{2,})$""".r

  def parse(email: String): Option[Contact] = email match {
    case JustEmail(e, _, _) => Some(Contact(e))
    case EmailWithName(n, e, _, _0) => Some(Contact(e, Some(n)))
    case _ => None
  }
}

case class Contact(email: String, name: Option[String] = None) {
  override def toString = if (name == None) email else s""""${name.get}" <${email}>"""
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
    def attachments(t: String): Seq[Annex[Attachable]] = Nil
  }

  implicit def htmlMailable(implicit conv: HtmlToPlainTextConverter): Mailable[HtmlEmail] =
    new Mailable[HtmlEmail] {
      def content(t: HtmlEmail) = conv.convert(t.html)
      def htmlContent(t: HtmlEmail) = Some((t.html.format, t.inlines))
      def attachments(t: HtmlEmail) = t.attachments
    }

}

object Attachment {
  implicit def attachmentIsAttachable: Attachable[Attachment] = new Attachable[Attachment] {
    def resourceName(attachment: Attachment): String = attachment.name
    
    def contentType(attachment: Attachment): MimeTypes.MimeType =
      attachment.contentType().orElse(MimeTypes.extension(attachment.name).headOption).getOrElse(
          MimeTypes.`application/octet-stream`)
    
    def bytes(attachment: Attachment): Bytes =
      attachment.content { implicit reader => _.slurp[Byte] }
  }
}

case class Attachment(name: String, content: Annex[Reader.ForBytes],
    contentType: OptionalParameter[MimeTypes.MimeType] = UnspecifiedParameter)

trait Mailable[T] {
  def content(t: T): String
  def htmlContent(t: T): Option[(String, Seq[Annex[Attachable]])]
  def attachments(t: T): Seq[Annex[Attachable]]
}

object `package` {
  implicit def mailEnrichedUriContext(uri: UriContext.type): MailEnrichedUriContext =
    MailEnrichedUriContext(uri)
  
  implicit def mailEnrichedStringContext(sc: StringContext): ContactStringContext =
    new ContactStringContext(sc)
  
  implicit def sendExtensionMethod[T: Sendable](sendable: T): Sendable.Capability[T] =
    Sendable.Capability[T](sendable)
}

case class Smtp(hostname: String, port: Int = 25) {

  def sendmail(to: Seq[Contact],
                             from: Contact,
                             subject: String,
                             cc: Seq[Contact],
                             bcc: Seq[Contact],
                             content: String,
                             htmlContent: Option[(String, Seq[Annex[Attachable]])],
                             attachments: Seq[Annex[Attachable]])(implicit mode: Mode[`Smtp#sendmail`]): mode.Wrap[SendReport, SendAddressException with SendException] =
    doSendmail(to.map(_.toString),
             from.toString,
             subject,
             cc.map(_.toString),
             bcc.map(_.toString),
             content,
             htmlContent,
             attachments)

  def doSendmail(to: Seq[String],
               from: String,
               subject: String,
               cc: Seq[String],
               bcc: Seq[String],
               bodyText: String,
               bodyHtml: Option[(String, Seq[Annex[Attachable]])],
               attachments: Seq[Annex[Attachable]])(implicit mode: Mode[_]): mode.Wrap[SendReport, SendAddressException with SendException] = mode wrap {

    import javax.mail._
    import javax.mail.util._
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
    for (r <- bcc) msg.addRecipient(Message.RecipientType.BCC, new InternetAddress(r))
    msg.setSubject(subject)

    def source(attachment: Annex[Attachable], inline: Boolean) = {
      val part = new MimeBodyPart()
      part.setDisposition(if(inline) Part.INLINE else Part.ATTACHMENT)
      if(inline) part.setHeader("Content-ID", s"<${attachment(_.resourceName)}>")
      else part.setFileName(attachment(_.resourceName))
      part.setDataHandler(new DataHandler(new ByteArrayDataSource(attachment(_.bytes).bytes, attachment(_.contentType).name)))
      part
    }
    
    bodyHtml match {
      case Some((html, inlines)) =>
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
          inlines.foreach { inline => top.addBodyPart(source(inline, true)) }
        }

        if (attachments.length > 0) {
          val body = new MimeBodyPart()
          body.setContent(top)
          top = new MimeMultipart("mixed")
          top.addBodyPart(body)
          attachments.foreach { attachment => top.addBodyPart(source(attachment, false)) }
        }
        msg.setContent(top)

      case None => {
        if (attachments.length > 0) {
          val body = new MimeBodyPart()
          body.setText(bodyText, "UTF-8")
          val top = new MimeMultipart("mixed")
          top.addBodyPart(body)
          attachments.foreach { attachment => top.addBodyPart(source(attachment, false)) }
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
  case class Capability[T: Sendable](msg: T) {
    def send()(implicit smtpServer: Smtp, mode: Mode[`send`]): mode.Wrap[SendReport, SendAddressException with SendException] = mode.wrap {
      smtpServer.sendmail(?[Sendable[T]].to(msg), ?[Sendable[T]].from(msg), ?[Sendable[T]].subject(msg), ?[Sendable[T]].cc(msg),
          ?[Sendable[T]].bcc(msg), ?[Sendable[T]].content(msg), ?[Sendable[T]].htmlContent(msg), ?[Sendable[T]].attachments(msg))
    }
  }
}

trait Sendable[T] {
  def to(t: T): Seq[Contact]
  def cc(t: T): Seq[Contact]
  def bcc(t: T): Seq[Contact]
  def from(t: T): Contact
  def subject(t: T): String
  def content(t: T): String
  def htmlContent(t: T): Option[(String, Seq[Annex[Attachable]])]
  def attachments(t: T): Seq[Annex[Attachable]]
}

case class SendAddressException(invalid: Set[Contact], validSent: Set[Contact],
    validUnsent: Set[Contact]) extends RuntimeException("the email could not be sent to all addresses")

case class SendException() extends RuntimeException("the email could not be sent")

