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

import rapture.core._

package sendmailBackends {
  object javamail {
    def apply()(implicit smtpServer: Smtp): SendmailBackend = implicitSendmailBackend
    implicit def implicitSendmailBackend(implicit smtpServer: Smtp): SendmailBackend =
      new SendmailBackend {
        def sendmail(to: Seq[String],
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
          props.put("mail.smtp.host", smtpServer.hostname)
          props.put("mail.smtp.port", smtpServer.port.toString)
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
          SendReport(msg.getMessageID)
        }
      }
  }
}
