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

package rapture.mime

/** Provides a typesafe list of MIME types, including mappings from common file extensions. This
  * list was produced from the public domain list of MIME types at
  * http://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types w*/
object MimeTypes {

  private var exts = Map[String, List[MimeType]]()
  private var types = Map[String, MimeType]()

  def unapply(mt: String) = types.get(mt)

  /** Looks up the MIME type based on a file extension. */
  def extension(ext: String) = exts.get(ext).getOrElse(Nil)

  /** Provides a simple wrapper around a String to represent a MIME type, and the filename
    * extensions which correspond to that type. */
  case class MimeType(name: String, extensions: String*) {
    override def toString = name
    for (ext <- extensions) exts = exts.updated(
        ext, this :: exts.get(ext).getOrElse(Nil))
    types = types.updated(name, this)
  }

  val `text/plain` = MimeType("text/plain",
                              "asc",
                              "conf",
                              "def",
                              "diff",
                              "in",
                              "list",
                              "log",
                              "pot",
                              "text",
                              "txt")

  val `application/x-www-form-urlencoded` = MimeType(
      "application/x-www-form-urlencoded")
  val `application/activemessage` = MimeType("application/activemessage")
  val `application/andrew-inset` = MimeType("application/andrew-inset", "ez")
  val `application/applefile` = MimeType("application/applefile")
  val `application/applixware` = MimeType("application/applixware", "aw")
  val `application/atom+xml` = MimeType("application/atom+xml", "atom")
  val `application/atomcat+xml` = MimeType(
      "application/atomcat+xml", "atomcat")
  val `application/atomicmail` = MimeType("application/atomicmail")
  val `application/atomsvc+xml` = MimeType(
      "application/atomsvc+xml", "atomsvc")
  val `application/auth-policy+xml` = MimeType("application/auth-policy+xml")
  val `application/batch-smtp` = MimeType("application/batch-smtp")
  val `application/beep+xml` = MimeType("application/beep+xml")
  val `application/cals-1840` = MimeType("application/cals-1840")
  val `application/ccxml+xml` = MimeType("application/ccxml+xml", "ccxml")
  val `application/cea-2018+xml` = MimeType("application/cea-2018+xml")
  val `application/cellml+xml` = MimeType("application/cellml+xml")
  val `application/cnrp+xml` = MimeType("application/cnrp+xml")
  val `application/commonground` = MimeType("application/commonground")
  val `application/conference-info+xml` = MimeType(
      "application/conference-info+xml")
  val `application/cpl+xml` = MimeType("application/cpl+xml")
  val `application/csta+xml` = MimeType("application/csta+xml")
  val `application/cstadata+xml` = MimeType("application/cstadata+xml")
  val `application/cu-seeme` = MimeType("application/cu-seeme", "cu")
  val `application/cybercash` = MimeType("application/cybercash")
  val `application/davmount+xml` = MimeType(
      "application/davmount+xml", "davmount")
  val `application/dca-rft` = MimeType("application/dca-rft")
  val `application/dec-dx` = MimeType("application/dec-dx")
  val `application/dialog-info+xml` = MimeType("application/dialog-info+xml")
  val `application/dicom` = MimeType("application/dicom")
  val `application/dns` = MimeType("application/dns")
  val `application/dsptype` = MimeType("application/dsptype", "tsp")
  val `application/dvcs` = MimeType("application/dvcs")
  val `application/ecmascript` = MimeType("application/ecmascript", "ecma")
  val `application/edi-consent` = MimeType("application/edi-consent")
  val `application/edi-x12` = MimeType("application/edi-x12")
  val `application/edifact` = MimeType("application/edifact")
  val `application/emma+xml` = MimeType("application/emma+xml", "emma")
  val `application/epp+xml` = MimeType("application/epp+xml")
  val `application/epub+zip` = MimeType("application/epub+zip", "epub")
  val `application/eshop` = MimeType("application/eshop")
  val `application/example` = MimeType("application/example")
  val `application/fastinfoset` = MimeType("application/fastinfoset")
  val `application/fastsoap` = MimeType("application/fastsoap")
  val `application/fits` = MimeType("application/fits")
  val `application/font-tdpfr` = MimeType("application/font-tdpfr", "pfr")
  val `application/futuresplash` = MimeType("application/futuresplash", "spl")
  val `application/h224` = MimeType("application/h224")
  val `application/hta` = MimeType("application/hta", "hta")
  val `application/http` = MimeType("application/http")
  val `application/hyperstudio` = MimeType("application/hyperstudio", "stk")
  val `application/ibe-key-request+xml` = MimeType(
      "application/ibe-key-request+xml")
  val `application/ibe-pkg-reply+xml` = MimeType(
      "application/ibe-pkg-reply+xml")
  val `application/ibe-pp-data` = MimeType("application/ibe-pp-data")
  val `application/iges` = MimeType("application/iges")
  val `application/im-iscomposing+xml` = MimeType(
      "application/im-iscomposing+xml")
  val `application/index` = MimeType("application/index")
  val `application/index.cmd` = MimeType("application/index.cmd")
  val `application/index.obj` = MimeType("application/index.obj")
  val `application/index.response` = MimeType("application/index.response")
  val `application/index.vnd` = MimeType("application/index.vnd")
  val `application/iotp` = MimeType("application/iotp")
  val `application/ipp` = MimeType("application/ipp")
  val `application/isup` = MimeType("application/isup")
  val `application/java-archive` = MimeType("application/java-archive", "jar")
  val `application/java-serialized-object` = MimeType(
      "application/java-serialized-object", "ser")
  val `application/java-vm` = MimeType("application/java-vm", "class")
  val `application/javascript` = MimeType("application/javascript", "js")
  val `application/json` = MimeType("application/json", "json")
  val `application/kpml-request+xml` = MimeType("application/kpml-request+xml")
  val `application/kpml-response+xml` = MimeType(
      "application/kpml-response+xml")
  val `application/lost+xml` = MimeType("application/lost+xml", "lostxml")
  val `application/mac-binhex40` = MimeType("application/mac-binhex40", "hqx")
  val `application/mac-compactpro` = MimeType(
      "application/mac-compactpro", "cpt")
  val `application/macwriteii` = MimeType("application/macwriteii")
  val `application/marc` = MimeType("application/marc", "mrc")
  val `application/mathematica` = MimeType(
      "application/mathematica", "ma", "mb", "nb")
  val `application/mathml+xml` = MimeType("application/mathml+xml", "mathml")

  val `application/mbms-associated-procedure-description+xml` = MimeType(
      "application/mbms-associated-procedure-description+xml")

  val `application/mbms-deregister+xml` = MimeType(
      "application/mbms-deregister+xml")
  val `application/mbms-envelope+xml` = MimeType(
      "application/mbms-envelope+xml")
  val `application/mbms-msk+xml` = MimeType("application/mbms-msk+xml")
  val `application/mbms-msk-response+xml` = MimeType(
      "application/mbms-msk-response+xml")
  val `application/mbms-protection-description+xml` = MimeType(
      "application/mbms-protection-description+xml")
  val `application/mbms-reception-report+xml` = MimeType(
      "application/mbms-reception-report+xml")
  val `application/mbms-register+xml` = MimeType(
      "application/mbms-register+xml")
  val `application/mbms-register-response+xml` = MimeType(
      "application/mbms-register-response+xml")
  val `application/mbms-user-service-description+xml` = MimeType(
      "application/mbms-user-service-description+xml")
  val `application/mbox` = MimeType("application/mbox", "mbox")
  val `application/media_control+xml` = MimeType(
      "application/media_control+xml")
  val `application/mediaservercontrol+xml` = MimeType(
      "application/mediaservercontrol+xml", "mscml")
  val `application/mikey` = MimeType("application/mikey")
  val `application/moss-keys` = MimeType("application/moss-keys")
  val `application/moss-signature` = MimeType("application/moss-signature")
  val `application/mosskey-data` = MimeType("application/mosskey-data")
  val `application/mosskey-request` = MimeType("application/mosskey-request")
  val `application/mp4` = MimeType("application/mp4", "mp4s")
  val `application/mpeg4-generic` = MimeType("application/mpeg4-generic")
  val `application/mpeg4-iod` = MimeType("application/mpeg4-iod")
  val `application/mpeg4-iod-xmt` = MimeType("application/mpeg4-iod-xmt")
  val `application/msaccess` = MimeType("application/msaccess", "mdb")
  val `application/msword` = MimeType("application/msword", "doc", "dot")
  val `application/mxf` = MimeType("application/mxf", "mxf")
  val `application/nasdata` = MimeType("application/nasdata")
  val `application/news-checkgroups` = MimeType("application/news-checkgroups")
  val `application/news-groupinfo` = MimeType("application/news-groupinfo")
  val `application/news-transmission` = MimeType(
      "application/news-transmission")
  val `application/nss` = MimeType("application/nss")
  val `application/ocsp-request` = MimeType("application/ocsp-request")
  val `application/ocsp-response` = MimeType("application/ocsp-response")

  val `application/octet-stream` = MimeType("application/octet-stream",
                                            "bin",
                                            "bpk",
                                            "deploy",
                                            "dist",
                                            "distz",
                                            "dmg",
                                            "dms",
                                            "dump",
                                            "elc",
                                            "iso",
                                            "lha",
                                            "lrf",
                                            "lzh",
                                            "pkg",
                                            "so")

  val `application/oda` = MimeType("application/oda", "oda")
  val `application/oebps-package+xml` = MimeType(
      "application/oebps-package+xml", "opf")
  val `application/ogg` = MimeType("application/ogg", "ogg", "ogx")
  val `application/onenote` = MimeType(
      "application/onenote", "onepkg", "onetmp", "onetoc", "onetoc2")
  val `application/parityfec` = MimeType("application/parityfec")
  val `application/patch-ops-error+xml` = MimeType(
      "application/patch-ops-error+xml", "xer")
  val `application/pdf` = MimeType("application/pdf", "pdf")
  val `application/pgp-encrypted` = MimeType(
      "application/pgp-encrypted", "pgp")
  val `application/pgp-keys` = MimeType("application/pgp-keys", "key")
  val `application/pgp-signature` = MimeType(
      "application/pgp-signature", "asc", "pgp", "sig")
  val `application/pics-rules` = MimeType("application/pics-rules", "prf")
  val `application/pidf+xml` = MimeType("application/pidf+xml")
  val `application/pidf-diff+xml` = MimeType("application/pidf-diff+xml")
  val `application/pkcs10` = MimeType("application/pkcs10", "p10")
  val `application/pkcs7-mime` = MimeType(
      "application/pkcs7-mime", "p7c", "p7m")
  val `application/pkcs7-signature` = MimeType(
      "application/pkcs7-signature", "p7s")
  val `application/pkix-cert` = MimeType("application/pkix-cert", "cer")
  val `application/pkix-crl` = MimeType("application/pkix-crl", "crl")
  val `application/pkix-pkipath` = MimeType(
      "application/pkix-pkipath", "pkipath")
  val `application/pkixcmp` = MimeType("application/pkixcmp", "pki")
  val `application/pls+xml` = MimeType("application/pls+xml", "pls")
  val `application/poc-settings+xml` = MimeType("application/poc-settings+xml")
  val `application/postscript` = MimeType(
      "application/postscript", "ai", "eps", "ps")
  val `application/prs.alvestrand.titrax-sheet` = MimeType(
      "application/prs.alvestrand.titrax-sheet")
  val `application/prs.cww` = MimeType("application/prs.cww", "cww")
  val `application/prs.nprend` = MimeType("application/prs.nprend")
  val `application/prs.plucker` = MimeType("application/prs.plucker")
  val `application/qsig` = MimeType("application/qsig")
  val `application/rar` = MimeType("application/rar", "rar")
  val `application/rdf+xml` = MimeType("application/rdf+xml", "rdf")
  val `application/reginfo+xml` = MimeType("application/reginfo+xml", "rif")
  val `application/relax-ng-compact-syntax` = MimeType(
      "application/relax-ng-compact-syntax", "rnc")
  val `application/remote-printing` = MimeType("application/remote-printing")
  val `application/resource-lists+xml` = MimeType(
      "application/resource-lists+xml", "rl")
  val `application/resource-lists-diff+xml` = MimeType(
      "application/resource-lists-diff+xml", "rld")
  val `application/riscos` = MimeType("application/riscos")
  val `application/rlmi+xml` = MimeType("application/rlmi+xml")
  val `application/rls-services+xml` = MimeType(
      "application/rls-services+xml", "rs")
  val `application/rsd+xml` = MimeType("application/rsd+xml", "rsd")
  val `application/rss+xml` = MimeType("application/rss+xml", "rss")
  val `application/rtf` = MimeType("application/rtf", "rtf")
  val `application/rtx` = MimeType("application/rtx")
  val `application/samlassertion+xml` = MimeType(
      "application/samlassertion+xml")
  val `application/samlmetadata+xml` = MimeType("application/samlmetadata+xml")
  val `application/sbml+xml` = MimeType("application/sbml+xml", "sbml")
  val `application/scvp-cv-request` = MimeType(
      "application/scvp-cv-request", "scq")
  val `application/scvp-cv-response` = MimeType(
      "application/scvp-cv-response", "scs")
  val `application/scvp-vp-request` = MimeType(
      "application/scvp-vp-request", "spq")
  val `application/scvp-vp-response` = MimeType(
      "application/scvp-vp-response", "spp")
  val `application/sdp` = MimeType("application/sdp", "sdp")
  val `application/set-payment` = MimeType("application/set-payment")
  val `application/set-payment-initiation` = MimeType(
      "application/set-payment-initiation", "setpay")
  val `application/set-registration` = MimeType("application/set-registration")
  val `application/set-registration-initiation` = MimeType(
      "application/set-registration-initiation", "setreg")
  val `application/sgml` = MimeType("application/sgml")
  val `application/sgml-open-catalog` = MimeType(
      "application/sgml-open-catalog")
  val `application/shf+xml` = MimeType("application/shf+xml", "shf")
  val `application/sieve` = MimeType("application/sieve")
  val `application/simple-filter+xml` = MimeType(
      "application/simple-filter+xml")
  val `application/simple-message-summary` = MimeType(
      "application/simple-message-summary")
  val `application/simplesymbolcontainer` = MimeType(
      "application/simplesymbolcontainer")
  val `application/slate` = MimeType("application/slate")
  val `application/smil` = MimeType("application/smil", "smi", "smil")
  val `application/smil+xml` = MimeType("application/smil+xml", "smi", "smil")
  val `application/soap+fastinfoset` = MimeType("application/soap+fastinfoset")
  val `application/soap+xml` = MimeType("application/soap+xml")
  val `application/sparql-query` = MimeType("application/sparql-query", "rq")
  val `application/sparql-results+xml` = MimeType(
      "application/sparql-results+xml", "srx")
  val `application/spirits-event+xml` = MimeType(
      "application/spirits-event+xml")
  val `application/srgs` = MimeType("application/srgs", "gram")
  val `application/srgs+xml` = MimeType("application/srgs+xml", "grxml")
  val `application/ssml+xml` = MimeType("application/ssml+xml", "ssml")
  val `application/timestamp-query` = MimeType("application/timestamp-query")
  val `application/timestamp-reply` = MimeType("application/timestamp-reply")
  val `application/tve-trigger` = MimeType("application/tve-trigger")
  val `application/ulpfec` = MimeType("application/ulpfec")
  val `application/vemmi` = MimeType("application/vemmi")
  val `application/vividence.scriptfile` = MimeType(
      "application/vividence.scriptfile")
  val `application/vnd.3gpp.bsf+xml` = MimeType("application/vnd.3gpp.bsf+xml")
  val `application/vnd.3gpp.pic-bw-large` = MimeType(
      "application/vnd.3gpp.pic-bw-large", "plb")
  val `application/vnd.3gpp.pic-bw-small` = MimeType(
      "application/vnd.3gpp.pic-bw-small", "psb")
  val `application/vnd.3gpp.pic-bw-var` = MimeType(
      "application/vnd.3gpp.pic-bw-var", "pvb")
  val `application/vnd.3gpp.sms` = MimeType("application/vnd.3gpp.sms")
  val `application/vnd.3gpp2.bcmcsinfo+xml` = MimeType(
      "application/vnd.3gpp2.bcmcsinfo+xml")
  val `application/vnd.3gpp2.sms` = MimeType("application/vnd.3gpp2.sms")
  val `application/vnd.3gpp2.tcap` = MimeType(
      "application/vnd.3gpp2.tcap", "tcap")
  val `application/vnd.3m.post-it-notes` = MimeType(
      "application/vnd.3m.post-it-notes", "pwn")
  val `application/vnd.accpac.simply.aso` = MimeType(
      "application/vnd.accpac.simply.aso", "aso")
  val `application/vnd.accpac.simply.imp` = MimeType(
      "application/vnd.accpac.simply.imp", "imp")
  val `application/vnd.acucobol` = MimeType("application/vnd.acucobol", "acu")
  val `application/vnd.acucorp` = MimeType(
      "application/vnd.acucorp", "acutc", "atc")

  val `application/vnd.adobe.air-application-installer-package+zip` = MimeType(
      "application/vnd.adobe.air-application-installer-package+zip", "air")

  val `application/vnd.adobe.xdp+xml` = MimeType(
      "application/vnd.adobe.xdp+xml", "xdp")
  val `application/vnd.adobe.xfdf` = MimeType(
      "application/vnd.adobe.xfdf", "xfdf")
  val `application/vnd.aether.imp` = MimeType("application/vnd.aether.imp")
  val `application/vnd.airzip.filesecure.azf` = MimeType(
      "application/vnd.airzip.filesecure.azf", "azf")
  val `application/vnd.airzip.filesecure.azs` = MimeType(
      "application/vnd.airzip.filesecure.azs", "azs")
  val `application/vnd.amazon.ebook` = MimeType(
      "application/vnd.amazon.ebook", "azw")
  val `application/vnd.americandynamics.acc` = MimeType(
      "application/vnd.americandynamics.acc", "acc")
  val `application/vnd.amiga.ami` = MimeType(
      "application/vnd.amiga.ami", "ami")
  val `application/vnd.android.package-archive` = MimeType(
      "application/vnd.android.package-archive", "apk")

  val `application/vnd.anser-web-certificate-issue-initiation` = MimeType(
      "application/vnd.anser-web-certificate-issue-initiation", "cii")

  val `application/vnd.anser-web-funds-transfer-initiation` = MimeType(
      "application/vnd.anser-web-funds-transfer-initiation", "fti")

  val `application/vnd.antix.game-component` = MimeType(
      "application/vnd.antix.game-component", "atx")
  val `application/vnd.apple.installer+xml` = MimeType(
      "application/vnd.apple.installer+xml", "mpkg")
  val `application/vnd.arastra.swi` = MimeType(
      "application/vnd.arastra.swi", "swi")
  val `application/vnd.audiograph` = MimeType(
      "application/vnd.audiograph", "aep")
  val `application/vnd.autopackage` = MimeType("application/vnd.autopackage")
  val `application/vnd.avistar+xml` = MimeType("application/vnd.avistar+xml")
  val `application/vnd.blueice.multipass` = MimeType(
      "application/vnd.blueice.multipass", "mpm")
  val `application/vnd.bluetooth.ep.oob` = MimeType(
      "application/vnd.bluetooth.ep.oob")
  val `application/vnd.bmi` = MimeType("application/vnd.bmi", "bmi")
  val `application/vnd.businessobjects` = MimeType(
      "application/vnd.businessobjects", "rep")
  val `application/vnd.cab-jscript` = MimeType("application/vnd.cab-jscript")
  val `application/vnd.canon-cpdl` = MimeType("application/vnd.canon-cpdl")
  val `application/vnd.canon-lips` = MimeType("application/vnd.canon-lips")
  val `application/vnd.cendio.thinlinc.clientconf` = MimeType(
      "application/vnd.cendio.thinlinc.clientconf")
  val `application/vnd.chemdraw+xml` = MimeType(
      "application/vnd.chemdraw+xml", "cdxml")
  val `application/vnd.chipnuts.karaoke-mmd` = MimeType(
      "application/vnd.chipnuts.karaoke-mmd", "mmd")
  val `application/vnd.cinderella` = MimeType(
      "application/vnd.cinderella", "cdy")
  val `application/vnd.cirpack.isdn-ext` = MimeType(
      "application/vnd.cirpack.isdn-ext")
  val `application/vnd.claymore` = MimeType("application/vnd.claymore", "cla")
  val `application/vnd.clonk.c4group` = MimeType(
      "application/vnd.clonk.c4group", "c4d", "c4f", "c4g", "c4p", "c4u")
  val `application/vnd.commerce-battelle` = MimeType(
      "application/vnd.commerce-battelle")
  val `application/vnd.commonspace` = MimeType(
      "application/vnd.commonspace", "csp")
  val `application/vnd.contact.cmsg` = MimeType(
      "application/vnd.contact.cmsg", "cdbcmsg")
  val `application/vnd.cosmocaller` = MimeType(
      "application/vnd.cosmocaller", "cmc")
  val `application/vnd.crick.clicker` = MimeType(
      "application/vnd.crick.clicker", "clkx")
  val `application/vnd.crick.clicker.keyboard` = MimeType(
      "application/vnd.crick.clicker.keyboard", "clkk")
  val `application/vnd.crick.clicker.palette` = MimeType(
      "application/vnd.crick.clicker.palette", "clkp")
  val `application/vnd.crick.clicker.template` = MimeType(
      "application/vnd.crick.clicker.template", "clkt")
  val `application/vnd.crick.clicker.wordbank` = MimeType(
      "application/vnd.crick.clicker.wordbank", "clkw")
  val `application/vnd.criticaltools.wbs+xml` = MimeType(
      "application/vnd.criticaltools.wbs+xml", "wbs")
  val `application/vnd.ctc-posml` = MimeType(
      "application/vnd.ctc-posml", "pml")
  val `application/vnd.ctct.ws+xml` = MimeType("application/vnd.ctct.ws+xml")
  val `application/vnd.cups-pdf` = MimeType("application/vnd.cups-pdf")
  val `application/vnd.cups-postscript` = MimeType(
      "application/vnd.cups-postscript")
  val `application/vnd.cups-ppd` = MimeType("application/vnd.cups-ppd", "ppd")
  val `application/vnd.cups-raster` = MimeType("application/vnd.cups-raster")
  val `application/vnd.cups-raw` = MimeType("application/vnd.cups-raw")
  val `application/vnd.curl.car` = MimeType("application/vnd.curl.car", "car")
  val `application/vnd.curl.pcurl` = MimeType(
      "application/vnd.curl.pcurl", "pcurl")
  val `application/vnd.cybank` = MimeType("application/vnd.cybank")
  val `application/vnd.data-vision.rdz` = MimeType(
      "application/vnd.data-vision.rdz", "rdz")
  val `application/vnd.denovo.fcselayout-link` = MimeType(
      "application/vnd.denovo.fcselayout-link", "fe", "_launch")
  val `application/vnd.dir-bi.plate-dl-nosuffix` = MimeType(
      "application/vnd.dir-bi.plate-dl-nosuffix")
  val `application/vnd.dna` = MimeType("application/vnd.dna", "dna")
  val `application/vnd.dolby.mlp` = MimeType(
      "application/vnd.dolby.mlp", "mlp")
  val `application/vnd.dolby.mobile.1` = MimeType(
      "application/vnd.dolby.mobile.1")
  val `application/vnd.dolby.mobile.2` = MimeType(
      "application/vnd.dolby.mobile.2")
  val `application/vnd.dpgraph` = MimeType("application/vnd.dpgraph", "dpg")
  val `application/vnd.dreamfactory` = MimeType(
      "application/vnd.dreamfactory", "dfac")
  val `application/vnd.dvb.esgcontainer` = MimeType(
      "application/vnd.dvb.esgcontainer")
  val `application/vnd.dvb.ipdcdftnotifaccess` = MimeType(
      "application/vnd.dvb.ipdcdftnotifaccess")
  val `application/vnd.dvb.ipdcesgaccess` = MimeType(
      "application/vnd.dvb.ipdcesgaccess")
  val `application/vnd.dvb.ipdcroaming` = MimeType(
      "application/vnd.dvb.ipdcroaming")
  val `application/vnd.dvb.iptv.alfec-base` = MimeType(
      "application/vnd.dvb.iptv.alfec-base")
  val `application/vnd.dvb.iptv.alfec-enhancement` = MimeType(
      "application/vnd.dvb.iptv.alfec-enhancement")
  val `application/vnd.dvb.notif-aggregate-root+xml` = MimeType(
      "application/vnd.dvb.notif-aggregate-root+xml")
  val `application/vnd.dvb.notif-container+xml` = MimeType(
      "application/vnd.dvb.notif-container+xml")
  val `application/vnd.dvb.notif-generic+xml` = MimeType(
      "application/vnd.dvb.notif-generic+xml")
  val `application/vnd.dvb.notif-ia-msglist+xml` = MimeType(
      "application/vnd.dvb.notif-ia-msglist+xml")

  val `application/vnd.dvb.notif-ia-registration-request+xml` = MimeType(
      "application/vnd.dvb.notif-ia-registration-request+xml")

  val `application/vnd.dvb.notif-ia-registration-response+xml` = MimeType(
      "application/vnd.dvb.notif-ia-registration-response+xml")

  val `application/vnd.dvb.notif-init+xml` = MimeType(
      "application/vnd.dvb.notif-init+xml")
  val `application/vnd.dxr` = MimeType("application/vnd.dxr")
  val `application/vnd.dynageo` = MimeType("application/vnd.dynageo", "geo")
  val `application/vnd.ecdis-update` = MimeType("application/vnd.ecdis-update")
  val `application/vnd.ecowin.chart` = MimeType(
      "application/vnd.ecowin.chart", "mag")
  val `application/vnd.ecowin.filerequest` = MimeType(
      "application/vnd.ecowin.filerequest")
  val `application/vnd.ecowin.fileupdate` = MimeType(
      "application/vnd.ecowin.fileupdate")
  val `application/vnd.ecowin.series` = MimeType(
      "application/vnd.ecowin.series")
  val `application/vnd.ecowin.seriesrequest` = MimeType(
      "application/vnd.ecowin.seriesrequest")
  val `application/vnd.ecowin.seriesupdate` = MimeType(
      "application/vnd.ecowin.seriesupdate")
  val `application/vnd.emclient.accessrequest+xml` = MimeType(
      "application/vnd.emclient.accessrequest+xml")
  val `application/vnd.enliven` = MimeType("application/vnd.enliven", "nml")
  val `application/vnd.epson.esf` = MimeType(
      "application/vnd.epson.esf", "esf")
  val `application/vnd.epson.msf` = MimeType(
      "application/vnd.epson.msf", "msf")
  val `application/vnd.epson.quickanime` = MimeType(
      "application/vnd.epson.quickanime", "qam")
  val `application/vnd.epson.salt` = MimeType(
      "application/vnd.epson.salt", "slt")
  val `application/vnd.epson.ssf` = MimeType(
      "application/vnd.epson.ssf", "ssf")
  val `application/vnd.ericsson.quickcall` = MimeType(
      "application/vnd.ericsson.quickcall")
  val `application/vnd.eszigno3+xml` = MimeType(
      "application/vnd.eszigno3+xml", "es3", "et3")
  val `application/vnd.etsi.aoc+xml` = MimeType("application/vnd.etsi.aoc+xml")
  val `application/vnd.etsi.cug+xml` = MimeType("application/vnd.etsi.cug+xml")
  val `application/vnd.etsi.iptvcommand+xml` = MimeType(
      "application/vnd.etsi.iptvcommand+xml")
  val `application/vnd.etsi.iptvdiscovery+xml` = MimeType(
      "application/vnd.etsi.iptvdiscovery+xml")
  val `application/vnd.etsi.iptvprofile+xml` = MimeType(
      "application/vnd.etsi.iptvprofile+xml")
  val `application/vnd.etsi.iptvsad-bc+xml` = MimeType(
      "application/vnd.etsi.iptvsad-bc+xml")
  val `application/vnd.etsi.iptvsad-cod+xml` = MimeType(
      "application/vnd.etsi.iptvsad-cod+xml")
  val `application/vnd.etsi.iptvsad-npvr+xml` = MimeType(
      "application/vnd.etsi.iptvsad-npvr+xml")
  val `application/vnd.etsi.iptvueprofile+xml` = MimeType(
      "application/vnd.etsi.iptvueprofile+xml")
  val `application/vnd.etsi.mcid+xml` = MimeType(
      "application/vnd.etsi.mcid+xml")
  val `application/vnd.etsi.sci+xml` = MimeType("application/vnd.etsi.sci+xml")
  val `application/vnd.etsi.simservs+xml` = MimeType(
      "application/vnd.etsi.simservs+xml")
  val `application/vnd.eudora.data` = MimeType("application/vnd.eudora.data")
  val `application/vnd.ezpix-album` = MimeType(
      "application/vnd.ezpix-album", "ez2")
  val `application/vnd.ezpix-package` = MimeType(
      "application/vnd.ezpix-package", "ez3")
  val `application/vnd.f-secure.mobile` = MimeType(
      "application/vnd.f-secure.mobile")
  val `application/vnd.fdf` = MimeType("application/vnd.fdf", "fdf")
  val `application/vnd.fdsn.mseed` = MimeType(
      "application/vnd.fdsn.mseed", "mseed")
  val `application/vnd.fdsn.seed` = MimeType(
      "application/vnd.fdsn.seed", "dataless", "seed")
  val `application/vnd.ffsns` = MimeType("application/vnd.ffsns")
  val `application/vnd.fints` = MimeType("application/vnd.fints")
  val `application/vnd.flographit` = MimeType(
      "application/vnd.flographit", "gph")
  val `application/vnd.fluxtime.clip` = MimeType(
      "application/vnd.fluxtime.clip", "ftc")
  val `application/vnd.font-fontforge-sfd` = MimeType(
      "application/vnd.font-fontforge-sfd")
  val `application/vnd.framemaker` = MimeType(
      "application/vnd.framemaker", "book", "fm", "frame", "maker")
  val `application/vnd.frogans.fnc` = MimeType(
      "application/vnd.frogans.fnc", "fnc")
  val `application/vnd.frogans.ltf` = MimeType(
      "application/vnd.frogans.ltf", "ltf")
  val `application/vnd.fsc.weblaunch` = MimeType(
      "application/vnd.fsc.weblaunch", "fsc")
  val `application/vnd.fujitsu.oasys` = MimeType(
      "application/vnd.fujitsu.oasys", "oas")
  val `application/vnd.fujitsu.oasys2` = MimeType(
      "application/vnd.fujitsu.oasys2", "oa2")
  val `application/vnd.fujitsu.oasys3` = MimeType(
      "application/vnd.fujitsu.oasys3", "oa3")
  val `application/vnd.fujitsu.oasysgp` = MimeType(
      "application/vnd.fujitsu.oasysgp", "fg5")
  val `application/vnd.fujitsu.oasysprs` = MimeType(
      "application/vnd.fujitsu.oasysprs", "bh2")
  val `application/vnd.fujixerox.art-ex` = MimeType(
      "application/vnd.fujixerox.art-ex")
  val `application/vnd.fujixerox.art4` = MimeType(
      "application/vnd.fujixerox.art4")
  val `application/vnd.fujixerox.ddd` = MimeType(
      "application/vnd.fujixerox.ddd", "ddd")
  val `application/vnd.fujixerox.docuworks` = MimeType(
      "application/vnd.fujixerox.docuworks", "xdw")
  val `application/vnd.fujixerox.docuworks.binder` = MimeType(
      "application/vnd.fujixerox.docuworks.binder", "xbd")
  val `application/vnd.fujixerox.hbpl` = MimeType(
      "application/vnd.fujixerox.hbpl")
  val `application/vnd.fut-misnet` = MimeType("application/vnd.fut-misnet")
  val `application/vnd.fuzzysheet` = MimeType(
      "application/vnd.fuzzysheet", "fzs")
  val `application/vnd.genomatix.tuxedo` = MimeType(
      "application/vnd.genomatix.tuxedo", "txd")
  val `application/vnd.geogebra.file` = MimeType(
      "application/vnd.geogebra.file", "ggb")
  val `application/vnd.geogebra.tool` = MimeType(
      "application/vnd.geogebra.tool", "ggt")
  val `application/vnd.geometry-explorer` = MimeType(
      "application/vnd.geometry-explorer", "gex", "gre")
  val `application/vnd.gmx` = MimeType("application/vnd.gmx", "gmx")
  val `application/vnd.google-earth.kml+xml` = MimeType(
      "application/vnd.google-earth.kml+xml", "kml")
  val `application/vnd.google-earth.kmz` = MimeType(
      "application/vnd.google-earth.kmz", "kmz")
  val `application/vnd.grafeq` = MimeType(
      "application/vnd.grafeq", "gqf", "gqs")
  val `application/vnd.gridmp` = MimeType("application/vnd.gridmp")
  val `application/vnd.groove-account` = MimeType(
      "application/vnd.groove-account", "gac")
  val `application/vnd.groove-help` = MimeType(
      "application/vnd.groove-help", "ghf")
  val `application/vnd.groove-identity-message` = MimeType(
      "application/vnd.groove-identity-message", "gim")
  val `application/vnd.groove-injector` = MimeType(
      "application/vnd.groove-injector", "grv")
  val `application/vnd.groove-tool-message` = MimeType(
      "application/vnd.groove-tool-message", "gtm")
  val `application/vnd.groove-tool-template` = MimeType(
      "application/vnd.groove-tool-template", "tpl")
  val `application/vnd.groove-vcard` = MimeType(
      "application/vnd.groove-vcard", "vcg")
  val `application/vnd.handheld-entertainment+xml` = MimeType(
      "application/vnd.handheld-entertainment+xml", "zmm")
  val `application/vnd.hbci` = MimeType("application/vnd.hbci", "hbci")
  val `application/vnd.hcl-bireports` = MimeType(
      "application/vnd.hcl-bireports")
  val `application/vnd.hhe.lesson-player` = MimeType(
      "application/vnd.hhe.lesson-player", "les")
  val `application/vnd.hp-hpgl` = MimeType("application/vnd.hp-hpgl", "hpgl")
  val `application/vnd.hp-hpid` = MimeType("application/vnd.hp-hpid", "hpid")
  val `application/vnd.hp-hps` = MimeType("application/vnd.hp-hps", "hps")
  val `application/vnd.hp-jlyt` = MimeType("application/vnd.hp-jlyt", "jlt")
  val `application/vnd.hp-pcl` = MimeType("application/vnd.hp-pcl", "pcl")
  val `application/vnd.hp-pclxl` = MimeType(
      "application/vnd.hp-pclxl", "pclxl")
  val `application/vnd.httphone` = MimeType("application/vnd.httphone")
  val `application/vnd.hydrostatix.sof-data` = MimeType(
      "application/vnd.hydrostatix.sof-data", "sfd", "-hdstx")
  val `application/vnd.hzn-3d-crossword` = MimeType(
      "application/vnd.hzn-3d-crossword", "x3d")
  val `application/vnd.ibm.afplinedata` = MimeType(
      "application/vnd.ibm.afplinedata")
  val `application/vnd.ibm.electronic-media` = MimeType(
      "application/vnd.ibm.electronic-media")
  val `application/vnd.ibm.minipay` = MimeType(
      "application/vnd.ibm.minipay", "mpy")
  val `application/vnd.ibm.modcap` = MimeType(
      "application/vnd.ibm.modcap", "afp", "list3820", "listafp")
  val `application/vnd.ibm.rights-management` = MimeType(
      "application/vnd.ibm.rights-management", "irm")
  val `application/vnd.ibm.secure-container` = MimeType(
      "application/vnd.ibm.secure-container", "sc")
  val `application/vnd.iccprofile` = MimeType(
      "application/vnd.iccprofile", "icc", "icm")
  val `application/vnd.igloader` = MimeType("application/vnd.igloader", "igl")
  val `application/vnd.immervision-ivp` = MimeType(
      "application/vnd.immervision-ivp", "ivp")
  val `application/vnd.immervision-ivu` = MimeType(
      "application/vnd.immervision-ivu", "ivu")
  val `application/vnd.informedcontrol.rms+xml` = MimeType(
      "application/vnd.informedcontrol.rms+xml")
  val `application/vnd.informix-visionary` = MimeType(
      "application/vnd.informix-visionary")
  val `application/vnd.intercon.formnet` = MimeType(
      "application/vnd.intercon.formnet", "xpw", "xpx")
  val `application/vnd.intertrust.digibox` = MimeType(
      "application/vnd.intertrust.digibox")
  val `application/vnd.intertrust.nncp` = MimeType(
      "application/vnd.intertrust.nncp")
  val `application/vnd.intu.qbo` = MimeType("application/vnd.intu.qbo", "qbo")
  val `application/vnd.intu.qfx` = MimeType("application/vnd.intu.qfx", "qfx")
  val `application/vnd.iptc.g2.conceptitem+xml` = MimeType(
      "application/vnd.iptc.g2.conceptitem+xml")
  val `application/vnd.iptc.g2.knowledgeitem+xml` = MimeType(
      "application/vnd.iptc.g2.knowledgeitem+xml")
  val `application/vnd.iptc.g2.newsitem+xml` = MimeType(
      "application/vnd.iptc.g2.newsitem+xml")
  val `application/vnd.iptc.g2.packageitem+xml` = MimeType(
      "application/vnd.iptc.g2.packageitem+xml")
  val `application/vnd.ipunplugged.rcprofile` = MimeType(
      "application/vnd.ipunplugged.rcprofile", "rcprofile")
  val `application/vnd.irepository.package+xml` = MimeType(
      "application/vnd.irepository.package+xml", "irp")
  val `application/vnd.is-xpr` = MimeType("application/vnd.is-xpr", "xpr")
  val `application/vnd.jam` = MimeType("application/vnd.jam", "jam")
  val `application/vnd.japannet-directory-service` = MimeType(
      "application/vnd.japannet-directory-service")
  val `application/vnd.japannet-jpnstore-wakeup` = MimeType(
      "application/vnd.japannet-jpnstore-wakeup")
  val `application/vnd.japannet-payment-wakeup` = MimeType(
      "application/vnd.japannet-payment-wakeup")
  val `application/vnd.japannet-registration` = MimeType(
      "application/vnd.japannet-registration")
  val `application/vnd.japannet-registration-wakeup` = MimeType(
      "application/vnd.japannet-registration-wakeup")
  val `application/vnd.japannet-setstore-wakeup` = MimeType(
      "application/vnd.japannet-setstore-wakeup")
  val `application/vnd.japannet-verification` = MimeType(
      "application/vnd.japannet-verification")
  val `application/vnd.japannet-verification-wakeup` = MimeType(
      "application/vnd.japannet-verification-wakeup")
  val `application/vnd.jcp.javame.midlet-rms` = MimeType(
      "application/vnd.jcp.javame.midlet-rms", "rms")
  val `application/vnd.jisp` = MimeType("application/vnd.jisp", "jisp")
  val `application/vnd.joost.joda-archive` = MimeType(
      "application/vnd.joost.joda-archive", "joda")
  val `application/vnd.kahootz` = MimeType(
      "application/vnd.kahootz", "ktr", "ktz")
  val `application/vnd.kde.karbon` = MimeType(
      "application/vnd.kde.karbon", "karbon")
  val `application/vnd.kde.kchart` = MimeType(
      "application/vnd.kde.kchart", "chrt")
  val `application/vnd.kde.kformula` = MimeType(
      "application/vnd.kde.kformula", "kfo")
  val `application/vnd.kde.kivio` = MimeType(
      "application/vnd.kde.kivio", "flw")
  val `application/vnd.kde.kontour` = MimeType(
      "application/vnd.kde.kontour", "kon")
  val `application/vnd.kde.kpresenter` = MimeType(
      "application/vnd.kde.kpresenter", "kpr", "kpt")
  val `application/vnd.kde.kspread` = MimeType(
      "application/vnd.kde.kspread", "ksp")
  val `application/vnd.kde.kword` = MimeType(
      "application/vnd.kde.kword", "kwd", "kwt")
  val `application/vnd.kenameaapp` = MimeType(
      "application/vnd.kenameaapp", "htke")
  val `application/vnd.kidspiration` = MimeType(
      "application/vnd.kidspiration", "kia")
  val `application/vnd.kinar` = MimeType("application/vnd.kinar", "kne", "knp")
  val `application/vnd.koan` = MimeType(
      "application/vnd.koan", "skd", "skm", "skp", "skt")
  val `application/vnd.kodak-descriptor` = MimeType(
      "application/vnd.kodak-descriptor", "sse")
  val `application/vnd.liberty-request+xml` = MimeType(
      "application/vnd.liberty-request+xml")

  val `application/vnd.llamagraphics.life-balance.desktop` = MimeType(
      "application/vnd.llamagraphics.life-balance.desktop", "lbd")

  val `application/vnd.llamagraphics.life-balance.exchange+xml` = MimeType(
      "application/vnd.llamagraphics.life-balance.exchange+xml", "lbe")

  val `application/vnd.lotus-1-2-3` = MimeType(
      "application/vnd.lotus-1-2-3", "123")
  val `application/vnd.lotus-approach` = MimeType(
      "application/vnd.lotus-approach", "apr")
  val `application/vnd.lotus-freelance` = MimeType(
      "application/vnd.lotus-freelance", "pre")
  val `application/vnd.lotus-notes` = MimeType(
      "application/vnd.lotus-notes", "nsf")
  val `application/vnd.lotus-organizer` = MimeType(
      "application/vnd.lotus-organizer", "org")
  val `application/vnd.lotus-screencam` = MimeType(
      "application/vnd.lotus-screencam", "scm")
  val `application/vnd.lotus-wordpro` = MimeType(
      "application/vnd.lotus-wordpro", "lwp")
  val `application/vnd.macports.portpkg` = MimeType(
      "application/vnd.macports.portpkg", "portpkg")
  val `application/vnd.marlin.drm.actiontoken+xml` = MimeType(
      "application/vnd.marlin.drm.actiontoken+xml")
  val `application/vnd.marlin.drm.conftoken+xml` = MimeType(
      "application/vnd.marlin.drm.conftoken+xml")
  val `application/vnd.marlin.drm.license+xml` = MimeType(
      "application/vnd.marlin.drm.license+xml")
  val `application/vnd.marlin.drm.mdcf` = MimeType(
      "application/vnd.marlin.drm.mdcf")
  val `application/vnd.mcd` = MimeType("application/vnd.mcd", "mcd")
  val `application/vnd.medcalcdata` = MimeType(
      "application/vnd.medcalcdata", "mc1")
  val `application/vnd.mediastation.cdkey` = MimeType(
      "application/vnd.mediastation.cdkey", "cdkey")
  val `application/vnd.meridian-slingshot` = MimeType(
      "application/vnd.meridian-slingshot")
  val `application/vnd.mfer` = MimeType("application/vnd.mfer", "mwf")
  val `application/vnd.mfmp` = MimeType("application/vnd.mfmp", "mfm")
  val `application/vnd.micrografx.flo` = MimeType(
      "application/vnd.micrografx.flo", "flo")
  val `application/vnd.micrografx.igx` = MimeType(
      "application/vnd.micrografx.igx", "igx")
  val `application/vnd.mif` = MimeType("application/vnd.mif", "mif")
  val `application/vnd.minisoft-hp3000-save` = MimeType(
      "application/vnd.minisoft-hp3000-save")

  val `application/vnd.mitsubishi.misty-guard.trustweb` = MimeType(
      "application/vnd.mitsubishi.misty-guard.trustweb")

  val `application/vnd.mobius.daf` = MimeType(
      "application/vnd.mobius.daf", "daf")
  val `application/vnd.mobius.dis` = MimeType(
      "application/vnd.mobius.dis", "dis")
  val `application/vnd.mobius.mbk` = MimeType(
      "application/vnd.mobius.mbk", "mbk")
  val `application/vnd.mobius.mqy` = MimeType(
      "application/vnd.mobius.mqy", "mqy")
  val `application/vnd.mobius.msl` = MimeType(
      "application/vnd.mobius.msl", "msl")
  val `application/vnd.mobius.plc` = MimeType(
      "application/vnd.mobius.plc", "plc")
  val `application/vnd.mobius.txf` = MimeType(
      "application/vnd.mobius.txf", "txf")
  val `application/vnd.mophun.application` = MimeType(
      "application/vnd.mophun.application", "mpn")
  val `application/vnd.mophun.certificate` = MimeType(
      "application/vnd.mophun.certificate", "mpc")
  val `application/vnd.motorola.flexsuite` = MimeType(
      "application/vnd.motorola.flexsuite")
  val `application/vnd.motorola.flexsuite.adsi` = MimeType(
      "application/vnd.motorola.flexsuite.adsi")
  val `application/vnd.motorola.flexsuite.fis` = MimeType(
      "application/vnd.motorola.flexsuite.fis")
  val `application/vnd.motorola.flexsuite.gotap` = MimeType(
      "application/vnd.motorola.flexsuite.gotap")
  val `application/vnd.motorola.flexsuite.kmr` = MimeType(
      "application/vnd.motorola.flexsuite.kmr")
  val `application/vnd.motorola.flexsuite.ttc` = MimeType(
      "application/vnd.motorola.flexsuite.ttc")
  val `application/vnd.motorola.flexsuite.wem` = MimeType(
      "application/vnd.motorola.flexsuite.wem")
  val `application/vnd.motorola.iprm` = MimeType(
      "application/vnd.motorola.iprm")
  val `application/vnd.mozilla.xul+xml` = MimeType(
      "application/vnd.mozilla.xul+xml", "xul")
  val `application/vnd.ms-artgalry` = MimeType(
      "application/vnd.ms-artgalry", "cil")
  val `application/vnd.ms-asf` = MimeType("application/vnd.ms-asf")
  val `application/vnd.ms-cab-compressed` = MimeType(
      "application/vnd.ms-cab-compressed", "cab")

  val `application/vnd.ms-excel` = MimeType("application/vnd.ms-excel",
                                            "xla",
                                            "xlb",
                                            "xlc",
                                            "xlm",
                                            "xls",
                                            "xlt",
                                            "xlw")

  val `application/vnd.ms-excel.addin.macroenabled.12` = MimeType(
      "application/vnd.ms-excel.addin.macroenabled.12", "xlam")

  val `application/vnd.ms-excel.sheet.binary.macroenabled.12` = MimeType(
      "application/vnd.ms-excel.sheet.binary.macroenabled.12", "xlsb")

  val `application/vnd.ms-excel.sheet.macroenabled.12` = MimeType(
      "application/vnd.ms-excel.sheet.macroenabled.12", "xlsm")

  val `application/vnd.ms-excel.template.macroenabled.12` = MimeType(
      "application/vnd.ms-excel.template.macroenabled.12", "xltm")

  val `application/vnd.ms-fontobject` = MimeType(
      "application/vnd.ms-fontobject", "eot")
  val `application/vnd.ms-htmlhelp` = MimeType(
      "application/vnd.ms-htmlhelp", "chm")
  val `application/vnd.ms-ims` = MimeType("application/vnd.ms-ims", "ims")
  val `application/vnd.ms-lrm` = MimeType("application/vnd.ms-lrm", "lrm")
  val `application/vnd.ms-pki.seccat` = MimeType(
      "application/vnd.ms-pki.seccat", "cat")
  val `application/vnd.ms-pki.stl` = MimeType(
      "application/vnd.ms-pki.stl", "stl")
  val `application/vnd.ms-playready.initiator+xml` = MimeType(
      "application/vnd.ms-playready.initiator+xml")
  val `application/vnd.ms-powerpoint` = MimeType(
      "application/vnd.ms-powerpoint", "pot", "pps", "ppt")

  val `application/vnd.ms-powerpoint.addin.macroenabled.12` = MimeType(
      "application/vnd.ms-powerpoint.addin.macroenabled.12", "ppam")

  val `application/vnd.ms-powerpoint.presentation.macroenabled.12` = MimeType(
      "application/vnd.ms-powerpoint.presentation.macroenabled.12", "pptm")

  val `application/vnd.ms-powerpoint.slide.macroenabled.12` = MimeType(
      "application/vnd.ms-powerpoint.slide.macroenabled.12", "sldm")

  val `application/vnd.ms-powerpoint.slideshow.macroenabled.12` = MimeType(
      "application/vnd.ms-powerpoint.slideshow.macroenabled.12", "ppsm")

  val `application/vnd.ms-powerpoint.template.macroenabled.12` = MimeType(
      "application/vnd.ms-powerpoint.template.macroenabled.12", "potm")

  val `application/vnd.ms-project` = MimeType(
      "application/vnd.ms-project", "mpp", "mpt")
  val `application/vnd.ms-tnef` = MimeType("application/vnd.ms-tnef")
  val `application/vnd.ms-wmdrm.lic-chlg-req` = MimeType(
      "application/vnd.ms-wmdrm.lic-chlg-req")
  val `application/vnd.ms-wmdrm.lic-resp` = MimeType(
      "application/vnd.ms-wmdrm.lic-resp")
  val `application/vnd.ms-wmdrm.meter-chlg-req` = MimeType(
      "application/vnd.ms-wmdrm.meter-chlg-req")
  val `application/vnd.ms-wmdrm.meter-resp` = MimeType(
      "application/vnd.ms-wmdrm.meter-resp")

  val `application/vnd.ms-word.document.macroenabled.12` = MimeType(
      "application/vnd.ms-word.document.macroenabled.12", "docm")

  val `application/vnd.ms-word.template.macroenabled.12` = MimeType(
      "application/vnd.ms-word.template.macroenabled.12", "dotm")

  val `application/vnd.ms-works` = MimeType(
      "application/vnd.ms-works", "wcm", "wdb", "wks", "wps")
  val `application/vnd.ms-wpl` = MimeType("application/vnd.ms-wpl", "wpl")
  val `application/vnd.ms-xpsdocument` = MimeType(
      "application/vnd.ms-xpsdocument", "xps")
  val `application/vnd.mseq` = MimeType("application/vnd.mseq", "mseq")
  val `application/vnd.msign` = MimeType("application/vnd.msign")
  val `application/vnd.multiad.creator` = MimeType(
      "application/vnd.multiad.creator")
  val `application/vnd.multiad.creator.cif` = MimeType(
      "application/vnd.multiad.creator.cif")
  val `application/vnd.music-niff` = MimeType("application/vnd.music-niff")
  val `application/vnd.musician` = MimeType("application/vnd.musician", "mus")
  val `application/vnd.muvee.style` = MimeType(
      "application/vnd.muvee.style", "msty")
  val `application/vnd.ncd.control` = MimeType("application/vnd.ncd.control")
  val `application/vnd.ncd.reference` = MimeType(
      "application/vnd.ncd.reference")
  val `application/vnd.nervana` = MimeType("application/vnd.nervana")
  val `application/vnd.netfpx` = MimeType("application/vnd.netfpx")
  val `application/vnd.neurolanguage.nlu` = MimeType(
      "application/vnd.neurolanguage.nlu", "nlu")
  val `application/vnd.noblenet-directory` = MimeType(
      "application/vnd.noblenet-directory", "nnd")
  val `application/vnd.noblenet-sealer` = MimeType(
      "application/vnd.noblenet-sealer", "nns")
  val `application/vnd.noblenet-web` = MimeType(
      "application/vnd.noblenet-web", "nnw")
  val `application/vnd.nokia.catalogs` = MimeType(
      "application/vnd.nokia.catalogs")
  val `application/vnd.nokia.conml+wbxml` = MimeType(
      "application/vnd.nokia.conml+wbxml")
  val `application/vnd.nokia.conml+xml` = MimeType(
      "application/vnd.nokia.conml+xml")
  val `application/vnd.nokia.iptv.config+xml` = MimeType(
      "application/vnd.nokia.iptv.config+xml")
  val `application/vnd.nokia.isds-radio-presets` = MimeType(
      "application/vnd.nokia.isds-radio-presets")
  val `application/vnd.nokia.landmark+wbxml` = MimeType(
      "application/vnd.nokia.landmark+wbxml")
  val `application/vnd.nokia.landmark+xml` = MimeType(
      "application/vnd.nokia.landmark+xml")
  val `application/vnd.nokia.landmarkcollection+xml` = MimeType(
      "application/vnd.nokia.landmarkcollection+xml")
  val `application/vnd.nokia.n-gage.ac+xml` = MimeType(
      "application/vnd.nokia.n-gage.ac+xml")
  val `application/vnd.nokia.n-gage.data` = MimeType(
      "application/vnd.nokia.n-gage.data", "ngdat")

  val `application/vnd.nokia.n-gage.symbian.install` = MimeType(
      "application/vnd.nokia.n-gage.symbian.install", "n", "-gage")

  val `application/vnd.nokia.ncd` = MimeType("application/vnd.nokia.ncd")
  val `application/vnd.nokia.pcd+wbxml` = MimeType(
      "application/vnd.nokia.pcd+wbxml")
  val `application/vnd.nokia.pcd+xml` = MimeType(
      "application/vnd.nokia.pcd+xml")
  val `application/vnd.nokia.radio-preset` = MimeType(
      "application/vnd.nokia.radio-preset", "rpst")
  val `application/vnd.nokia.radio-presets` = MimeType(
      "application/vnd.nokia.radio-presets", "rpss")
  val `application/vnd.novadigm.edm` = MimeType(
      "application/vnd.novadigm.edm", "edm")
  val `application/vnd.novadigm.edx` = MimeType(
      "application/vnd.novadigm.edx", "edx")
  val `application/vnd.novadigm.ext` = MimeType(
      "application/vnd.novadigm.ext", "ext")
  val `application/vnd.oasis.opendocument.chart` = MimeType(
      "application/vnd.oasis.opendocument.chart", "odc")

  val `application/vnd.oasis.opendocument.chart-template` = MimeType(
      "application/vnd.oasis.opendocument.chart-template", "otc")

  val `application/vnd.oasis.opendocument.database` = MimeType(
      "application/vnd.oasis.opendocument.database", "odb")
  val `application/vnd.oasis.opendocument.formula` = MimeType(
      "application/vnd.oasis.opendocument.formula", "odf")

  val `application/vnd.oasis.opendocument.formula-template` = MimeType(
      "application/vnd.oasis.opendocument.formula-template", "odft")

  val `application/vnd.oasis.opendocument.graphics` = MimeType(
      "application/vnd.oasis.opendocument.graphics", "odg")

  val `application/vnd.oasis.opendocument.graphics-template` = MimeType(
      "application/vnd.oasis.opendocument.graphics-template", "otg")

  val `application/vnd.oasis.opendocument.image` = MimeType(
      "application/vnd.oasis.opendocument.image", "odi")

  val `application/vnd.oasis.opendocument.image-template` = MimeType(
      "application/vnd.oasis.opendocument.image-template", "oti")

  val `application/vnd.oasis.opendocument.presentation` = MimeType(
      "application/vnd.oasis.opendocument.presentation", "odp")

  val `application/vnd.oasis.opendocument.presentation-template` = MimeType(
      "application/vnd.oasis.opendocument.presentation-template", "otp")

  val `application/vnd.oasis.opendocument.spreadsheet` = MimeType(
      "application/vnd.oasis.opendocument.spreadsheet", "ods")

  val `application/vnd.oasis.opendocument.spreadsheet-template` = MimeType(
      "application/vnd.oasis.opendocument.spreadsheet-template", "ots")

  val `application/vnd.oasis.opendocument.text` = MimeType(
      "application/vnd.oasis.opendocument.text", "odt")

  val `application/vnd.oasis.opendocument.text-master` = MimeType(
      "application/vnd.oasis.opendocument.text-master", "odm", "otm")

  val `application/vnd.oasis.opendocument.text-template` = MimeType(
      "application/vnd.oasis.opendocument.text-template", "ott")

  val `application/vnd.oasis.opendocument.text-web` = MimeType(
      "application/vnd.oasis.opendocument.text-web", "oth")
  val `application/vnd.obn` = MimeType("application/vnd.obn")
  val `application/vnd.olpc-sugar` = MimeType(
      "application/vnd.olpc-sugar", "xo")
  val `application/vnd.oma-scws-config` = MimeType(
      "application/vnd.oma-scws-config")
  val `application/vnd.oma-scws-http-request` = MimeType(
      "application/vnd.oma-scws-http-request")
  val `application/vnd.oma-scws-http-response` = MimeType(
      "application/vnd.oma-scws-http-response")

  val `application/vnd.oma.bcast.associated-procedure-parameter+xml` =
    MimeType("application/vnd.oma.bcast.associated-procedure-parameter+xml")

  val `application/vnd.oma.bcast.drm-trigger+xml` = MimeType(
      "application/vnd.oma.bcast.drm-trigger+xml")
  val `application/vnd.oma.bcast.imd+xml` = MimeType(
      "application/vnd.oma.bcast.imd+xml")
  val `application/vnd.oma.bcast.ltkm` = MimeType(
      "application/vnd.oma.bcast.ltkm")
  val `application/vnd.oma.bcast.notification+xml` = MimeType(
      "application/vnd.oma.bcast.notification+xml")
  val `application/vnd.oma.bcast.provisioningtrigger` = MimeType(
      "application/vnd.oma.bcast.provisioningtrigger")
  val `application/vnd.oma.bcast.sgboot` = MimeType(
      "application/vnd.oma.bcast.sgboot")
  val `application/vnd.oma.bcast.sgdd+xml` = MimeType(
      "application/vnd.oma.bcast.sgdd+xml")
  val `application/vnd.oma.bcast.sgdu` = MimeType(
      "application/vnd.oma.bcast.sgdu")

  val `application/vnd.oma.bcast.simple-symbol-container` = MimeType(
      "application/vnd.oma.bcast.simple-symbol-container")

  val `application/vnd.oma.bcast.smartcard-trigger+xml` = MimeType(
      "application/vnd.oma.bcast.smartcard-trigger+xml")

  val `application/vnd.oma.bcast.sprov+xml` = MimeType(
      "application/vnd.oma.bcast.sprov+xml")
  val `application/vnd.oma.bcast.stkm` = MimeType(
      "application/vnd.oma.bcast.stkm")
  val `application/vnd.oma.dcd` = MimeType("application/vnd.oma.dcd")
  val `application/vnd.oma.dcdc` = MimeType("application/vnd.oma.dcdc")
  val `application/vnd.oma.dd2+xml` = MimeType(
      "application/vnd.oma.dd2+xml", "dd2")
  val `application/vnd.oma.drm.risd+xml` = MimeType(
      "application/vnd.oma.drm.risd+xml")
  val `application/vnd.oma.group-usage-list+xml` = MimeType(
      "application/vnd.oma.group-usage-list+xml")

  val `application/vnd.oma.poc.detailed-progress-report+xml` = MimeType(
      "application/vnd.oma.poc.detailed-progress-report+xml")

  val `application/vnd.oma.poc.final-report+xml` = MimeType(
      "application/vnd.oma.poc.final-report+xml")
  val `application/vnd.oma.poc.groups+xml` = MimeType(
      "application/vnd.oma.poc.groups+xml")

  val `application/vnd.oma.poc.invocation-descriptor+xml` = MimeType(
      "application/vnd.oma.poc.invocation-descriptor+xml")

  val `application/vnd.oma.poc.optimized-progress-report+xml` = MimeType(
      "application/vnd.oma.poc.optimized-progress-report+xml")

  val `application/vnd.oma.xcap-directory+xml` = MimeType(
      "application/vnd.oma.xcap-directory+xml")
  val `application/vnd.omads-email+xml` = MimeType(
      "application/vnd.omads-email+xml")
  val `application/vnd.omads-file+xml` = MimeType(
      "application/vnd.omads-file+xml")
  val `application/vnd.omads-folder+xml` = MimeType(
      "application/vnd.omads-folder+xml")
  val `application/vnd.omaloc-supl-init` = MimeType(
      "application/vnd.omaloc-supl-init")
  val `application/vnd.openofficeorg.extension` = MimeType(
      "application/vnd.openofficeorg.extension", "oxt")

  val `application/vnd.openxmlformats-officedocument.presentationml.presentation` =
    MimeType(
        "application/vnd.openxmlformats-officedocument.presentationml.presentation",
        "pptx")

  val `application/vnd.openxmlformats-officedocument.presentationml.slide` =
    MimeType(
        "application/vnd.openxmlformats-officedocument.presentationml.slide",
        "sldx")

  val `application/vnd.openxmlformats-officedocument.presentationml.slideshow` =
    MimeType(
        "application/vnd.openxmlformats-officedocument.presentationml.slideshow",
        "ppsx")

  val `application/vnd.openxmlformats-officedocument.presentationml.template` =
    MimeType(
        "application/vnd.openxmlformats-officedocument.presentationml.template",
        "potx")

  val `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet` =
    MimeType(
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        "xlsx")

  val `application/vnd.openxmlformats-officedocument.spreadsheetml.template` =
    MimeType(
        "application/vnd.openxmlformats-officedocument.spreadsheetml.template",
        "xltx")

  val `application/vnd.openxmlformats-officedocument.wordprocessingml.document` =
    MimeType(
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "docx")

  val `application/vnd.openxmlformats-officedocument.wordprocessingml.template` =
    MimeType(
        "application/vnd.openxmlformats-officedocument.wordprocessingml.template",
        "dotx")

  val `application/vnd.osa.netdeploy` = MimeType(
      "application/vnd.osa.netdeploy")
  val `application/vnd.osgi.bundle` = MimeType("application/vnd.osgi.bundle")
  val `application/vnd.osgi.dp` = MimeType("application/vnd.osgi.dp", "dp")
  val `application/vnd.otps.ct-kip+xml` = MimeType(
      "application/vnd.otps.ct-kip+xml")
  val `application/vnd.palm` = MimeType(
      "application/vnd.palm", "oprc", "pdb", "pqa")
  val `application/vnd.paos.xml` = MimeType("application/vnd.paos.xml")
  val `application/vnd.pg.format` = MimeType(
      "application/vnd.pg.format", "str")
  val `application/vnd.pg.osasli` = MimeType(
      "application/vnd.pg.osasli", "ei6")
  val `application/vnd.piaccess.application-licence` = MimeType(
      "application/vnd.piaccess.application-licence")
  val `application/vnd.picsel` = MimeType("application/vnd.picsel", "efif")
  val `application/vnd.poc.group-advertisement+xml` = MimeType(
      "application/vnd.poc.group-advertisement+xml")
  val `application/vnd.pocketlearn` = MimeType(
      "application/vnd.pocketlearn", "plf")
  val `application/vnd.powerbuilder6` = MimeType(
      "application/vnd.powerbuilder6", "pbd")
  val `application/vnd.powerbuilder6-s` = MimeType(
      "application/vnd.powerbuilder6-s")
  val `application/vnd.powerbuilder7` = MimeType(
      "application/vnd.powerbuilder7")
  val `application/vnd.powerbuilder7-s` = MimeType(
      "application/vnd.powerbuilder7-s")
  val `application/vnd.powerbuilder75` = MimeType(
      "application/vnd.powerbuilder75")
  val `application/vnd.powerbuilder75-s` = MimeType(
      "application/vnd.powerbuilder75-s")
  val `application/vnd.preminet` = MimeType("application/vnd.preminet")
  val `application/vnd.previewsystems.box` = MimeType(
      "application/vnd.previewsystems.box", "box")
  val `application/vnd.proteus.magazine` = MimeType(
      "application/vnd.proteus.magazine", "mgz")
  val `application/vnd.publishare-delta-tree` = MimeType(
      "application/vnd.publishare-delta-tree", "qps")
  val `application/vnd.pvi.ptid1` = MimeType(
      "application/vnd.pvi.ptid1", "ptid")
  val `application/vnd.pwg-multiplexed` = MimeType(
      "application/vnd.pwg-multiplexed")
  val `application/vnd.pwg-xhtml-print+xml` = MimeType(
      "application/vnd.pwg-xhtml-print+xml")
  val `application/vnd.qualcomm.brew-app-res` = MimeType(
      "application/vnd.qualcomm.brew-app-res")

  val `application/vnd.quark.quarkxpress` = MimeType(
      "application/vnd.quark.quarkxpress",
      "qwd",
      "qwt",
      "qxb",
      "qxd",
      "qxl",
      "qxt")

  val `application/vnd.rapid` = MimeType("application/vnd.rapid")
  val `application/vnd.recordare.musicxml` = MimeType(
      "application/vnd.recordare.musicxml", "mxl")
  val `application/vnd.recordare.musicxml+xml` = MimeType(
      "application/vnd.recordare.musicxml+xml", "musicxml")
  val `application/vnd.renlearn.rlprint` = MimeType(
      "application/vnd.renlearn.rlprint")
  val `application/vnd.rim.cod` = MimeType("application/vnd.rim.cod", "cod")
  val `application/vnd.rn-realmedia` = MimeType(
      "application/vnd.rn-realmedia", "rm")
  val `application/vnd.route66.link66+xml` = MimeType(
      "application/vnd.route66.link66+xml", "link66")
  val `application/vnd.ruckus.download` = MimeType(
      "application/vnd.ruckus.download")
  val `application/vnd.s3sms` = MimeType("application/vnd.s3sms")
  val `application/vnd.sbm.cid` = MimeType("application/vnd.sbm.cid")
  val `application/vnd.sbm.mid2` = MimeType("application/vnd.sbm.mid2")
  val `application/vnd.scribus` = MimeType("application/vnd.scribus")
  val `application/vnd.sealed.3df` = MimeType("application/vnd.sealed.3df")
  val `application/vnd.sealed.csf` = MimeType("application/vnd.sealed.csf")
  val `application/vnd.sealed.doc` = MimeType("application/vnd.sealed.doc")
  val `application/vnd.sealed.eml` = MimeType("application/vnd.sealed.eml")
  val `application/vnd.sealed.mht` = MimeType("application/vnd.sealed.mht")
  val `application/vnd.sealed.net` = MimeType("application/vnd.sealed.net")
  val `application/vnd.sealed.ppt` = MimeType("application/vnd.sealed.ppt")
  val `application/vnd.sealed.tiff` = MimeType("application/vnd.sealed.tiff")
  val `application/vnd.sealed.xls` = MimeType("application/vnd.sealed.xls")
  val `application/vnd.sealedmedia.softseal.html` = MimeType(
      "application/vnd.sealedmedia.softseal.html")
  val `application/vnd.sealedmedia.softseal.pdf` = MimeType(
      "application/vnd.sealedmedia.softseal.pdf")
  val `application/vnd.seemail` = MimeType("application/vnd.seemail", "see")
  val `application/vnd.sema` = MimeType("application/vnd.sema", "sema")
  val `application/vnd.semd` = MimeType("application/vnd.semd", "semd")
  val `application/vnd.semf` = MimeType("application/vnd.semf", "semf")
  val `application/vnd.shana.informed.formdata` = MimeType(
      "application/vnd.shana.informed.formdata", "ifm")
  val `application/vnd.shana.informed.formtemplate` = MimeType(
      "application/vnd.shana.informed.formtemplate", "itp")
  val `application/vnd.shana.informed.interchange` = MimeType(
      "application/vnd.shana.informed.interchange", "iif")
  val `application/vnd.shana.informed.package` = MimeType(
      "application/vnd.shana.informed.package", "ipk")
  val `application/vnd.simtech-mindmapper` = MimeType(
      "application/vnd.simtech-mindmapper", "twd", "twds")
  val `application/vnd.smaf` = MimeType("application/vnd.smaf", "mmf")
  val `application/vnd.smart.teacher` = MimeType(
      "application/vnd.smart.teacher", "teacher")
  val `application/vnd.software602.filler.form+xml` = MimeType(
      "application/vnd.software602.filler.form+xml")

  val `application/vnd.software602.filler.form-xml-zip` = MimeType(
      "application/vnd.software602.filler.form-xml-zip")

  val `application/vnd.solent.sdkm+xml` = MimeType(
      "application/vnd.solent.sdkm+xml", "sdkd", "sdkm")
  val `application/vnd.spotfire.dxp` = MimeType(
      "application/vnd.spotfire.dxp", "dxp")
  val `application/vnd.spotfire.sfs` = MimeType(
      "application/vnd.spotfire.sfs", "sfs")
  val `application/vnd.sss-cod` = MimeType("application/vnd.sss-cod")
  val `application/vnd.sss-dtf` = MimeType("application/vnd.sss-dtf")
  val `application/vnd.sss-ntf` = MimeType("application/vnd.sss-ntf")
  val `application/vnd.stardivision.calc` = MimeType(
      "application/vnd.stardivision.calc", "sdc")
  val `application/vnd.stardivision.draw` = MimeType(
      "application/vnd.stardivision.draw", "sda")
  val `application/vnd.stardivision.impress` = MimeType(
      "application/vnd.stardivision.impress", "sdd", "sdp")
  val `application/vnd.stardivision.math` = MimeType(
      "application/vnd.stardivision.math", "sdf", "smf")
  val `application/vnd.stardivision.writer` = MimeType(
      "application/vnd.stardivision.writer", "sdw", "vor")
  val `application/vnd.stardivision.writer-global` = MimeType(
      "application/vnd.stardivision.writer-global", "sgl")
  val `application/vnd.street-stream` = MimeType(
      "application/vnd.street-stream")
  val `application/vnd.sun.wadl+xml` = MimeType("application/vnd.sun.wadl+xml")
  val `application/vnd.sun.xml.calc` = MimeType(
      "application/vnd.sun.xml.calc", "sxc")
  val `application/vnd.sun.xml.calc.template` = MimeType(
      "application/vnd.sun.xml.calc.template", "stc")
  val `application/vnd.sun.xml.draw` = MimeType(
      "application/vnd.sun.xml.draw", "sxd")
  val `application/vnd.sun.xml.draw.template` = MimeType(
      "application/vnd.sun.xml.draw.template", "std")
  val `application/vnd.sun.xml.impress` = MimeType(
      "application/vnd.sun.xml.impress", "sxi")
  val `application/vnd.sun.xml.impress.template` = MimeType(
      "application/vnd.sun.xml.impress.template", "sti")
  val `application/vnd.sun.xml.math` = MimeType(
      "application/vnd.sun.xml.math", "sxm")
  val `application/vnd.sun.xml.writer` = MimeType(
      "application/vnd.sun.xml.writer", "sxw")
  val `application/vnd.sun.xml.writer.global` = MimeType(
      "application/vnd.sun.xml.writer.global", "sxg")
  val `application/vnd.sun.xml.writer.template` = MimeType(
      "application/vnd.sun.xml.writer.template", "stw")
  val `application/vnd.sus-calendar` = MimeType(
      "application/vnd.sus-calendar", "sus", "susp")
  val `application/vnd.svd` = MimeType("application/vnd.svd", "svd")
  val `application/vnd.swiftview-ics` = MimeType(
      "application/vnd.swiftview-ics")
  val `application/vnd.symbian.install` = MimeType(
      "application/vnd.symbian.install", "sis", "sisx")
  val `application/vnd.syncml+xml` = MimeType(
      "application/vnd.syncml+xml", "xsm")
  val `application/vnd.syncml.dm+wbxml` = MimeType(
      "application/vnd.syncml.dm+wbxml", "bdm")
  val `application/vnd.syncml.dm+xml` = MimeType(
      "application/vnd.syncml.dm+xml", "xdm")
  val `application/vnd.syncml.dm.notification` = MimeType(
      "application/vnd.syncml.dm.notification")
  val `application/vnd.syncml.ds.notification` = MimeType(
      "application/vnd.syncml.ds.notification")
  val `application/vnd.tao.intent-module-archive` = MimeType(
      "application/vnd.tao.intent-module-archive", "tao")
  val `application/vnd.tmobile-livetv` = MimeType(
      "application/vnd.tmobile-livetv", "tmo")
  val `application/vnd.trid.tpt` = MimeType("application/vnd.trid.tpt", "tpt")
  val `application/vnd.triscape.mxs` = MimeType(
      "application/vnd.triscape.mxs", "mxs")
  val `application/vnd.trueapp` = MimeType("application/vnd.trueapp", "tra")
  val `application/vnd.truedoc` = MimeType("application/vnd.truedoc")
  val `application/vnd.ufdl` = MimeType("application/vnd.ufdl", "ufd", "ufdl")
  val `application/vnd.uiq.theme` = MimeType(
      "application/vnd.uiq.theme", "utz")
  val `application/vnd.umajin` = MimeType("application/vnd.umajin", "umj")
  val `application/vnd.unity` = MimeType("application/vnd.unity", "unityweb")
  val `application/vnd.uoml+xml` = MimeType("application/vnd.uoml+xml", "uoml")
  val `application/vnd.uplanet.alert` = MimeType(
      "application/vnd.uplanet.alert")
  val `application/vnd.uplanet.alert-wbxml` = MimeType(
      "application/vnd.uplanet.alert-wbxml")
  val `application/vnd.uplanet.bearer-choice` = MimeType(
      "application/vnd.uplanet.bearer-choice")
  val `application/vnd.uplanet.bearer-choice-wbxml` = MimeType(
      "application/vnd.uplanet.bearer-choice-wbxml")
  val `application/vnd.uplanet.cacheop` = MimeType(
      "application/vnd.uplanet.cacheop")
  val `application/vnd.uplanet.cacheop-wbxml` = MimeType(
      "application/vnd.uplanet.cacheop-wbxml")
  val `application/vnd.uplanet.channel` = MimeType(
      "application/vnd.uplanet.channel")
  val `application/vnd.uplanet.channel-wbxml` = MimeType(
      "application/vnd.uplanet.channel-wbxml")
  val `application/vnd.uplanet.list` = MimeType("application/vnd.uplanet.list")
  val `application/vnd.uplanet.list-wbxml` = MimeType(
      "application/vnd.uplanet.list-wbxml")
  val `application/vnd.uplanet.listcmd` = MimeType(
      "application/vnd.uplanet.listcmd")
  val `application/vnd.uplanet.listcmd-wbxml` = MimeType(
      "application/vnd.uplanet.listcmd-wbxml")
  val `application/vnd.uplanet.signal` = MimeType(
      "application/vnd.uplanet.signal")
  val `application/vnd.vcx` = MimeType("application/vnd.vcx", "vcx")
  val `application/vnd.vd-study` = MimeType("application/vnd.vd-study")
  val `application/vnd.vectorworks` = MimeType("application/vnd.vectorworks")
  val `application/vnd.vidsoft.vidconference` = MimeType(
      "application/vnd.vidsoft.vidconference")
  val `application/vnd.visio` = MimeType(
      "application/vnd.visio", "vsd", "vss", "vst", "vsw")
  val `application/vnd.visionary` = MimeType(
      "application/vnd.visionary", "vis")
  val `application/vnd.vividence.scriptfile` = MimeType(
      "application/vnd.vividence.scriptfile")
  val `application/vnd.vsf` = MimeType("application/vnd.vsf", "vsf")
  val `application/vnd.wap.sic` = MimeType("application/vnd.wap.sic")
  val `application/vnd.wap.slc` = MimeType("application/vnd.wap.slc")
  val `application/vnd.wap.wbxml` = MimeType(
      "application/vnd.wap.wbxml", "wbxml")
  val `application/vnd.wap.wmlc` = MimeType("application/vnd.wap.wmlc", "wmlc")
  val `application/vnd.wap.wmlscriptc` = MimeType(
      "application/vnd.wap.wmlscriptc", "wmlsc")
  val `application/vnd.webturbo` = MimeType("application/vnd.webturbo", "wtb")
  val `application/vnd.wfa.wsc` = MimeType("application/vnd.wfa.wsc")
  val `application/vnd.wmc` = MimeType("application/vnd.wmc")
  val `application/vnd.wmf.bootstrap` = MimeType(
      "application/vnd.wmf.bootstrap")
  val `application/vnd.wordperfect` = MimeType(
      "application/vnd.wordperfect", "wpd")
  val `application/vnd.wqd` = MimeType("application/vnd.wqd", "wqd")
  val `application/vnd.wrq-hp3000-labelled` = MimeType(
      "application/vnd.wrq-hp3000-labelled")
  val `application/vnd.wt.stf` = MimeType("application/vnd.wt.stf", "stf")
  val `application/vnd.wv.csp+wbxml` = MimeType("application/vnd.wv.csp+wbxml")
  val `application/vnd.wv.csp+xml` = MimeType("application/vnd.wv.csp+xml")
  val `application/vnd.wv.ssp+xml` = MimeType("application/vnd.wv.ssp+xml")
  val `application/vnd.xara` = MimeType("application/vnd.xara", "xar")
  val `application/vnd.xfdl` = MimeType("application/vnd.xfdl", "xfdl")
  val `application/vnd.xfdl.webform` = MimeType("application/vnd.xfdl.webform")
  val `application/vnd.xmi+xml` = MimeType("application/vnd.xmi+xml")
  val `application/vnd.xmpie.cpkg` = MimeType("application/vnd.xmpie.cpkg")
  val `application/vnd.xmpie.dpkg` = MimeType("application/vnd.xmpie.dpkg")
  val `application/vnd.xmpie.plan` = MimeType("application/vnd.xmpie.plan")
  val `application/vnd.xmpie.ppkg` = MimeType("application/vnd.xmpie.ppkg")
  val `application/vnd.xmpie.xlim` = MimeType("application/vnd.xmpie.xlim")
  val `application/vnd.yamaha.hv-dic` = MimeType(
      "application/vnd.yamaha.hv-dic", "hvd")
  val `application/vnd.yamaha.hv-script` = MimeType(
      "application/vnd.yamaha.hv-script", "hvs")
  val `application/vnd.yamaha.hv-voice` = MimeType(
      "application/vnd.yamaha.hv-voice", "hvp")
  val `application/vnd.yamaha.openscoreformat` = MimeType(
      "application/vnd.yamaha.openscoreformat", "osf")

  val `application/vnd.yamaha.openscoreformat.osfpvg+xml` = MimeType(
      "application/vnd.yamaha.openscoreformat.osfpvg+xml", "osfpvg")

  val `application/vnd.yamaha.smaf-audio` = MimeType(
      "application/vnd.yamaha.smaf-audio", "saf")
  val `application/vnd.yamaha.smaf-phrase` = MimeType(
      "application/vnd.yamaha.smaf-phrase", "spf")
  val `application/vnd.yellowriver-custom-menu` = MimeType(
      "application/vnd.yellowriver-custom-menu", "cmp")
  val `application/vnd.zul` = MimeType("application/vnd.zul", "zir", "zirz")
  val `application/vnd.zzazz.deck+xml` = MimeType(
      "application/vnd.zzazz.deck+xml", "zaz")
  val `application/voicexml+xml` = MimeType("application/voicexml+xml", "vxml")
  val `application/watcherinfo+xml` = MimeType("application/watcherinfo+xml")
  val `application/whoispp-query` = MimeType("application/whoispp-query")
  val `application/whoispp-response` = MimeType("application/whoispp-response")
  val `application/winhlp` = MimeType("application/winhlp", "hlp")
  val `application/wita` = MimeType("application/wita")
  val `application/wordperfect` = MimeType("application/wordperfect", "wpd")
  val `application/wordperfect5.1` = MimeType(
      "application/wordperfect5.1", "wp5")
  val `application/wsdl+xml` = MimeType("application/wsdl+xml", "wsdl")
  val `application/wspolicy+xml` = MimeType(
      "application/wspolicy+xml", "wspolicy")
  val `application/x-123` = MimeType("application/x-123", "wk")
  val `application/x-abiword` = MimeType("application/x-abiword", "abw")
  val `application/x-ace-compressed` = MimeType(
      "application/x-ace-compressed", "ace")
  val `application/x-apple-diskimage` = MimeType(
      "application/x-apple-diskimage", "dmg")
  val `application/x-authorware-bin` = MimeType(
      "application/x-authorware-bin", "aab", "u32", "vox", "x32")
  val `application/x-authorware-map` = MimeType(
      "application/x-authorware-map", "aam")
  val `application/x-authorware-seg` = MimeType(
      "application/x-authorware-seg", "aas")
  val `application/x-bcpio` = MimeType("application/x-bcpio", "bcpio")
  val `application/x-bittorrent` = MimeType(
      "application/x-bittorrent", "torrent")
  val `application/x-bzip` = MimeType("application/x-bzip", "bz")
  val `application/x-bzip2` = MimeType("application/x-bzip2", "boz", "bz2")
  val `application/x-cdf` = MimeType("application/x-cdf", "cdf")
  val `application/x-cdlink` = MimeType("application/x-cdlink", "vcd")
  val `application/x-chat` = MimeType("application/x-chat", "chat")
  val `application/x-chess-pgn` = MimeType("application/x-chess-pgn", "pgn")
  val `application/x-compress` = MimeType("application/x-compress")
  val `application/x-cpio` = MimeType("application/x-cpio", "cpio")
  val `application/x-csh` = MimeType("application/x-csh", "csh")
  val `application/x-debian-package` = MimeType(
      "application/x-debian-package", "deb", "udeb")

  val `application/x-director` = MimeType("application/x-director",
                                          "cct",
                                          "cst",
                                          "cxt",
                                          "dcr",
                                          "dir",
                                          "dxr",
                                          "fgd",
                                          "swa",
                                          "w3d")

  val `application/x-dms` = MimeType("application/x-dms", "dms")
  val `application/x-doom` = MimeType("application/x-doom", "wad")
  val `application/x-dtbncx+xml` = MimeType("application/x-dtbncx+xml", "ncx")
  val `application/x-dtbook+xml` = MimeType("application/x-dtbook+xml", "dtb")
  val `application/x-dtbresource+xml` = MimeType(
      "application/x-dtbresource+xml", "res")
  val `application/x-dvi` = MimeType("application/x-dvi", "dvi")
  val `application/x-flac` = MimeType("application/x-flac", "flac")
  val `application/x-font` = MimeType(
      "application/x-font", "gsf", "pcf", "pcf", ".Z", "pfa", "pfb")
  val `application/x-font-bdf` = MimeType("application/x-font-bdf", "bdf")
  val `application/x-font-dos` = MimeType("application/x-font-dos")
  val `application/x-font-framemaker` = MimeType(
      "application/x-font-framemaker")
  val `application/x-font-ghostscript` = MimeType(
      "application/x-font-ghostscript", "gsf")
  val `application/x-font-libgrx` = MimeType("application/x-font-libgrx")
  val `application/x-font-linux-psf` = MimeType(
      "application/x-font-linux-psf", "psf")
  val `application/x-font-otf` = MimeType("application/x-font-otf", "otf")
  val `application/x-font-pcf` = MimeType("application/x-font-pcf", "pcf")
  val `application/x-font-snf` = MimeType("application/x-font-snf", "snf")
  val `application/x-font-speedo` = MimeType("application/x-font-speedo")
  val `application/x-font-sunos-news` = MimeType(
      "application/x-font-sunos-news")
  val `application/x-font-ttf` = MimeType(
      "application/x-font-ttf", "ttc", "ttf")
  val `application/x-font-type1` = MimeType(
      "application/x-font-type1", "afm", "pfa", "pfb", "pfm")
  val `application/x-font-vfont` = MimeType("application/x-font-vfont")
  val `application/x-freemind` = MimeType("application/x-freemind", "mm")
  val `application/x-futuresplash` = MimeType(
      "application/x-futuresplash", "spl")
  val `application/x-gnumeric` = MimeType("application/x-gnumeric", "gnumeric")
  val `application/x-go-sgf` = MimeType("application/x-go-sgf", "sgf")
  val `application/x-graphing-calculator` = MimeType(
      "application/x-graphing-calculator", "gcf")
  val `application/x-gtar` = MimeType(
      "application/x-gtar", "gtar", "taz", "tgz")
  val `application/x-gzip` = MimeType("application/x-gzip")
  val `application/x-hdf` = MimeType("application/x-hdf", "hdf")
  val `application/x-ica` = MimeType("application/x-ica", "ica")
  val `application/x-internet-signup` = MimeType(
      "application/x-internet-signup", "ins", "isp")
  val `application/x-iphone` = MimeType("application/x-iphone", "iii")
  val `application/x-iso9660-image` = MimeType(
      "application/x-iso9660-image", "iso")
  val `application/x-java-jnlp-file` = MimeType(
      "application/x-java-jnlp-file", "jnlp")
  val `application/x-javascript` = MimeType("application/x-javascript", "js")
  val `application/x-jmol` = MimeType("application/x-jmol", "jmz")
  val `application/x-kchart` = MimeType("application/x-kchart", "chrt")
  val `application/x-killustrator` = MimeType(
      "application/x-killustrator", "kil")
  val `application/x-koan` = MimeType(
      "application/x-koan", "skd", "skm", "skp", "skt")
  val `application/x-kpresenter` = MimeType(
      "application/x-kpresenter", "kpr", "kpt")
  val `application/x-kspread` = MimeType("application/x-kspread", "ksp")
  val `application/x-kword` = MimeType("application/x-kword", "kwd", "kwt")
  val `application/x-latex` = MimeType("application/x-latex", "latex")
  val `application/x-lha` = MimeType("application/x-lha", "lha")
  val `application/x-lzh` = MimeType("application/x-lzh", "lzh")
  val `application/x-lzx` = MimeType("application/x-lzx", "lzx")
  val `application/x-maker` = MimeType("application/x-maker",
                                       "book",
                                       "fb",
                                       "fbdoc",
                                       "fm",
                                       "frame",
                                       "frm",
                                       "maker")
  val `application/x-mif` = MimeType("application/x-mif", "mif")
  val `application/x-mobipocket-ebook` = MimeType(
      "application/x-mobipocket-ebook", "mobi", "prc")
  val `application/x-ms-application` = MimeType(
      "application/x-ms-application", "application")
  val `application/x-ms-wmd` = MimeType("application/x-ms-wmd", "wmd")
  val `application/x-ms-wmz` = MimeType("application/x-ms-wmz", "wmz")
  val `application/x-ms-xbap` = MimeType("application/x-ms-xbap", "xbap")
  val `application/x-msaccess` = MimeType("application/x-msaccess", "mdb")
  val `application/x-msbinder` = MimeType("application/x-msbinder", "obd")
  val `application/x-mscardfile` = MimeType("application/x-mscardfile", "crd")
  val `application/x-msclip` = MimeType("application/x-msclip", "clp")
  val `application/x-msdos-program` = MimeType(
      "application/x-msdos-program", "bat", "com", "dll", "exe")
  val `application/x-msdownload` = MimeType(
      "application/x-msdownload", "bat", "com", "dll", "exe", "msi")
  val `application/x-msi` = MimeType("application/x-msi", "msi")
  val `application/x-msmediaview` = MimeType(
      "application/x-msmediaview", "m13", "m14", "mvb")
  val `application/x-msmetafile` = MimeType("application/x-msmetafile", "wmf")
  val `application/x-msmoney` = MimeType("application/x-msmoney", "mny")
  val `application/x-mspublisher` = MimeType(
      "application/x-mspublisher", "pub")
  val `application/x-msschedule` = MimeType("application/x-msschedule", "scd")
  val `application/x-msterminal` = MimeType("application/x-msterminal", "trm")
  val `application/x-mswrite` = MimeType("application/x-mswrite", "wri")
  val `application/x-netcdf` = MimeType("application/x-netcdf", "cdf", "nc")
  val `application/x-ns-proxy-autoconfig` = MimeType(
      "application/x-ns-proxy-autoconfig", "pac")
  val `application/x-nwc` = MimeType("application/x-nwc", "nwc")
  val `application/x-object` = MimeType("application/x-object", "o")
  val `application/x-oz-application` = MimeType(
      "application/x-oz-application", "oza")
  val `application/x-pkcs12` = MimeType("application/x-pkcs12", "p12", "pfx")
  val `application/x-pkcs7-certificates` = MimeType(
      "application/x-pkcs7-certificates", "p7b", "spc")
  val `application/x-pkcs7-certreqresp` = MimeType(
      "application/x-pkcs7-certreqresp", "p7r")
  val `application/x-pkcs7-crl` = MimeType("application/x-pkcs7-crl", "crl")
  val `application/x-python-code` = MimeType(
      "application/x-python-code", "pyc", "pyo")
  val `application/x-quicktimeplayer` = MimeType(
      "application/x-quicktimeplayer", "qtl")
  val `application/x-rar-compressed` = MimeType(
      "application/x-rar-compressed", "rar")
  val `application/x-redhat-package-manager` = MimeType(
      "application/x-redhat-package-manager", "rpm")
  val `application/x-sh` = MimeType("application/x-sh", "sh")
  val `application/x-shar` = MimeType("application/x-shar", "shar")
  val `application/x-shockwave-flash` = MimeType(
      "application/x-shockwave-flash", "swf", "swfl")
  val `application/x-silverlight-app` = MimeType(
      "application/x-silverlight-app", "xap")
  val `application/x-stuffit` = MimeType(
      "application/x-stuffit", "sit", "sitx")
  val `application/x-stuffitx` = MimeType("application/x-stuffitx", "sitx")
  val `application/x-sv4cpio` = MimeType("application/x-sv4cpio", "sv4cpio")
  val `application/x-sv4crc` = MimeType("application/x-sv4crc", "sv4crc")
  val `application/x-tar` = MimeType("application/x-tar", "tar")
  val `application/x-tcl` = MimeType("application/x-tcl", "tcl")
  val `application/x-tex` = MimeType("application/x-tex", "tex")
  val `application/x-tex-gf` = MimeType("application/x-tex-gf", "gf")
  val `application/x-tex-pk` = MimeType("application/x-tex-pk", "pk")
  val `application/x-tex-tfm` = MimeType("application/x-tex-tfm", "tfm")
  val `application/x-texinfo` = MimeType(
      "application/x-texinfo", "texi", "texinfo")
  val `application/x-trash` = MimeType(
      "application/x-trash", "%", "bak", "old", "sik", "~")
  val `application/x-troff` = MimeType(
      "application/x-troff", "roff", "t", "tr")
  val `application/x-troff-man` = MimeType("application/x-troff-man", "man")
  val `application/x-troff-me` = MimeType("application/x-troff-me", "me")
  val `application/x-troff-ms` = MimeType("application/x-troff-ms", "ms")
  val `application/x-ustar` = MimeType("application/x-ustar", "ustar")
  val `application/x-wais-source` = MimeType(
      "application/x-wais-source", "src")
  val `application/x-wingz` = MimeType("application/x-wingz", "wz")
  val `application/x-x509-ca-cert` = MimeType(
      "application/x-x509-ca-cert", "crt", "der")
  val `application/x-xcf` = MimeType("application/x-xcf", "xcf")
  val `application/x-xfig` = MimeType("application/x-xfig", "fig")
  val `application/x-xpinstall` = MimeType("application/x-xpinstall", "xpi")
  val `application/x400-bp` = MimeType("application/x400-bp")
  val `application/xcap-att+xml` = MimeType("application/xcap-att+xml")
  val `application/xcap-caps+xml` = MimeType("application/xcap-caps+xml")
  val `application/xcap-el+xml` = MimeType("application/xcap-el+xml")
  val `application/xcap-error+xml` = MimeType("application/xcap-error+xml")
  val `application/xcap-ns+xml` = MimeType("application/xcap-ns+xml")
  val `application/xcon-conference-info+xml` = MimeType(
      "application/xcon-conference-info+xml")
  val `application/xcon-conference-info-diff+xml` = MimeType(
      "application/xcon-conference-info-diff+xml")
  val `application/xenc+xml` = MimeType("application/xenc+xml", "xenc")
  val `application/xhtml+xml` = MimeType(
      "application/xhtml+xml", "xht", "xhtml")
  val `application/xhtml-voice+xml` = MimeType("application/xhtml-voice+xml")
  val `application/xml` = MimeType("application/xml", "xml", "xsl")
  val `application/xml-dtd` = MimeType("application/xml-dtd", "dtd")
  val `application/xml-external-parsed-entity` = MimeType(
      "application/xml-external-parsed-entity")
  val `application/xmpp+xml` = MimeType("application/xmpp+xml")
  val `application/xop+xml` = MimeType("application/xop+xml", "xop")
  val `application/xslt+xml` = MimeType("application/xslt+xml", "xslt")
  val `application/xspf+xml` = MimeType("application/xspf+xml", "xspf")
  val `application/xv+xml` = MimeType(
      "application/xv+xml", "mxml", "xhvml", "xvm", "xvml")
  val `application/zip` = MimeType("application/zip", "zip")
  val `audio/32kadpcm` = MimeType("audio/32kadpcm")
  val `audio/3gpp` = MimeType("audio/3gpp")
  val `audio/3gpp2` = MimeType("audio/3gpp2")
  val `audio/ac3` = MimeType("audio/ac3")
  val `audio/adpcm` = MimeType("audio/adpcm", "adp")
  val `audio/amr` = MimeType("audio/amr")
  val `audio/amr-wb` = MimeType("audio/amr-wb")
  val `audio/amr-wb+` = MimeType("audio/amr-wb+")
  val `audio/asc` = MimeType("audio/asc")
  val `audio/basic` = MimeType("audio/basic", "au", "snd")
  val `audio/bv16` = MimeType("audio/bv16")
  val `audio/bv32` = MimeType("audio/bv32")
  val `audio/clearmode` = MimeType("audio/clearmode")
  val `audio/cn` = MimeType("audio/cn")
  val `audio/dat12` = MimeType("audio/dat12")
  val `audio/dls` = MimeType("audio/dls")
  val `audio/dsr-es201108` = MimeType("audio/dsr-es201108")
  val `audio/dsr-es202050` = MimeType("audio/dsr-es202050")
  val `audio/dsr-es202211` = MimeType("audio/dsr-es202211")
  val `audio/dsr-es202212` = MimeType("audio/dsr-es202212")
  val `audio/dvi4` = MimeType("audio/dvi4")
  val `audio/eac3` = MimeType("audio/eac3")
  val `audio/evrc` = MimeType("audio/evrc")
  val `audio/evrc-qcp` = MimeType("audio/evrc-qcp")
  val `audio/evrc0` = MimeType("audio/evrc0")
  val `audio/evrc1` = MimeType("audio/evrc1")
  val `audio/evrcb` = MimeType("audio/evrcb")
  val `audio/evrcb0` = MimeType("audio/evrcb0")
  val `audio/evrcb1` = MimeType("audio/evrcb1")
  val `audio/evrcwb` = MimeType("audio/evrcwb")
  val `audio/evrcwb0` = MimeType("audio/evrcwb0")
  val `audio/evrcwb1` = MimeType("audio/evrcwb1")
  val `audio/example` = MimeType("audio/example")
  val `audio/g719` = MimeType("audio/g719")
  val `audio/g722` = MimeType("audio/g722")
  val `audio/g7221` = MimeType("audio/g7221")
  val `audio/g723` = MimeType("audio/g723")
  val `audio/g726-16` = MimeType("audio/g726-16")
  val `audio/g726-24` = MimeType("audio/g726-24")
  val `audio/g726-32` = MimeType("audio/g726-32")
  val `audio/g726-40` = MimeType("audio/g726-40")
  val `audio/g728` = MimeType("audio/g728")
  val `audio/g729` = MimeType("audio/g729")
  val `audio/g7291` = MimeType("audio/g7291")
  val `audio/g729d` = MimeType("audio/g729d")
  val `audio/g729e` = MimeType("audio/g729e")
  val `audio/gsm` = MimeType("audio/gsm")
  val `audio/gsm-efr` = MimeType("audio/gsm-efr")
  val `audio/ilbc` = MimeType("audio/ilbc")
  val `audio/l16` = MimeType("audio/l16")
  val `audio/l20` = MimeType("audio/l20")
  val `audio/l24` = MimeType("audio/l24")
  val `audio/l8` = MimeType("audio/l8")
  val `audio/lpc` = MimeType("audio/lpc")
  val `audio/midi` = MimeType("audio/midi", "kar", "mid", "midi", "rmi")
  val `audio/mobile-xmf` = MimeType("audio/mobile-xmf")
  val `audio/mp4` = MimeType("audio/mp4", "mp4a")
  val `audio/mp4a-latm` = MimeType("audio/mp4a-latm")
  val `audio/mpa` = MimeType("audio/mpa")
  val `audio/mpa-robust` = MimeType("audio/mpa-robust")
  val `audio/mpeg` = MimeType(
      "audio/mpeg", "m2a", "m3a", "m4a", "mp2", "mp2a", "mp3", "mpega", "mpga")
  val `audio/mpeg4-generic` = MimeType("audio/mpeg4-generic")
  val `audio/mpegurl` = MimeType("audio/mpegurl", "m3u")
  val `audio/ogg` = MimeType("audio/ogg", "oga", "ogg", "spx")
  val `audio/parityfec` = MimeType("audio/parityfec")
  val `audio/pcma` = MimeType("audio/pcma")
  val `audio/pcma-wb` = MimeType("audio/pcma-wb")
  val `audio/pcmu` = MimeType("audio/pcmu")
  val `audio/pcmu-wb` = MimeType("audio/pcmu-wb")
  val `audio/prs.sid` = MimeType("audio/prs.sid", "sid")
  val `audio/qcelp` = MimeType("audio/qcelp")
  val `audio/red` = MimeType("audio/red")
  val `audio/rtp-enc-aescm128` = MimeType("audio/rtp-enc-aescm128")
  val `audio/rtp-midi` = MimeType("audio/rtp-midi")
  val `audio/rtx` = MimeType("audio/rtx")
  val `audio/smv` = MimeType("audio/smv")
  val `audio/smv-qcp` = MimeType("audio/smv-qcp")
  val `audio/smv0` = MimeType("audio/smv0")
  val `audio/sp-midi` = MimeType("audio/sp-midi")
  val `audio/t140c` = MimeType("audio/t140c")
  val `audio/t38` = MimeType("audio/t38")
  val `audio/telephone-event` = MimeType("audio/telephone-event")
  val `audio/tone` = MimeType("audio/tone")
  val `audio/ulpfec` = MimeType("audio/ulpfec")
  val `audio/vdvi` = MimeType("audio/vdvi")
  val `audio/vmr-wb` = MimeType("audio/vmr-wb")
  val `audio/vnd.3gpp.iufp` = MimeType("audio/vnd.3gpp.iufp")
  val `audio/vnd.4sb` = MimeType("audio/vnd.4sb")
  val `audio/vnd.audiokoz` = MimeType("audio/vnd.audiokoz")
  val `audio/vnd.celp` = MimeType("audio/vnd.celp")
  val `audio/vnd.cisco.nse` = MimeType("audio/vnd.cisco.nse")
  val `audio/vnd.cmles.radio-events` = MimeType("audio/vnd.cmles.radio-events")
  val `audio/vnd.cns.anp1` = MimeType("audio/vnd.cns.anp1")
  val `audio/vnd.cns.inf1` = MimeType("audio/vnd.cns.inf1")
  val `audio/vnd.digital-winds` = MimeType("audio/vnd.digital-winds", "eol")
  val `audio/vnd.dlna.adts` = MimeType("audio/vnd.dlna.adts")
  val `audio/vnd.dolby.heaac.1` = MimeType("audio/vnd.dolby.heaac.1")
  val `audio/vnd.dolby.heaac.2` = MimeType("audio/vnd.dolby.heaac.2")
  val `audio/vnd.dolby.mlp` = MimeType("audio/vnd.dolby.mlp")
  val `audio/vnd.dolby.mps` = MimeType("audio/vnd.dolby.mps")
  val `audio/vnd.dolby.pl2` = MimeType("audio/vnd.dolby.pl2")
  val `audio/vnd.dolby.pl2x` = MimeType("audio/vnd.dolby.pl2x")
  val `audio/vnd.dolby.pl2z` = MimeType("audio/vnd.dolby.pl2z")
  val `audio/vnd.dts` = MimeType("audio/vnd.dts", "dts")
  val `audio/vnd.dts.hd` = MimeType("audio/vnd.dts.hd", "dtshd")
  val `audio/vnd.everad.plj` = MimeType("audio/vnd.everad.plj")
  val `audio/vnd.hns.audio` = MimeType("audio/vnd.hns.audio")
  val `audio/vnd.lucent.voice` = MimeType("audio/vnd.lucent.voice", "lvp")
  val `audio/vnd.ms-playready.media.pya` = MimeType(
      "audio/vnd.ms-playready.media.pya", "pya")
  val `audio/vnd.nokia.mobile-xmf` = MimeType("audio/vnd.nokia.mobile-xmf")
  val `audio/vnd.nortel.vbk` = MimeType("audio/vnd.nortel.vbk")
  val `audio/vnd.nuera.ecelp4800` = MimeType(
      "audio/vnd.nuera.ecelp4800", "ecelp4800")
  val `audio/vnd.nuera.ecelp7470` = MimeType(
      "audio/vnd.nuera.ecelp7470", "ecelp7470")
  val `audio/vnd.nuera.ecelp9600` = MimeType(
      "audio/vnd.nuera.ecelp9600", "ecelp9600")
  val `audio/vnd.octel.sbc` = MimeType("audio/vnd.octel.sbc")
  val `audio/vnd.qcelp` = MimeType("audio/vnd.qcelp")
  val `audio/vnd.rhetorex.32kadpcm` = MimeType("audio/vnd.rhetorex.32kadpcm")
  val `audio/vnd.sealedmedia.softseal.mpeg` = MimeType(
      "audio/vnd.sealedmedia.softseal.mpeg")
  val `audio/vnd.vmx.cvsd` = MimeType("audio/vnd.vmx.cvsd")
  val `audio/vorbis` = MimeType("audio/vorbis")
  val `audio/vorbis-config` = MimeType("audio/vorbis-config")
  val `audio/x-aac` = MimeType("audio/x-aac", "aac")
  val `audio/x-aiff` = MimeType("audio/x-aiff", "aif", "aifc", "aiff")
  val `audio/x-gsm` = MimeType("audio/x-gsm", "gsm")
  val `audio/x-mpegurl` = MimeType("audio/x-mpegurl", "m3u")
  val `audio/x-ms-wax` = MimeType("audio/x-ms-wax", "wax")
  val `audio/x-ms-wma` = MimeType("audio/x-ms-wma", "wma")
  val `audio/x-pn-realaudio` = MimeType(
      "audio/x-pn-realaudio", "ra", "ram", "rm")
  val `audio/x-pn-realaudio-plugin` = MimeType(
      "audio/x-pn-realaudio-plugin", "rmp")
  val `audio/x-realaudio` = MimeType("audio/x-realaudio", "ra")
  val `audio/x-scpls` = MimeType("audio/x-scpls", "pls")
  val `audio/x-sd2` = MimeType("audio/x-sd2", "sd2")
  val `audio/x-wav` = MimeType("audio/x-wav", "wav")
  val `chemical/x-alchemy` = MimeType("chemical/x-alchemy", "alc")
  val `chemical/x-cache` = MimeType("chemical/x-cache", "cac", "cache")
  val `chemical/x-cache-csf` = MimeType("chemical/x-cache-csf", "csf")
  val `chemical/x-cactvs-binary` = MimeType(
      "chemical/x-cactvs-binary", "cascii", "cbin", "ctab")
  val `chemical/x-cdx` = MimeType("chemical/x-cdx", "cdx")
  val `chemical/x-cerius` = MimeType("chemical/x-cerius", "cer")
  val `chemical/x-chem3d` = MimeType("chemical/x-chem3d", "c3d")
  val `chemical/x-chemdraw` = MimeType("chemical/x-chemdraw", "chm")
  val `chemical/x-cif` = MimeType("chemical/x-cif", "cif")
  val `chemical/x-cmdf` = MimeType("chemical/x-cmdf", "cmdf")
  val `chemical/x-cml` = MimeType("chemical/x-cml", "cml")
  val `chemical/x-compass` = MimeType("chemical/x-compass", "cpa")
  val `chemical/x-crossfire` = MimeType("chemical/x-crossfire", "bsd")
  val `chemical/x-csml` = MimeType("chemical/x-csml", "csm", "csml")
  val `chemical/x-ctx` = MimeType("chemical/x-ctx", "ctx")
  val `chemical/x-cxf` = MimeType("chemical/x-cxf", "cef", "cxf")
  val `chemical/x-embl-dl-nucleotide` = MimeType(
      "chemical/x-embl-dl-nucleotide", "emb", "embl")
  val `chemical/x-galactic-spc` = MimeType("chemical/x-galactic-spc", "spc")
  val `chemical/x-gamess-input` = MimeType(
      "chemical/x-gamess-input", "gam", "gamin", "inp")
  val `chemical/x-gaussian-checkpoint` = MimeType(
      "chemical/x-gaussian-checkpoint", "fch", "fchk")
  val `chemical/x-gaussian-cube` = MimeType("chemical/x-gaussian-cube", "cub")
  val `chemical/x-gaussian-input` = MimeType(
      "chemical/x-gaussian-input", "gau", "gjc", "gjf")
  val `chemical/x-gaussian-log` = MimeType("chemical/x-gaussian-log", "gal")
  val `chemical/x-gcg8-sequence` = MimeType("chemical/x-gcg8-sequence", "gcg")
  val `chemical/x-genbank` = MimeType("chemical/x-genbank", "gen")
  val `chemical/x-hin` = MimeType("chemical/x-hin", "hin")
  val `chemical/x-isostar` = MimeType("chemical/x-isostar", "ist", "istr")
  val `chemical/x-jcamp-dx` = MimeType("chemical/x-jcamp-dx", "dx", "jdx")
  val `chemical/x-kinemage` = MimeType("chemical/x-kinemage", "kin")
  val `chemical/x-macmolecule` = MimeType("chemical/x-macmolecule", "mcm")
  val `chemical/x-macromodel-input` = MimeType(
      "chemical/x-macromodel-input", "mmd", "mmod")
  val `chemical/x-mdl-molfile` = MimeType("chemical/x-mdl-molfile", "mol")
  val `chemical/x-mdl-rdfile` = MimeType("chemical/x-mdl-rdfile", "rd")
  val `chemical/x-mdl-rxnfile` = MimeType("chemical/x-mdl-rxnfile", "rxn")
  val `chemical/x-mdl-sdfile` = MimeType("chemical/x-mdl-sdfile", "sd", "sdf")
  val `chemical/x-mdl-tgf` = MimeType("chemical/x-mdl-tgf", "tgf")
  val `chemical/x-mmcif` = MimeType("chemical/x-mmcif", "mcif")
  val `chemical/x-mol2` = MimeType("chemical/x-mol2", "mol2")
  val `chemical/x-molconn-Z` = MimeType("chemical/x-molconn-Z", "b")
  val `chemical/x-mopac-graph` = MimeType("chemical/x-mopac-graph", "gpt")
  val `chemical/x-mopac-input` = MimeType(
      "chemical/x-mopac-input", "dat", "mop", "mopcrt", "mpc", "zmt")
  val `chemical/x-mopac-out` = MimeType("chemical/x-mopac-out", "moo")
  val `chemical/x-mopac-vib` = MimeType("chemical/x-mopac-vib", "mvb")
  val `chemical/x-ncbi-asn1` = MimeType("chemical/x-ncbi-asn1", "asn")
  val `chemical/x-ncbi-asn1-ascii` = MimeType(
      "chemical/x-ncbi-asn1-ascii", "ent", "prt")
  val `chemical/x-ncbi-asn1-binary` = MimeType(
      "chemical/x-ncbi-asn1-binary", "aso", "val")
  val `chemical/x-ncbi-asn1-spec` = MimeType(
      "chemical/x-ncbi-asn1-spec", "asn")
  val `chemical/x-pdb` = MimeType("chemical/x-pdb", "ent", "pdb")
  val `chemical/x-rosdal` = MimeType("chemical/x-rosdal", "ros")
  val `chemical/x-swissprot` = MimeType("chemical/x-swissprot", "sw")
  val `chemical/x-vamas-iso14976` = MimeType(
      "chemical/x-vamas-iso14976", "vms")
  val `chemical/x-vmd` = MimeType("chemical/x-vmd", "vmd")
  val `chemical/x-xtel` = MimeType("chemical/x-xtel", "xtel")
  val `chemical/x-xyz` = MimeType("chemical/x-xyz", "xyz")
  val `image/bmp` = MimeType("image/bmp", "bmp")
  val `image/cgm` = MimeType("image/cgm", "cgm")
  val `image/example` = MimeType("image/example")
  val `image/fits` = MimeType("image/fits")
  val `image/g3fax` = MimeType("image/g3fax", "g3")
  val `image/gif` = MimeType("image/gif", "gif")
  val `image/ief` = MimeType("image/ief", "ief")
  val `image/jp2` = MimeType("image/jp2")
  val `image/jpeg` = MimeType("image/jpeg", "jpe", "jpeg", "jpg")
  val `image/jpm` = MimeType("image/jpm")
  val `image/jpx` = MimeType("image/jpx")
  val `image/naplps` = MimeType("image/naplps")
  val `image/pcx` = MimeType("image/pcx", "pcx")
  val `image/png` = MimeType("image/png", "png")
  val `image/prs.btif` = MimeType("image/prs.btif", "btif")
  val `image/prs.pti` = MimeType("image/prs.pti")
  val `image/svg+xml` = MimeType("image/svg+xml", "svg", "svgz")
  val `image/t38` = MimeType("image/t38")
  val `image/tiff` = MimeType("image/tiff", "tif", "tiff")
  val `image/tiff-fx` = MimeType("image/tiff-fx")
  val `image/vnd.adobe.photoshop` = MimeType(
      "image/vnd.adobe.photoshop", "psd")
  val `image/vnd.cns.inf2` = MimeType("image/vnd.cns.inf2")
  val `image/vnd.djvu` = MimeType("image/vnd.djvu", "djv", "djvu")
  val `image/vnd.dwg` = MimeType("image/vnd.dwg", "dwg")
  val `image/vnd.dxf` = MimeType("image/vnd.dxf", "dxf")
  val `image/vnd.fastbidsheet` = MimeType("image/vnd.fastbidsheet", "fbs")
  val `image/vnd.fpx` = MimeType("image/vnd.fpx", "fpx")
  val `image/vnd.fst` = MimeType("image/vnd.fst", "fst")
  val `image/vnd.fujixerox.edmics-mmr` = MimeType(
      "image/vnd.fujixerox.edmics-mmr", "mmr")
  val `image/vnd.fujixerox.edmics-rlc` = MimeType(
      "image/vnd.fujixerox.edmics-rlc", "rlc")
  val `image/vnd.globalgraphics.pgb` = MimeType("image/vnd.globalgraphics.pgb")
  val `image/vnd.microsoft.icon` = MimeType("image/vnd.microsoft.icon")
  val `image/vnd.mix` = MimeType("image/vnd.mix")
  val `image/vnd.ms-modi` = MimeType("image/vnd.ms-modi", "mdi")
  val `image/vnd.net-fpx` = MimeType("image/vnd.net-fpx", "npx")
  val `image/vnd.radiance` = MimeType("image/vnd.radiance")
  val `image/vnd.sealed.png` = MimeType("image/vnd.sealed.png")
  val `image/vnd.sealedmedia.softseal.gif` = MimeType(
      "image/vnd.sealedmedia.softseal.gif")
  val `image/vnd.sealedmedia.softseal.jpg` = MimeType(
      "image/vnd.sealedmedia.softseal.jpg")
  val `image/vnd.svf` = MimeType("image/vnd.svf")
  val `image/vnd.wap.wbmp` = MimeType("image/vnd.wap.wbmp", "wbmp")
  val `image/vnd.xiff` = MimeType("image/vnd.xiff", "xif")
  val `image/x-cmu-raster` = MimeType("image/x-cmu-raster", "ras")
  val `image/x-cmx` = MimeType("image/x-cmx", "cmx")
  val `image/x-coreldraw` = MimeType("image/x-coreldraw", "cdr")
  val `image/x-coreldrawpattern` = MimeType("image/x-coreldrawpattern", "pat")
  val `image/x-coreldrawtemplate` = MimeType(
      "image/x-coreldrawtemplate", "cdt")
  val `image/x-corelphotopaint` = MimeType("image/x-corelphotopaint", "cpt")
  val `image/x-freehand` = MimeType(
      "image/x-freehand", "fh", "fh4", "fh5", "fh7", "fhc")
  val `image/x-icon` = MimeType("image/x-icon", "ico")
  val `image/x-jg` = MimeType("image/x-jg", "art")
  val `image/x-jng` = MimeType("image/x-jng", "jng")
  val `image/x-ms-bmp` = MimeType("image/x-ms-bmp", "bmp")
  val `image/x-pcx` = MimeType("image/x-pcx", "pcx")
  val `image/x-photoshop` = MimeType("image/x-photoshop", "psd")
  val `image/x-pict` = MimeType("image/x-pict", "pct", "pic")
  val `image/x-portable-anymap` = MimeType("image/x-portable-anymap", "pnm")
  val `image/x-portable-bitmap` = MimeType("image/x-portable-bitmap", "pbm")
  val `image/x-portable-graymap` = MimeType("image/x-portable-graymap", "pgm")
  val `image/x-portable-pixmap` = MimeType("image/x-portable-pixmap", "ppm")
  val `image/x-rgb` = MimeType("image/x-rgb", "rgb")
  val `image/x-xbitmap` = MimeType("image/x-xbitmap", "xbm")
  val `image/x-xpixmap` = MimeType("image/x-xpixmap", "xpm")
  val `image/x-xwindowdump` = MimeType("image/x-xwindowdump", "xwd")
  val `message/cpim` = MimeType("message/cpim")
  val `message/delivery-status` = MimeType("message/delivery-status")
  val `message/disposition-notification` = MimeType(
      "message/disposition-notification")
  val `message/example` = MimeType("message/example")
  val `message/external-body` = MimeType("message/external-body")
  val `message/global` = MimeType("message/global")
  val `message/global-delivery-status` = MimeType(
      "message/global-delivery-status")
  val `message/global-disposition-notification` = MimeType(
      "message/global-disposition-notification")
  val `message/global-headers` = MimeType("message/global-headers")
  val `message/http` = MimeType("message/http")
  val `message/imdn+xml` = MimeType("message/imdn+xml")
  val `message/news` = MimeType("message/news")
  val `message/partial` = MimeType("message/partial")
  val `message/rfc822` = MimeType("message/rfc822", "eml", "mime")
  val `message/s-http` = MimeType("message/s-http")
  val `message/sip` = MimeType("message/sip")
  val `message/sipfrag` = MimeType("message/sipfrag")
  val `message/tracking-status` = MimeType("message/tracking-status")
  val `message/vnd.si.simp` = MimeType("message/vnd.si.simp")
  val `model/example` = MimeType("model/example")
  val `model/iges` = MimeType("model/iges", "iges", "igs")
  val `model/mesh` = MimeType("model/mesh", "mesh", "msh", "silo")
  val `model/vnd.dwf` = MimeType("model/vnd.dwf", "dwf")
  val `model/vnd.flatland.3dml` = MimeType("model/vnd.flatland.3dml")
  val `model/vnd.gdl` = MimeType("model/vnd.gdl", "gdl")
  val `model/vnd.gs-gdl` = MimeType("model/vnd.gs-gdl")
  val `model/vnd.gs.gdl` = MimeType("model/vnd.gs.gdl")
  val `model/vnd.gtw` = MimeType("model/vnd.gtw", "gtw")
  val `model/vnd.moml+xml` = MimeType("model/vnd.moml+xml")
  val `model/vnd.mts` = MimeType("model/vnd.mts", "mts")
  val `model/vnd.parasolid.transmit.binary` = MimeType(
      "model/vnd.parasolid.transmit.binary")
  val `model/vnd.parasolid.transmit.text` = MimeType(
      "model/vnd.parasolid.transmit.text")
  val `model/vnd.vtu` = MimeType("model/vnd.vtu", "vtu")
  val `model/vrml` = MimeType("model/vrml", "vrml", "wrl")
  val `multipart/alternative` = MimeType("multipart/alternative")
  val `multipart/appledouble` = MimeType("multipart/appledouble")
  val `multipart/byteranges` = MimeType("multipart/byteranges")
  val `multipart/digest` = MimeType("multipart/digest")
  val `multipart/encrypted` = MimeType("multipart/encrypted")
  val `multipart/example` = MimeType("multipart/example")
  val `multipart/form-data` = MimeType("multipart/form-data")
  val `multipart/header-set` = MimeType("multipart/header-set")
  val `multipart/mixed` = MimeType("multipart/mixed")
  val `multipart/parallel` = MimeType("multipart/parallel")
  val `multipart/related` = MimeType("multipart/related")
  val `multipart/report` = MimeType("multipart/report")
  val `multipart/signed` = MimeType("multipart/signed")
  val `multipart/voice-message` = MimeType("multipart/voice-message")
  val `text/calendar` = MimeType("text/calendar", "ics", "icz", "ifb")
  val `text/comma-separated-values` = MimeType(
      "text/comma-separated-values", "csv")
  val `text/css` = MimeType("text/css", "css")
  val `text/csv` = MimeType("text/csv", "csv")
  val `text/directory` = MimeType("text/directory")
  val `text/dns` = MimeType("text/dns")
  val `text/ecmascript` = MimeType("text/ecmascript")
  val `text/enriched` = MimeType("text/enriched")
  val `text/example` = MimeType("text/example")
  val `text/h323` = MimeType("text/h323", "323")
  val `text/html` = MimeType("text/html", "htm", "html", "shtml")
  val `text/iuls` = MimeType("text/iuls", "uls")
  val `text/javascript` = MimeType("text/javascript")
  val `text/mathml` = MimeType("text/mathml", "mml")
  val `text/parityfec` = MimeType("text/parityfec")
  val `text/prs.fallenstein.rst` = MimeType("text/prs.fallenstein.rst")
  val `text/prs.lines.tag` = MimeType("text/prs.lines.tag", "dsc")
  val `text/red` = MimeType("text/red")
  val `text/rfc822-headers` = MimeType("text/rfc822-headers")
  val `text/richtext` = MimeType("text/richtext", "rtx")
  val `text/rtf` = MimeType("text/rtf", "rtf")
  val `text/rtp-enc-aescm128` = MimeType("text/rtp-enc-aescm128")
  val `text/rtx` = MimeType("text/rtx")
  val `text/scriptlet` = MimeType("text/scriptlet", "sct", "wsc")
  val `text/sgml` = MimeType("text/sgml", "sgm", "sgml")
  val `text/t140` = MimeType("text/t140")
  val `text/tab-separated-values` = MimeType(
      "text/tab-separated-values", "tsv")
  val `text/texmacs` = MimeType("text/texmacs", "tm", "ts")
  val `text/troff` = MimeType(
      "text/troff", "man", "me", "ms", "roff", "t", "tr")
  val `text/ulpfec` = MimeType("text/ulpfec")
  val `text/uri-list` = MimeType("text/uri-list", "uri", "uris", "urls")
  val `text/vnd.abc` = MimeType("text/vnd.abc")
  val `text/vnd.curl` = MimeType("text/vnd.curl", "curl")
  val `text/vnd.curl.dcurl` = MimeType("text/vnd.curl.dcurl", "dcurl")
  val `text/vnd.curl.mcurl` = MimeType("text/vnd.curl.mcurl", "mcurl")
  val `text/vnd.curl.scurl` = MimeType("text/vnd.curl.scurl", "scurl")
  val `text/vnd.dmclientscript` = MimeType("text/vnd.dmclientscript")
  val `text/vnd.esmertec.theme-descriptor` = MimeType(
      "text/vnd.esmertec.theme-descriptor")
  val `text/vnd.fly` = MimeType("text/vnd.fly", "fly")
  val `text/vnd.fmi.flexstor` = MimeType("text/vnd.fmi.flexstor", "flx")
  val `text/vnd.graphviz` = MimeType("text/vnd.graphviz", "gv")
  val `text/vnd.in3d.3dml` = MimeType("text/vnd.in3d.3dml", "3dml")
  val `text/vnd.in3d.spot` = MimeType("text/vnd.in3d.spot", "spot")
  val `text/vnd.iptc.newsml` = MimeType("text/vnd.iptc.newsml")
  val `text/vnd.iptc.nitf` = MimeType("text/vnd.iptc.nitf")
  val `text/vnd.latex-z` = MimeType("text/vnd.latex-z")
  val `text/vnd.motorola.reflex` = MimeType("text/vnd.motorola.reflex")
  val `text/vnd.ms-mediapackage` = MimeType("text/vnd.ms-mediapackage")
  val `text/vnd.net2phone.commcenter.command` = MimeType(
      "text/vnd.net2phone.commcenter.command")
  val `text/vnd.si.uricatalogue` = MimeType("text/vnd.si.uricatalogue")
  val `text/vnd.sun.j2me.app-descriptor` = MimeType(
      "text/vnd.sun.j2me.app-descriptor", "jad")
  val `text/vnd.trolltech.linguist` = MimeType("text/vnd.trolltech.linguist")
  val `text/vnd.wap.si` = MimeType("text/vnd.wap.si")
  val `text/vnd.wap.sl` = MimeType("text/vnd.wap.sl")
  val `text/vnd.wap.wml` = MimeType("text/vnd.wap.wml", "wml")
  val `text/vnd.wap.wmlscript` = MimeType("text/vnd.wap.wmlscript", "wmls")
  val `text/x-asm` = MimeType("text/x-asm", "asm", "s")
  val `text/x-bibtex` = MimeType("text/x-bibtex", "bib")
  val `text/x-c` = MimeType(
      "text/x-c", "c", "cc", "cpp", "cxx", "dic", "h", "hh")
  val `text/x-c++hdr` = MimeType(
      "text/x-c++hdr", "h", "++", "hh", "hpp", "hxx")
  val `text/x-c++src` = MimeType(
      "text/x-c++src", "c", "++", "cc", "cpp", "cxx")
  val `text/x-chdr` = MimeType("text/x-chdr", "h")
  val `text/x-csh` = MimeType("text/x-csh", "csh")
  val `text/x-csrc` = MimeType("text/x-csrc", "c")
  val `text/x-fortran` = MimeType("text/x-fortran", "f", "f77", "f90", "for")
  val `text/x-haskell` = MimeType("text/x-haskell", "hs")
  val `text/x-java` = MimeType("text/x-java", "java")
  val `text/x-java-source` = MimeType("text/x-java-source", "java")
  val `text/x-literate-haskell` = MimeType("text/x-literate-haskell", "lhs")
  val `text/x-moc` = MimeType("text/x-moc", "moc")
  val `text/x-pascal` = MimeType("text/x-pascal", "p", "pas")
  val `text/x-pcs-gcd` = MimeType("text/x-pcs-gcd", "gcd")
  val `text/x-perl` = MimeType("text/x-perl", "pl", "pm")
  val `text/x-psp` = MimeType("text/x-psp", "psp")
  val `text/x-python` = MimeType("text/x-python", "py")
  val `text/x-setext` = MimeType("text/x-setext", "etx")
  val `text/x-sh` = MimeType("text/x-sh", "sh")
  val `text/x-tcl` = MimeType("text/x-tcl", "tcl", "tk")
  val `text/x-tex` = MimeType("text/x-tex", "cls", "ltx", "sty", "tex")
  val `text/x-uuencode` = MimeType("text/x-uuencode", "uu")
  val `text/x-vcalendar` = MimeType("text/x-vcalendar", "vcs")
  val `text/x-vcard` = MimeType("text/x-vcard", "vcf")
  val `text/xml` = MimeType("text/xml")
  val `text/xml-external-parsed-entity` = MimeType(
      "text/xml-external-parsed-entity")
  val `video/3gpp` = MimeType("video/3gpp", "3gp")
  val `video/3gpp-tt` = MimeType("video/3gpp-tt")
  val `video/3gpp2` = MimeType("video/3gpp2", "3g2")
  val `video/bmpeg` = MimeType("video/bmpeg")
  val `video/bt656` = MimeType("video/bt656")
  val `video/celb` = MimeType("video/celb")
  val `video/dl` = MimeType("video/dl", "dl")
  val `video/dv` = MimeType("video/dv", "dif", "dv")
  val `video/example` = MimeType("video/example")
  val `video/fli` = MimeType("video/fli", "fli")
  val `video/gl` = MimeType("video/gl", "gl")
  val `video/h261` = MimeType("video/h261", "h261")
  val `video/h263` = MimeType("video/h263", "h263")
  val `video/h263-1998` = MimeType("video/h263-1998")
  val `video/h263-2000` = MimeType("video/h263-2000")
  val `video/h264` = MimeType("video/h264", "h264")
  val `video/jpeg` = MimeType("video/jpeg", "jpgv")
  val `video/jpeg2000` = MimeType("video/jpeg2000")
  val `video/jpm` = MimeType("video/jpm", "jpgm", "jpm")
  val `video/mj2` = MimeType("video/mj2", "mj2", "mjp2")
  val `video/mp1s` = MimeType("video/mp1s")
  val `video/mp2p` = MimeType("video/mp2p")
  val `video/mp2t` = MimeType("video/mp2t")
  val `video/mp4` = MimeType("video/mp4", "mp4", "mp4v", "mpg4")
  val `video/mp4v-es` = MimeType("video/mp4v-es")
  val `video/mpeg` = MimeType("video/mpeg", "m1v", "m2v", "mpe", "mpeg", "mpg")
  val `video/mpeg4-generic` = MimeType("video/mpeg4-generic")
  val `video/mpv` = MimeType("video/mpv")
  val `video/nv` = MimeType("video/nv")
  val `video/ogg` = MimeType("video/ogg", "ogv")
  val `video/parityfec` = MimeType("video/parityfec")
  val `video/pointer` = MimeType("video/pointer")
  val `video/quicktime` = MimeType("video/quicktime", "mov", "qt")
  val `video/raw` = MimeType("video/raw")
  val `video/rtp-enc-aescm128` = MimeType("video/rtp-enc-aescm128")
  val `video/rtx` = MimeType("video/rtx")
  val `video/smpte292m` = MimeType("video/smpte292m")
  val `video/ulpfec` = MimeType("video/ulpfec")
  val `video/vc1` = MimeType("video/vc1")
  val `video/vnd.cctv` = MimeType("video/vnd.cctv")
  val `video/vnd.dlna.mpeg-tts` = MimeType("video/vnd.dlna.mpeg-tts")
  val `video/vnd.fvt` = MimeType("video/vnd.fvt", "fvt")
  val `video/vnd.hns.video` = MimeType("video/vnd.hns.video")
  val `video/vnd.iptvforum.1dparityfec-1010` = MimeType(
      "video/vnd.iptvforum.1dparityfec-1010")
  val `video/vnd.iptvforum.1dparityfec-2005` = MimeType(
      "video/vnd.iptvforum.1dparityfec-2005")
  val `video/vnd.iptvforum.2dparityfec-1010` = MimeType(
      "video/vnd.iptvforum.2dparityfec-1010")
  val `video/vnd.iptvforum.2dparityfec-2005` = MimeType(
      "video/vnd.iptvforum.2dparityfec-2005")
  val `video/vnd.iptvforum.ttsavc` = MimeType("video/vnd.iptvforum.ttsavc")
  val `video/vnd.iptvforum.ttsmpeg2` = MimeType("video/vnd.iptvforum.ttsmpeg2")
  val `video/vnd.motorola.video` = MimeType("video/vnd.motorola.video")
  val `video/vnd.motorola.videop` = MimeType("video/vnd.motorola.videop")
  val `video/vnd.mpegurl` = MimeType("video/vnd.mpegurl", "m4u", "mxu")
  val `video/vnd.ms-playready.media.pyv` = MimeType(
      "video/vnd.ms-playready.media.pyv", "pyv")
  val `video/vnd.nokia.interleaved-multimedia` = MimeType(
      "video/vnd.nokia.interleaved-multimedia")
  val `video/vnd.nokia.videovoip` = MimeType("video/vnd.nokia.videovoip")
  val `video/vnd.objectvideo` = MimeType("video/vnd.objectvideo")
  val `video/vnd.sealed.mpeg1` = MimeType("video/vnd.sealed.mpeg1")
  val `video/vnd.sealed.mpeg4` = MimeType("video/vnd.sealed.mpeg4")
  val `video/vnd.sealed.swf` = MimeType("video/vnd.sealed.swf")
  val `video/vnd.sealedmedia.softseal.mov` = MimeType(
      "video/vnd.sealedmedia.softseal.mov")
  val `video/vnd.vivo` = MimeType("video/vnd.vivo", "viv")
  val `video/x-f4v` = MimeType("video/x-f4v", "f4v")
  val `video/x-fli` = MimeType("video/x-fli", "fli")
  val `video/x-flv` = MimeType("video/x-flv", "flv")
  val `video/x-la-asf` = MimeType("video/x-la-asf", "lsf", "lsx")
  val `video/x-m4v` = MimeType("video/x-m4v", "m4v")
  val `video/x-mng` = MimeType("video/x-mng", "mng")
  val `video/x-ms-asf` = MimeType("video/x-ms-asf", "asf", "asx")
  val `video/x-ms-wm` = MimeType("video/x-ms-wm", "wm")
  val `video/x-ms-wmv` = MimeType("video/x-ms-wmv", "wmv")
  val `video/x-ms-wmx` = MimeType("video/x-ms-wmx", "wmx")
  val `video/x-ms-wvx` = MimeType("video/x-ms-wvx", "wvx")
  val `video/x-msvideo` = MimeType("video/x-msvideo", "avi")
  val `video/x-sgi-movie` = MimeType("video/x-sgi-movie", "movie")
  val `x-conference/x-cooltalk` = MimeType("x-conference/x-cooltalk", "ice")
  val `x-world/x-vrml` = MimeType("x-world/x-vrml", "vrm", "vrml", "wrl")

  val mimeTypesMap = new javax.activation.MimetypesFileTypeMap()

  types.values foreach { t =>
    mimeTypesMap.addMimeTypes(t.name + " " + t.extensions.mkString(" "))
  }
}
