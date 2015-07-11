/******************************************************************************************************************\
* Rapture I18N, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in complance    *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.i18n

import rapture.base._

import scala.reflect._
import scala.reflect.macros._

import language.implicitConversions
import language.experimental.macros

object `package` {

  implicit def i18nStringToString[L <: Language](msg: I18n[String, L])
      (implicit locale: Locale[_ >: L]): String = locale.from(msg)
  
  implicit class I18nEnrichedStringContext(sc: StringContext) {
    
    private def context[L <: Language: ClassTag](sc: StringContext, params: List[I18nStringParam[L]]):
        I18n[String, L] =
      new I18n[String, L](Map(implicitly[ClassTag[L]] -> sc.parts.zip(params.map(_.i18n.apply[L]) :+ "").map {
        case (a, b) => a+b
      }.mkString))

    def aa(params: I18nStringParam[Aa]*): I18n[String, Aa] = context[Aa](sc, params.toList)
    def ab(params: I18nStringParam[Ab]*): I18n[String, Ab] = context[Ab](sc, params.toList)
    def ae(params: I18nStringParam[Ae]*): I18n[String, Ae] = context[Ae](sc, params.toList)
    def af(params: I18nStringParam[Af]*): I18n[String, Af] = context[Af](sc, params.toList)
    def ak(params: I18nStringParam[Ak]*): I18n[String, Ak] = context[Ak](sc, params.toList)
    def am(params: I18nStringParam[Am]*): I18n[String, Am] = context[Am](sc, params.toList)
    def an(params: I18nStringParam[An]*): I18n[String, An] = context[An](sc, params.toList)
    def ar(params: I18nStringParam[Ar]*): I18n[String, Ar] = context[Ar](sc, params.toList)
    def as(params: I18nStringParam[As]*): I18n[String, As] = context[As](sc, params.toList)
    def av(params: I18nStringParam[Av]*): I18n[String, Av] = context[Av](sc, params.toList)
    def ay(params: I18nStringParam[Ay]*): I18n[String, Ay] = context[Ay](sc, params.toList)
    def az(params: I18nStringParam[Az]*): I18n[String, Az] = context[Az](sc, params.toList)
    def ba(params: I18nStringParam[Ba]*): I18n[String, Ba] = context[Ba](sc, params.toList)
    def be(params: I18nStringParam[Be]*): I18n[String, Be] = context[Be](sc, params.toList)
    def bg(params: I18nStringParam[Bg]*): I18n[String, Bg] = context[Bg](sc, params.toList)
    def bh(params: I18nStringParam[Bh]*): I18n[String, Bh] = context[Bh](sc, params.toList)
    def bi(params: I18nStringParam[Bi]*): I18n[String, Bi] = context[Bi](sc, params.toList)
    def bm(params: I18nStringParam[Bm]*): I18n[String, Bm] = context[Bm](sc, params.toList)
    def bn(params: I18nStringParam[Bn]*): I18n[String, Bn] = context[Bn](sc, params.toList)
    def bo(params: I18nStringParam[Bo]*): I18n[String, Bo] = context[Bo](sc, params.toList)
    def br(params: I18nStringParam[Br]*): I18n[String, Br] = context[Br](sc, params.toList)
    def bs(params: I18nStringParam[Bs]*): I18n[String, Bs] = context[Bs](sc, params.toList)
    def ca(params: I18nStringParam[Ca]*): I18n[String, Ca] = context[Ca](sc, params.toList)
    def ce(params: I18nStringParam[Ce]*): I18n[String, Ce] = context[Ce](sc, params.toList)
    def ch(params: I18nStringParam[Ch]*): I18n[String, Ch] = context[Ch](sc, params.toList)
    def co(params: I18nStringParam[Co]*): I18n[String, Co] = context[Co](sc, params.toList)
    def cr(params: I18nStringParam[Cr]*): I18n[String, Cr] = context[Cr](sc, params.toList)
    def cs(params: I18nStringParam[Cs]*): I18n[String, Cs] = context[Cs](sc, params.toList)
    def cu(params: I18nStringParam[Cu]*): I18n[String, Cu] = context[Cu](sc, params.toList)
    def cv(params: I18nStringParam[Cv]*): I18n[String, Cv] = context[Cv](sc, params.toList)
    def cy(params: I18nStringParam[Cy]*): I18n[String, Cy] = context[Cy](sc, params.toList)
    def da(params: I18nStringParam[Da]*): I18n[String, Da] = context[Da](sc, params.toList)
    def de(params: I18nStringParam[De]*): I18n[String, De] = context[De](sc, params.toList)
    def dv(params: I18nStringParam[Dv]*): I18n[String, Dv] = context[Dv](sc, params.toList)
    def dz(params: I18nStringParam[Dz]*): I18n[String, Dz] = context[Dz](sc, params.toList)
    def ee(params: I18nStringParam[Ee]*): I18n[String, Ee] = context[Ee](sc, params.toList)
    def el(params: I18nStringParam[El]*): I18n[String, El] = context[El](sc, params.toList)
    def en(params: I18nStringParam[En]*): I18n[String, En] = context[En](sc, params.toList)
    def eo(params: I18nStringParam[Eo]*): I18n[String, Eo] = context[Eo](sc, params.toList)
    def es(params: I18nStringParam[Es]*): I18n[String, Es] = context[Es](sc, params.toList)
    def et(params: I18nStringParam[Et]*): I18n[String, Et] = context[Et](sc, params.toList)
    def eu(params: I18nStringParam[Eu]*): I18n[String, Eu] = context[Eu](sc, params.toList)
    def fa(params: I18nStringParam[Fa]*): I18n[String, Fa] = context[Fa](sc, params.toList)
    def ff(params: I18nStringParam[Ff]*): I18n[String, Ff] = context[Ff](sc, params.toList)
    def fi(params: I18nStringParam[Fi]*): I18n[String, Fi] = context[Fi](sc, params.toList)
    def fj(params: I18nStringParam[Fj]*): I18n[String, Fj] = context[Fj](sc, params.toList)
    def fo(params: I18nStringParam[Fo]*): I18n[String, Fo] = context[Fo](sc, params.toList)
    def fr(params: I18nStringParam[Fr]*): I18n[String, Fr] = context[Fr](sc, params.toList)
    def fy(params: I18nStringParam[Fy]*): I18n[String, Fy] = context[Fy](sc, params.toList)
    def ga(params: I18nStringParam[Ga]*): I18n[String, Ga] = context[Ga](sc, params.toList)
    def gd(params: I18nStringParam[Gd]*): I18n[String, Gd] = context[Gd](sc, params.toList)
    def gl(params: I18nStringParam[Gl]*): I18n[String, Gl] = context[Gl](sc, params.toList)
    def gn(params: I18nStringParam[Gn]*): I18n[String, Gn] = context[Gn](sc, params.toList)
    def gu(params: I18nStringParam[Gu]*): I18n[String, Gu] = context[Gu](sc, params.toList)
    def gv(params: I18nStringParam[Gv]*): I18n[String, Gv] = context[Gv](sc, params.toList)
    def ha(params: I18nStringParam[Ha]*): I18n[String, Ha] = context[Ha](sc, params.toList)
    def he(params: I18nStringParam[He]*): I18n[String, He] = context[He](sc, params.toList)
    def hi(params: I18nStringParam[Hi]*): I18n[String, Hi] = context[Hi](sc, params.toList)
    def ho(params: I18nStringParam[Ho]*): I18n[String, Ho] = context[Ho](sc, params.toList)
    def hr(params: I18nStringParam[Hr]*): I18n[String, Hr] = context[Hr](sc, params.toList)
    def ht(params: I18nStringParam[Ht]*): I18n[String, Ht] = context[Ht](sc, params.toList)
    def hu(params: I18nStringParam[Hu]*): I18n[String, Hu] = context[Hu](sc, params.toList)
    def hy(params: I18nStringParam[Hy]*): I18n[String, Hy] = context[Hy](sc, params.toList)
    def hz(params: I18nStringParam[Hz]*): I18n[String, Hz] = context[Hz](sc, params.toList)
    def ia(params: I18nStringParam[Ia]*): I18n[String, Ia] = context[Ia](sc, params.toList)
    def id(params: I18nStringParam[Id]*): I18n[String, Id] = context[Id](sc, params.toList)
    def ie(params: I18nStringParam[Ie]*): I18n[String, Ie] = context[Ie](sc, params.toList)
    def ig(params: I18nStringParam[Ig]*): I18n[String, Ig] = context[Ig](sc, params.toList)
    def ii(params: I18nStringParam[Ii]*): I18n[String, Ii] = context[Ii](sc, params.toList)
    def ik(params: I18nStringParam[Ik]*): I18n[String, Ik] = context[Ik](sc, params.toList)
    def io(params: I18nStringParam[Io]*): I18n[String, Io] = context[Io](sc, params.toList)
    def is(params: I18nStringParam[Is]*): I18n[String, Is] = context[Is](sc, params.toList)
    def it(params: I18nStringParam[It]*): I18n[String, It] = context[It](sc, params.toList)
    def iu(params: I18nStringParam[Iu]*): I18n[String, Iu] = context[Iu](sc, params.toList)
    def ja(params: I18nStringParam[Ja]*): I18n[String, Ja] = context[Ja](sc, params.toList)
    def jv(params: I18nStringParam[Jv]*): I18n[String, Jv] = context[Jv](sc, params.toList)
    def ka(params: I18nStringParam[Ka]*): I18n[String, Ka] = context[Ka](sc, params.toList)
    def kg(params: I18nStringParam[Kg]*): I18n[String, Kg] = context[Kg](sc, params.toList)
    def ki(params: I18nStringParam[Ki]*): I18n[String, Ki] = context[Ki](sc, params.toList)
    def kj(params: I18nStringParam[Kj]*): I18n[String, Kj] = context[Kj](sc, params.toList)
    def kk(params: I18nStringParam[Kk]*): I18n[String, Kk] = context[Kk](sc, params.toList)
    def kl(params: I18nStringParam[Kl]*): I18n[String, Kl] = context[Kl](sc, params.toList)
    def km(params: I18nStringParam[Km]*): I18n[String, Km] = context[Km](sc, params.toList)
    def kn(params: I18nStringParam[Kn]*): I18n[String, Kn] = context[Kn](sc, params.toList)
    def ko(params: I18nStringParam[Ko]*): I18n[String, Ko] = context[Ko](sc, params.toList)
    def kr(params: I18nStringParam[Kr]*): I18n[String, Kr] = context[Kr](sc, params.toList)
    def ks(params: I18nStringParam[Ks]*): I18n[String, Ks] = context[Ks](sc, params.toList)
    def ku(params: I18nStringParam[Ku]*): I18n[String, Ku] = context[Ku](sc, params.toList)
    def kv(params: I18nStringParam[Kv]*): I18n[String, Kv] = context[Kv](sc, params.toList)
    def kw(params: I18nStringParam[Kw]*): I18n[String, Kw] = context[Kw](sc, params.toList)
    def ky(params: I18nStringParam[Ky]*): I18n[String, Ky] = context[Ky](sc, params.toList)
    def la(params: I18nStringParam[La]*): I18n[String, La] = context[La](sc, params.toList)
    def lb(params: I18nStringParam[Lb]*): I18n[String, Lb] = context[Lb](sc, params.toList)
    def lg(params: I18nStringParam[Lg]*): I18n[String, Lg] = context[Lg](sc, params.toList)
    def li(params: I18nStringParam[Li]*): I18n[String, Li] = context[Li](sc, params.toList)
    def ln(params: I18nStringParam[Ln]*): I18n[String, Ln] = context[Ln](sc, params.toList)
    def lo(params: I18nStringParam[Lo]*): I18n[String, Lo] = context[Lo](sc, params.toList)
    def lt(params: I18nStringParam[Lt]*): I18n[String, Lt] = context[Lt](sc, params.toList)
    def lu(params: I18nStringParam[Lu]*): I18n[String, Lu] = context[Lu](sc, params.toList)
    def lv(params: I18nStringParam[Lv]*): I18n[String, Lv] = context[Lv](sc, params.toList)
    def mg(params: I18nStringParam[Mg]*): I18n[String, Mg] = context[Mg](sc, params.toList)
    def mh(params: I18nStringParam[Mh]*): I18n[String, Mh] = context[Mh](sc, params.toList)
    def mi(params: I18nStringParam[Mi]*): I18n[String, Mi] = context[Mi](sc, params.toList)
    def mk(params: I18nStringParam[Mk]*): I18n[String, Mk] = context[Mk](sc, params.toList)
    def ml(params: I18nStringParam[Ml]*): I18n[String, Ml] = context[Ml](sc, params.toList)
    def mn(params: I18nStringParam[Mn]*): I18n[String, Mn] = context[Mn](sc, params.toList)
    def mr(params: I18nStringParam[Mr]*): I18n[String, Mr] = context[Mr](sc, params.toList)
    def ms(params: I18nStringParam[Ms]*): I18n[String, Ms] = context[Ms](sc, params.toList)
    def mt(params: I18nStringParam[Mt]*): I18n[String, Mt] = context[Mt](sc, params.toList)
    def my(params: I18nStringParam[My]*): I18n[String, My] = context[My](sc, params.toList)
    def na(params: I18nStringParam[Na]*): I18n[String, Na] = context[Na](sc, params.toList)
    def nb(params: I18nStringParam[Nb]*): I18n[String, Nb] = context[Nb](sc, params.toList)
    def nd(params: I18nStringParam[Nd]*): I18n[String, Nd] = context[Nd](sc, params.toList)
    def ne(params: I18nStringParam[Ne]*): I18n[String, Ne] = context[Ne](sc, params.toList)
    def ng(params: I18nStringParam[Ng]*): I18n[String, Ng] = context[Ng](sc, params.toList)
    def nl(params: I18nStringParam[Nl]*): I18n[String, Nl] = context[Nl](sc, params.toList)
    def nn(params: I18nStringParam[Nn]*): I18n[String, Nn] = context[Nn](sc, params.toList)
    def no(params: I18nStringParam[No]*): I18n[String, No] = context[No](sc, params.toList)
    def nr(params: I18nStringParam[Nr]*): I18n[String, Nr] = context[Nr](sc, params.toList)
    def nv(params: I18nStringParam[Nv]*): I18n[String, Nv] = context[Nv](sc, params.toList)
    def ny(params: I18nStringParam[Ny]*): I18n[String, Ny] = context[Ny](sc, params.toList)
    def oc(params: I18nStringParam[Oc]*): I18n[String, Oc] = context[Oc](sc, params.toList)
    def oj(params: I18nStringParam[Oj]*): I18n[String, Oj] = context[Oj](sc, params.toList)
    def om(params: I18nStringParam[Om]*): I18n[String, Om] = context[Om](sc, params.toList)
    def or(params: I18nStringParam[Or]*): I18n[String, Or] = context[Or](sc, params.toList)
    def os(params: I18nStringParam[Os]*): I18n[String, Os] = context[Os](sc, params.toList)
    def pa(params: I18nStringParam[Pa]*): I18n[String, Pa] = context[Pa](sc, params.toList)
    def pi(params: I18nStringParam[Pi]*): I18n[String, Pi] = context[Pi](sc, params.toList)
    def pl(params: I18nStringParam[Pl]*): I18n[String, Pl] = context[Pl](sc, params.toList)
    def ps(params: I18nStringParam[Ps]*): I18n[String, Ps] = context[Ps](sc, params.toList)
    def pt(params: I18nStringParam[Pt]*): I18n[String, Pt] = context[Pt](sc, params.toList)
    def qu(params: I18nStringParam[Qu]*): I18n[String, Qu] = context[Qu](sc, params.toList)
    def rm(params: I18nStringParam[Rm]*): I18n[String, Rm] = context[Rm](sc, params.toList)
    def rn(params: I18nStringParam[Rn]*): I18n[String, Rn] = context[Rn](sc, params.toList)
    def ro(params: I18nStringParam[Ro]*): I18n[String, Ro] = context[Ro](sc, params.toList)
    def ru(params: I18nStringParam[Ru]*): I18n[String, Ru] = context[Ru](sc, params.toList)
    def rw(params: I18nStringParam[Rw]*): I18n[String, Rw] = context[Rw](sc, params.toList)
    def sa(params: I18nStringParam[Sa]*): I18n[String, Sa] = context[Sa](sc, params.toList)
    def sc(params: I18nStringParam[Sc]*): I18n[String, Sc] = context[Sc](sc, params.toList)
    def sd(params: I18nStringParam[Sd]*): I18n[String, Sd] = context[Sd](sc, params.toList)
    def se(params: I18nStringParam[Se]*): I18n[String, Se] = context[Se](sc, params.toList)
    def sg(params: I18nStringParam[Sg]*): I18n[String, Sg] = context[Sg](sc, params.toList)
    def si(params: I18nStringParam[Si]*): I18n[String, Si] = context[Si](sc, params.toList)
    def sk(params: I18nStringParam[Sk]*): I18n[String, Sk] = context[Sk](sc, params.toList)
    def sl(params: I18nStringParam[Sl]*): I18n[String, Sl] = context[Sl](sc, params.toList)
    def sm(params: I18nStringParam[Sm]*): I18n[String, Sm] = context[Sm](sc, params.toList)
    def sn(params: I18nStringParam[Sn]*): I18n[String, Sn] = context[Sn](sc, params.toList)
    def so(params: I18nStringParam[So]*): I18n[String, So] = context[So](sc, params.toList)
    def sq(params: I18nStringParam[Sq]*): I18n[String, Sq] = context[Sq](sc, params.toList)
    def sr(params: I18nStringParam[Sr]*): I18n[String, Sr] = context[Sr](sc, params.toList)
    def ss(params: I18nStringParam[Ss]*): I18n[String, Ss] = context[Ss](sc, params.toList)
    def st(params: I18nStringParam[St]*): I18n[String, St] = context[St](sc, params.toList)
    def su(params: I18nStringParam[Su]*): I18n[String, Su] = context[Su](sc, params.toList)
    def sv(params: I18nStringParam[Sv]*): I18n[String, Sv] = context[Sv](sc, params.toList)
    def sw(params: I18nStringParam[Sw]*): I18n[String, Sw] = context[Sw](sc, params.toList)
    def ta(params: I18nStringParam[Ta]*): I18n[String, Ta] = context[Ta](sc, params.toList)
    def te(params: I18nStringParam[Te]*): I18n[String, Te] = context[Te](sc, params.toList)
    def tg(params: I18nStringParam[Tg]*): I18n[String, Tg] = context[Tg](sc, params.toList)
    def th(params: I18nStringParam[Th]*): I18n[String, Th] = context[Th](sc, params.toList)
    def ti(params: I18nStringParam[Ti]*): I18n[String, Ti] = context[Ti](sc, params.toList)
    def tk(params: I18nStringParam[Tk]*): I18n[String, Tk] = context[Tk](sc, params.toList)
    def tl(params: I18nStringParam[Tl]*): I18n[String, Tl] = context[Tl](sc, params.toList)
    def tn(params: I18nStringParam[Tn]*): I18n[String, Tn] = context[Tn](sc, params.toList)
    def to(params: I18nStringParam[To]*): I18n[String, To] = context[To](sc, params.toList)
    def tr(params: I18nStringParam[Tr]*): I18n[String, Tr] = context[Tr](sc, params.toList)
    def ts(params: I18nStringParam[Ts]*): I18n[String, Ts] = context[Ts](sc, params.toList)
    def tt(params: I18nStringParam[Tt]*): I18n[String, Tt] = context[Tt](sc, params.toList)
    def tw(params: I18nStringParam[Tw]*): I18n[String, Tw] = context[Tw](sc, params.toList)
    def ty(params: I18nStringParam[Ty]*): I18n[String, Ty] = context[Ty](sc, params.toList)
    def ug(params: I18nStringParam[Ug]*): I18n[String, Ug] = context[Ug](sc, params.toList)
    def uk(params: I18nStringParam[Uk]*): I18n[String, Uk] = context[Uk](sc, params.toList)
    def ur(params: I18nStringParam[Ur]*): I18n[String, Ur] = context[Ur](sc, params.toList)
    def uz(params: I18nStringParam[Uz]*): I18n[String, Uz] = context[Uz](sc, params.toList)
    def ve(params: I18nStringParam[Ve]*): I18n[String, Ve] = context[Ve](sc, params.toList)
    def vi(params: I18nStringParam[Vi]*): I18n[String, Vi] = context[Vi](sc, params.toList)
    def vo(params: I18nStringParam[Vo]*): I18n[String, Vo] = context[Vo](sc, params.toList)
    def wa(params: I18nStringParam[Wa]*): I18n[String, Wa] = context[Wa](sc, params.toList)
    def wo(params: I18nStringParam[Wo]*): I18n[String, Wo] = context[Wo](sc, params.toList)
    def xh(params: I18nStringParam[Xh]*): I18n[String, Xh] = context[Xh](sc, params.toList)
    def yi(params: I18nStringParam[Yi]*): I18n[String, Yi] = context[Yi](sc, params.toList)
    def yo(params: I18nStringParam[Yo]*): I18n[String, Yo] = context[Yo](sc, params.toList)
    def za(params: I18nStringParam[Za]*): I18n[String, Za] = context[Za](sc, params.toList)
    def zh(params: I18nStringParam[Zh]*): I18n[String, Zh] = context[Zh](sc, params.toList)
    def zu(params: I18nStringParam[Zu]*): I18n[String, Zu] = context[Zu](sc, params.toList)
  }

}

object Macros {
  
  def missingTranslationsMacro[To <: Language: c.WeakTypeTag, From <: Language: c.WeakTypeTag](c:
      BlackboxContext)(from: c.Expr[I18n[String, From]]): c.Expr[I18n[String, To]] = {
    
    import c.universe._
    import compatibility._

    val fromLangs = (normalize(c)(weakTypeOf[From]) match {
      case rt: RefinedType => rt.parents
      case typ: Type => List(typ)
    }).map(_.toString.split("\\.").last.toLowerCase).to[Set]

    val toLangs = (normalize(c)(weakTypeOf[To]) match {
      case rt: RefinedType => rt.parents
      case typ: Type => List(typ)
    }).map(_.toString.split("\\.").last.toLowerCase).to[Set]

    val missing = toLangs -- fromLangs

    def langs(tree: Tree): Map[String, String] = tree match {
      case Apply(Select(Apply(Select(Select(Select(id1, id2), id3), id4), List(Apply(_, List(Literal(Constant(
          str: String)))))), lang), _) if id1.toString == "rapture" && id2.toString == "i18n" &&
          id3.toString == "package" && id4.toString == "I18nEnrichedStringContext" =>
        
        Map(lang.toString -> str)
      
      case Apply(app: Apply, _) =>
        langs(app)
      
      case Apply(TypeApply(Select(q, id6), _), List(app)) if id6.toString == "$bar" =>
        langs(q) ++ langs(app)
    }

    if(!missing.isEmpty)
      c.abort(c.enclosingPosition, s"""Some language translations were not provided: ${missing.mkString(", ")}""")
    else
      reify(from.splice.asInstanceOf[I18n[String, To]])
  }
}
