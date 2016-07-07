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

package rapture.i18n

import scala.reflect._
import annotation.unchecked._

object languages {
  object aa extends Locale[Aa]()
  object ab extends Locale[Ab]()
  object ae extends Locale[Ae]()
  object af extends Locale[Af]()
  object ak extends Locale[Ak]()
  object am extends Locale[Am]()
  object an extends Locale[An]()
  object ar extends Locale[Ar]()
  object as extends Locale[As]()
  object av extends Locale[Av]()
  object ay extends Locale[Ay]()
  object az extends Locale[Az]()
  object ba extends Locale[Ba]()
  object be extends Locale[Be]()
  object bg extends Locale[Bg]()
  object bh extends Locale[Bh]()
  object bi extends Locale[Bi]()
  object bm extends Locale[Bm]()
  object bn extends Locale[Bn]()
  object bo extends Locale[Bo]()
  object br extends Locale[Br]()
  object bs extends Locale[Bs]()
  object ca extends Locale[Ca]()
  object ce extends Locale[Ce]()
  object ch extends Locale[Ch]()
  object co extends Locale[Co]()
  object cr extends Locale[Cr]()
  object cs extends Locale[Cs]()
  object cu extends Locale[Cu]()
  object cv extends Locale[Cv]()
  object cy extends Locale[Cy]()
  object da extends Locale[Da]()
  object de extends Locale[De]()
  object dv extends Locale[Dv]()
  object dz extends Locale[Dz]()
  object ee extends Locale[Ee]()
  object el extends Locale[El]()
  object en extends Locale[En]()
  object eo extends Locale[Eo]()
  object es extends Locale[Es]()
  object et extends Locale[Et]()
  object eu extends Locale[Eu]()
  object fa extends Locale[Fa]()
  object ff extends Locale[Ff]()
  object fi extends Locale[Fi]()
  object fj extends Locale[Fj]()
  object fo extends Locale[Fo]()
  object fr extends Locale[Fr]()
  object fy extends Locale[Fy]()
  object ga extends Locale[Ga]()
  object gd extends Locale[Gd]()
  object gl extends Locale[Gl]()
  object gn extends Locale[Gn]()
  object gu extends Locale[Gu]()
  object gv extends Locale[Gv]()
  object ha extends Locale[Ha]()
  object he extends Locale[He]()
  object hi extends Locale[Hi]()
  object ho extends Locale[Ho]()
  object hr extends Locale[Hr]()
  object ht extends Locale[Ht]()
  object hu extends Locale[Hu]()
  object hy extends Locale[Hy]()
  object hz extends Locale[Hz]()
  object ia extends Locale[Ia]()
  object id extends Locale[Id]()
  object ie extends Locale[Ie]()
  object ig extends Locale[Ig]()
  object ii extends Locale[Ii]()
  object ik extends Locale[Ik]()
  object io extends Locale[Io]()
  object is extends Locale[Is]()
  object it extends Locale[It]()
  object iu extends Locale[Iu]()
  object ja extends Locale[Ja]()
  object jv extends Locale[Jv]()
  object ka extends Locale[Ka]()
  object kg extends Locale[Kg]()
  object ki extends Locale[Ki]()
  object kj extends Locale[Kj]()
  object kk extends Locale[Kk]()
  object kl extends Locale[Kl]()
  object km extends Locale[Km]()
  object kn extends Locale[Kn]()
  object ko extends Locale[Ko]()
  object kr extends Locale[Kr]()
  object ks extends Locale[Ks]()
  object ku extends Locale[Ku]()
  object kv extends Locale[Kv]()
  object kw extends Locale[Kw]()
  object ky extends Locale[Ky]()
  object la extends Locale[La]()
  object lb extends Locale[Lb]()
  object lg extends Locale[Lg]()
  object li extends Locale[Li]()
  object ln extends Locale[Ln]()
  object lo extends Locale[Lo]()
  object lt extends Locale[Lt]()
  object lu extends Locale[Lu]()
  object lv extends Locale[Lv]()
  object mg extends Locale[Mg]()
  object mh extends Locale[Mh]()
  object mi extends Locale[Mi]()
  object mk extends Locale[Mk]()
  object ml extends Locale[Ml]()
  object mn extends Locale[Mn]()
  object mr extends Locale[Mr]()
  object ms extends Locale[Ms]()
  object mt extends Locale[Mt]()
  object my extends Locale[My]()
  object na extends Locale[Na]()
  object nb extends Locale[Nb]()
  object nd extends Locale[Nd]()
  object ne extends Locale[Ne]()
  object ng extends Locale[Ng]()
  object nl extends Locale[Nl]()
  object nn extends Locale[Nn]()
  object no extends Locale[No]()
  object nr extends Locale[Nr]()
  object nv extends Locale[Nv]()
  object ny extends Locale[Ny]()
  object oc extends Locale[Oc]()
  object oj extends Locale[Oj]()
  object om extends Locale[Om]()
  object or extends Locale[Or]()
  object os extends Locale[Os]()
  object pa extends Locale[Pa]()
  object pi extends Locale[Pi]()
  object pl extends Locale[Pl]()
  object ps extends Locale[Ps]()
  object pt extends Locale[Pt]()
  object qu extends Locale[Qu]()
  object rm extends Locale[Rm]()
  object rn extends Locale[Rn]()
  object ro extends Locale[Ro]()
  object ru extends Locale[Ru]()
  object rw extends Locale[Rw]()
  object sa extends Locale[Sa]()
  object sc extends Locale[Sc]()
  object sd extends Locale[Sd]()
  object se extends Locale[Se]()
  object sg extends Locale[Sg]()
  object si extends Locale[Si]()
  object sk extends Locale[Sk]()
  object sl extends Locale[Sl]()
  object sm extends Locale[Sm]()
  object sn extends Locale[Sn]()
  object so extends Locale[So]()
  object sq extends Locale[Sq]()
  object sr extends Locale[Sr]()
  object ss extends Locale[Ss]()
  object st extends Locale[St]()
  object su extends Locale[Su]()
  object sv extends Locale[Sv]()
  object sw extends Locale[Sw]()
  object ta extends Locale[Ta]()
  object te extends Locale[Te]()
  object tg extends Locale[Tg]()
  object th extends Locale[Th]()
  object ti extends Locale[Ti]()
  object tk extends Locale[Tk]()
  object tl extends Locale[Tl]()
  object tn extends Locale[Tn]()
  object to extends Locale[To]()
  object tr extends Locale[Tr]()
  object ts extends Locale[Ts]()
  object tt extends Locale[Tt]()
  object tw extends Locale[Tw]()
  object ty extends Locale[Ty]()
  object ug extends Locale[Ug]()
  object uk extends Locale[Uk]()
  object ur extends Locale[Ur]()
  object uz extends Locale[Uz]()
  object ve extends Locale[Ve]()
  object vi extends Locale[Vi]()
  object vo extends Locale[Vo]()
  object wa extends Locale[Wa]()
  object wo extends Locale[Wo]()
  object xh extends Locale[Xh]()
  object yi extends Locale[Yi]()
  object yo extends Locale[Yo]()
  object za extends Locale[Za]()
  object zh extends Locale[Zh]()
  object zu extends Locale[Zu]()
}

case class DefaultLanguage[-L <: Language](tag: ClassTag[L @uncheckedVariance])

trait Language
final class Aa extends Language
final class Ab extends Language
final class Ae extends Language
final class Af extends Language
final class Ak extends Language
final class Am extends Language
final class An extends Language
final class Ar extends Language
final class As extends Language
final class Av extends Language
final class Ay extends Language
final class Az extends Language
final class Ba extends Language
final class Be extends Language
final class Bg extends Language
final class Bh extends Language
final class Bi extends Language
final class Bm extends Language
final class Bn extends Language
final class Bo extends Language
final class Br extends Language
final class Bs extends Language
final class Ca extends Language
final class Ce extends Language
final class Ch extends Language
final class Co extends Language
final class Cr extends Language
final class Cs extends Language
final class Cu extends Language
final class Cv extends Language
final class Cy extends Language
final class Da extends Language
final class De extends Language
final class Dv extends Language
final class Dz extends Language
final class Ee extends Language
final class El extends Language
final class En extends Language
final class Eo extends Language
final class Es extends Language
final class Et extends Language
final class Eu extends Language
final class Fa extends Language
final class Ff extends Language
final class Fi extends Language
final class Fj extends Language
final class Fo extends Language
final class Fr extends Language
final class Fy extends Language
final class Ga extends Language
final class Gd extends Language
final class Gl extends Language
final class Gn extends Language
final class Gu extends Language
final class Gv extends Language
final class Ha extends Language
final class He extends Language
final class Hi extends Language
final class Ho extends Language
final class Hr extends Language
final class Ht extends Language
final class Hu extends Language
final class Hy extends Language
final class Hz extends Language
final class Ia extends Language
final class Id extends Language
final class Ie extends Language
final class Ig extends Language
final class Ii extends Language
final class Ik extends Language
final class Io extends Language
final class Is extends Language
final class It extends Language
final class Iu extends Language
final class Ja extends Language
final class Jv extends Language
final class Ka extends Language
final class Kg extends Language
final class Ki extends Language
final class Kj extends Language
final class Kk extends Language
final class Kl extends Language
final class Km extends Language
final class Kn extends Language
final class Ko extends Language
final class Kr extends Language
final class Ks extends Language
final class Ku extends Language
final class Kv extends Language
final class Kw extends Language
final class Ky extends Language
final class La extends Language
final class Lb extends Language
final class Lg extends Language
final class Li extends Language
final class Ln extends Language
final class Lo extends Language
final class Lt extends Language
final class Lu extends Language
final class Lv extends Language
final class Mg extends Language
final class Mh extends Language
final class Mi extends Language
final class Mk extends Language
final class Ml extends Language
final class Mn extends Language
final class Mr extends Language
final class Ms extends Language
final class Mt extends Language
final class My extends Language
final class Na extends Language
final class Nb extends Language
final class Nd extends Language
final class Ne extends Language
final class Ng extends Language
final class Nl extends Language
final class Nn extends Language
final class No extends Language
final class Nr extends Language
final class Nv extends Language
final class Ny extends Language
final class Oc extends Language
final class Oj extends Language
final class Om extends Language
final class Or extends Language
final class Os extends Language
final class Pa extends Language
final class Pi extends Language
final class Pl extends Language
final class Ps extends Language
final class Pt extends Language
final class Qu extends Language
final class Rm extends Language
final class Rn extends Language
final class Ro extends Language
final class Ru extends Language
final class Rw extends Language
final class Sa extends Language
final class Sc extends Language
final class Sd extends Language
final class Se extends Language
final class Sg extends Language
final class Si extends Language
final class Sk extends Language
final class Sl extends Language
final class Sm extends Language
final class Sn extends Language
final class So extends Language
final class Sq extends Language
final class Sr extends Language
final class Ss extends Language
final class St extends Language
final class Su extends Language
final class Sv extends Language
final class Sw extends Language
final class Ta extends Language
final class Te extends Language
final class Tg extends Language
final class Th extends Language
final class Ti extends Language
final class Tk extends Language
final class Tl extends Language
final class Tn extends Language
final class To extends Language
final class Tr extends Language
final class Ts extends Language
final class Tt extends Language
final class Tw extends Language
final class Ty extends Language
final class Ug extends Language
final class Uk extends Language
final class Ur extends Language
final class Uz extends Language
final class Ve extends Language
final class Vi extends Language
final class Vo extends Language
final class Wa extends Language
final class Wo extends Language
final class Xh extends Language
final class Yi extends Language
final class Yo extends Language
final class Za extends Language
final class Zh extends Language
final class Zu extends Language
