package concrete

import scalaz._
import Scalaz._

import sbinary._
import sbinary.DefaultProtocol._

object SBinaryPimping {
  case class WriteM[A](v: A, f: Function1[Output, Unit])

  type Write = WriteM[Unit]

  implicit val WriteMPure: Pure[WriteM] = new Pure[WriteM] {
    def pure[A](a: => A): WriteM[A] = WriteM(a, (_ => ()))
  }

  implicit val WriteMBind: Bind[WriteM] = new Bind[WriteM] {
    def bind[A, B](ma: WriteM[A], f: A => WriteM[B]): WriteM[B] = {
      val WriteM(b, ab) = f(ma.v)
      WriteM(b, (o => { ma.f(o); ab(o) } ))
    }
  }

  implicit val WriteMFunctor: Functor[WriteM] = new Functor[WriteM] {
    def fmap[A, B](r: WriteM[A], f: scala.Function1[A, B]): WriteM[B] = WriteM(f(r.v), r.f)
  }


  def write[T](t: T)(implicit wr: Writes[T]): Write = WriteM((), o => wr.writes(o, t))

  // TODO: replace with whatever from sbinary
  implicit val ReadsPure: Pure[Reads] = new Pure[Reads] {
    def pure[A](a: => A): Reads[A] = new Reads[A] {
      def reads(in: sbinary.Input): A = a
    }
  }

  implicit val ReadsBind: Bind[Reads] = new Bind[Reads] {
    def bind[A, B](ra: Reads[A], f: A => Reads[B]): Reads[B] = new Reads[B] {
      def reads(in: sbinary.Input): B = {
        val a = ra.reads(in)
        f(a).reads(in)
      }
    }
  }

  implicit val ReadsFunctor: Functor[Reads] = new Functor[Reads] {
    def fmap[A, B](r: Reads[A], f: A => B): Reads[B] = new Reads[B] {
      def reads(in: sbinary.Input): B = f(r.reads(in))
    }
  }
}

import SBinaryPimping._

object Concrete {
  trait Migrate[To, From] {
    val extended: SafeCopy[From]

    def migrate(ma: From): To
  }

  trait Kind[A]
  case class Primitive[A]() extends Kind[A]
  case class Base[A]() extends Kind[A]
  case class Extends[A, From](m: Migrate[A, From], f: Proxy[From]) extends Kind[A]

  trait SafeCopy[A] {
    val version: Version[A] = Version(0)
    val kind: Kind[A] = Base()

    val getCopy: Contained[Reads[A]]
    def putCopy(a: A): Contained[Write]

    val internalConsistency: Consistency[A] = {
      implicit val _ = this
      lazy val proxy: Proxy[A] = proxyFromConsistency(ret)
      lazy val ret: Consistency[A] = computeConsistency(proxy)
      ret
    }
  }

  def constructGetterFromVersion[A: SafeCopy](diskVersion: Version[A], proxy: Proxy[A]): Reads[A] = {
    val safecopy = implicitly[SafeCopy[A]]
    val version = safecopy.version
    if (version == diskVersion) {
      unsafeUnPack(safecopy.getCopy)
    } else {
      kindFromProxy(proxy) match {
        case Primitive() => error("cannot migrate from primitive types")
        case Base() => error("Cannot find getter for version " + version)
        case Extends(m, bProxy) => {
          val xxx = constructGetterFromVersion(castVersion(diskVersion), bProxy)(m.extended)
          xxx.map(m.migrate _)
        }
      }
    }
  }

  def safeGet[A: SafeCopy]: Reads[A] = getSafeGet.join

  def getSafeGet[A](implicit sc: SafeCopy[A]): Reads[Reads[A]] = {
    val proxy = Proxy[A]()
    kindFromProxy(proxy) match {
      case Primitive() => unsafeUnPack(sc.getCopy).pure[Reads]
      case _ => implicitly[Reads[Version[A]]] map { v =>
        constructGetterFromVersion(v, proxy)
      }
    }
  }

  def safePut[A: SafeCopy](a: A): Write = {
    getSafePut >>= (putter => putter(a))
  }

  // TODO getSafePut
  def getSafePut[A](implicit sc: SafeCopy[A]): WriteM[A => Write] = {
    val proxy = Proxy[A]
    checkConsistency(Proxy[A](), {
      kindFromProxy(proxy) match {
        case Primitive() => ((a: A) => unsafeUnPack(sc.putCopy(Proxy.asProxyType(a, proxy)))).pure[WriteM]
        case _ => {
          write(versionFromProxy(proxy)) >|> ((a: A) => unsafeUnPack(sc.putCopy(Proxy.asProxyType(a, proxy)))).pure[WriteM]
        }
      }
    })
  }

  def extension[A, B](implicit sc: SafeCopy[A], m: Migrate[A, B]): Kind[A] = Extends(m, Proxy())

  def base[A]: Kind[A] = Base()

  def primitive[A]: Kind[A] = Primitive()

  case class Version[A](value: Int) extends NewType[Int]

  def castVersion[A, B](v: Version[A]): Version[B] = Version(v.value)

  object Version {
    implicit def serialize[A]: Format[Version[A]] = null
  }

  case class Contained[A](value: A) extends NewType[A]

  def unsafeUnPack[A](c: Contained[A]) = c.value
  def contain[A](a: A): Contained[A] = Contained(a)

  trait Consistency[A]
  case class Consistent[A]() extends Consistency[A]
  case class NotConsistent[A](reason: String) extends Consistency[A]

  def availableVersions[A: SafeCopy](p: Proxy[A]): List[Int] = kindFromProxy(p) match {
    case Primitive() => Nil
    case Base() => versionFromProxy(p).value :: Nil
    case Extends(m, bProxy) => versionFromProxy(p).value :: availableVersions(bProxy)(m.extended)
  }

  def validChain[A: SafeCopy](p: Proxy[A]): Boolean = kindFromProxy(p) match {
    case Primitive() => true
    case Base() => true
    case Extends(m, bProxy) => _check(bProxy)(m.extended)
  }

  def _check[A: SafeCopy](p: Proxy[A]): Boolean = kindFromProxy(p) match {
    case Primitive() => true
    case Base() => true
    case Extends(m, bProxy) => _check(bProxy)(m.extended)
  }

  def checkConsistency[A: SafeCopy, B, M[_]: Monad](p: Proxy[A], ks: M[B]): M[B] = consistentFromProxy(p) match {
    case NotConsistent(msg) => error(msg)
    case Consistent() => ks
  }

  def computeConsistency[A: SafeCopy](p: Proxy[A]): Consistency[A] = {
    lazy val versions = availableVersions(p)
    if (isObviouslyConsistent(kindFromProxy(p))) {
      Consistent()
    } else if (versions != versions.distinct) {
      NotConsistent("Duplicate version tags: %s" format versions.toString)
    } else if (!(validChain(p))) {
      NotConsistent("Primitive types cannot be extended as they have no version tag.")
    } else {
      Consistent()
    }
  }

  def isObviouslyConsistent[A](k: Kind[A]): Boolean = k match {
    case Primitive() => true
    case Base() => true
    case _ => false
  }

  def proxyFromConsistency[A: SafeCopy](c: => Consistency[A]): Proxy[A] = Proxy()
  def consistentFromProxy[A: SafeCopy](p: Proxy[A]): Consistency[A] = implicitly[SafeCopy[A]].internalConsistency
  def versionFromProxy[A: SafeCopy](p: Proxy[A]): Version[A] = implicitly[SafeCopy[A]].version
  def kindFromProxy[A: SafeCopy](p: Proxy[A]): Kind[A] = implicitly[SafeCopy[A]].kind

  case class Proxy[A]()
  object Proxy {
    def make[A](a: A): Proxy[A] = Proxy()
    def asProxyType[A](a: A, p: Proxy[A]): A = a
  }

  object Instances {
    def get[A](implicit s: Format[A]): Reads[A] = s

    implicit val StringSC = new SafeCopy[String] {
      override val kind = primitive[String]
      val getCopy = contain(get[String])
      def putCopy(s: String) = contain(write(s))
    }

    def replicateM[A, M[_]](i: Int, ma: M[A])(implicit m: Monad[M]): M[List[A]] = null

    implicit def Tuple2SafeCopy[A, B](implicit sa: SafeCopy[A], sb: SafeCopy[B]): SafeCopy[(A,B)] = new SafeCopy[Tuple2[A, B]] {
      override val kind = primitive[(A, B)]
      val getCopy = contain {
        ((a: A, b: B) => (a,b)).lift[Reads].apply(safeGet[A], safeGet[B])
      }
      def putCopy(t2: (A,B)) = contain {
        safePut(t2._1) >|> safePut(t2._2)
      }
    }

    implicit def ListSC[A](implicit sc: SafeCopy[A]): SafeCopy[List[A]] = new SafeCopy[List[A]] {
      override val kind = primitive[List[A]]
      val getCopy: Contained[Reads[List[A]]] = contain {
         get[Int] >>= { length => (getSafeGet[A] >>= (replicateM(length, _))) }
      }

      def putCopy(xs: List[A]) = contain {
        write(xs.length) >|> { getSafePut[A] >>= (f => xs.traverse_(f)) }
      }
    }
  }

  object example {
    import Instances._

    type Name = String
    type Address = String
    type Phone = String


    // Initial Version
    case class Contacts_v0(contacts: List[(Name, Address)])
    object Contacts_v0 {
      implicit val safecopy: SafeCopy[Contacts_v0] = new SafeCopy[Contacts_v0] {
        val getCopy = contain(safeGet[List[(Name, Address)]] map (Contacts_v0(_)))
        def putCopy(cs: Contacts_v0) = contain(safePut(cs.contacts))
      }
    }

    // Subsequent update
    case class Contact(name: Name, address: Address, phone: Phone)
    object Contact {
      implicit val safecopy: SafeCopy[Contact] = new SafeCopy[Contact] {
        val getCopy = contain {
          for (name <- safeGet[Name]; address <- safeGet[Address]; phone <- safeGet[Phone]) yield { Contact(name, address, phone) }
        }
        def putCopy(c: Contact) = contain {
          safePut(c.name) >|> safePut(c.address) >|> safePut(c.phone)
        }
      }
    }

    case class Contacts(contacts: List[Contact])
    object Contacts {
      implicit val safecopy: SafeCopy[Contacts] = new SafeCopy[Contacts] {
        override val version = Version[Contacts](2)
        override val kind = extension[Contacts, Contacts_v0]
        val getCopy = contain(safeGet[List[Contact]].map(Contacts(_)))
        def putCopy(cs: Contacts) = contain(safePut(cs.contacts))
      }

      implicit val m1 = new Migrate[Contacts, Contacts_v0] {
        val extended = implicitly[SafeCopy[Contacts_v0]]

        def migrate(old: Contacts_v0): Contacts = Contacts {
          old.contacts map { case (name, address) => Contact(name, address, "") }
        }
      }
    }
  }

  def main(args: Array[String]) {

  }
}
