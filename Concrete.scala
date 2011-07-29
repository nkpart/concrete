package concrete

import scalaz._
import Scalaz._

import sbinary.DefaultProtocol._
import sbinary._

object SBinaryPimping {
  case class WriteM[A](v: A, f: Function1[Output, Unit])

  type Write = WriteM[Unit]

  implicit val WriteWritesLol: Writes[Write] = new Writes[Write] {
    def writes(out: sbinary.Output, value: Write) {
      value.f(out)
    }
  }

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
  case class Extends[A, From](m: Migrate[A, From]) extends Kind[A]

  trait SafeCopy[A] {
    val version: Version = Version(0)
    val kind: Kind[A] = Base()

    val getCopy: Contained[Reads[A]]
    def putCopy(a: A): Contained[Write]

    lazy val internalConsistency: Consistency[A] = {
      implicit val _ = this
      val ret: Consistency[A] = computeConsistency
      ret
    }
  }

  def constructGetterFromVersion[A: SafeCopy](diskVersion: Version): Reads[A] = {
    val safecopy = implicitly[SafeCopy[A]]
    val version = safecopy.version
    if (version == diskVersion) {
      unsafeUnPack(safecopy.getCopy)
    } else {
      kindFromProxy match {
        case Primitive() => error("cannot migrate from primitive types")
        case Base() => error("Cannot find getter for version " + version)
        case Extends(m) => {
          val xxx = constructGetterFromVersion(diskVersion)(m.extended)
          xxx.map(m.migrate _)
        }
      }
    }
  }

  def safeGet[A: SafeCopy]: Reads[A] = getSafeGet.join

  def getSafeGet[A](implicit sc: SafeCopy[A]): Reads[Reads[A]] = {
    kindFromProxy match {
      case Primitive() => unsafeUnPack(sc.getCopy).pure[Reads]
      case _ => implicitly[Reads[Version]] map { v =>
        constructGetterFromVersion(v)
      }
    }
  }

  def safePut[A: SafeCopy](a: A): Write = {
    getSafePut >>= (putter => putter(a))
  }

  // TODO getSafePut
  def getSafePut[A](implicit sc: SafeCopy[A]): WriteM[A => Write] = {
    checkConsistency({
      kindFromProxy match {
        case Primitive() => ((a: A) => unsafeUnPack(sc.putCopy(a))).pure[WriteM]
        case _ => {
          write(versionFromProxy) >|> ((a: A) => unsafeUnPack(sc.putCopy(a))).pure[WriteM]
        }
      }
    })
  }

  def extension[A, B](implicit sc: SafeCopy[A], m: Migrate[A, B]): Kind[A] = Extends(m)

  def base[A]: Kind[A] = Base()

  def primitive[A]: Kind[A] = Primitive()

  case class Version(value: Int) extends NewType[Int]

  object Version {
    implicit def serialize: Format[Version] = new Format[_root_.concrete.Concrete.Version] {
      def reads(in: sbinary.Input) = Version(implicitly[Format[Int]].reads(in))

      def writes(out: Output, value: Version) {
        implicitly[Format[Int]].writes(out, value.value)
      }
    }
  }

  case class Contained[A](value: A) extends NewType[A]

  def unsafeUnPack[A](c: Contained[A]) = c.value
  def contain[A](a: A): Contained[A] = Contained(a)

  trait Consistency[A]
  case class Consistent[A]() extends Consistency[A]
  case class NotConsistent[A](reason: String) extends Consistency[A]

  def availableVersions[A: SafeCopy]: List[Int] = kindFromProxy match {
    case Primitive() => Nil
    case Base() => versionFromProxy.value :: Nil
    case Extends(m) => versionFromProxy.value :: availableVersions(m.extended)
  }

  def validChain[A: SafeCopy]: Boolean = kindFromProxy match {
    case Primitive() => true
    case Base() => true
    case Extends(m) => _check(m.extended)
  }

  def _check[A: SafeCopy]: Boolean = kindFromProxy match {
    case Primitive() => true
    case Base() => true
    case Extends(m) => _check(m.extended)
  }

  def checkConsistency[A: SafeCopy, B, M[_]: Monad](ks: M[B]): M[B] = consistentFromProxy match {
    case NotConsistent(msg) => error(msg)
    case Consistent() => ks
  }

  def computeConsistency[A: SafeCopy]: Consistency[A] = {
    val versions = availableVersions
    if (isObviouslyConsistent(kindFromProxy)) {
      Consistent()
    } else if (versions != versions.distinct) {
      NotConsistent("Duplicate version tags: %s" format versions.toString)
    } else if (!(validChain)) {
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

  def consistentFromProxy[A: SafeCopy]: Consistency[A] = implicitly[SafeCopy[A]].internalConsistency
  def versionFromProxy[A: SafeCopy]: Version = implicitly[SafeCopy[A]].version
  def kindFromProxy[A: SafeCopy]: Kind[A] = implicitly[SafeCopy[A]].kind

  object Instances {
    def get[A](implicit s: Format[A]): Reads[A] = s

    implicit val StringSC = new SafeCopy[String] {
      override val kind = primitive[String]
      val getCopy = contain(get[String])
      def putCopy(s: String) = contain(write(s))
    }

    def replicateM[A, M[_]](i: Int, ma: M[A])(implicit m: Monad[M]): M[List[A]] = ma.replicateM(i)

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
      // TODO: Fix the initialisation order dependencies of this. If the Migrate
      // instance is declared after the SafeCopy instance, then null pointers
      // abound.
      implicit val m1 = new Migrate[Contacts, Contacts_v0] {
        val extended = implicitly[SafeCopy[Contacts_v0]]

        def migrate(old: Contacts_v0): Contacts = Contacts {
          old.contacts map {case (name, address) => Contact(name, address, "ABC")}
        }
      }

      implicit val safecopy: SafeCopy[Contacts] = new SafeCopy[Contacts] {
        override val version = Version(2)
        override val kind = extension[Contacts, Contacts_v0]
        val getCopy = contain(safeGet[List[Contact]].map(Contacts(_)))
        def putCopy(cs: Contacts) = contain(safePut(cs.contacts))
      }
    }
  }

  def main(args: Array[String]) {
    import example._
    println(implicitly[SafeCopy[Contacts_v0]])
    val init = Contacts_v0(("Nick", "lolol") :: Nil)
    println(init)
    val writtenInit: Write = safePut(init)
    println("Got a write")
    val as = Operations.toByteArray(writtenInit)
    println(as)
    val next: Contacts = Operations.fromByteArray(as)(safeGet[Contacts])
    println(next)
  }
}
