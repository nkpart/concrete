package concrete

import scalaz._
import Scalaz._

import sbinary._
import sbinary.DefaultProtocol._

import SBinaryScalaz._

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

    lazy val internalConsistency: Consistency[A] = computeConsistency(this)
  }

  def constructGetterFromVersion[A](diskVersion: Version)(implicit safecopy: SafeCopy[A]): Reads[A] = {
    val version = safecopy.version
    if (version == diskVersion) {
      unsafeUnPack(safecopy.getCopy)
    } else {
      safecopy.kind match {
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
    sc.kind match {
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
      sc.kind match {
        case Primitive() => ((a: A) => unsafeUnPack(sc.putCopy(a))).pure[WriteM]
        case _ => write(sc.version) >|> ((a: A) => unsafeUnPack(sc.putCopy(a))).pure[WriteM]
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

  def availableVersions[A](implicit sc: SafeCopy[A]): List[Int] = sc.kind match {
    case Primitive() => Nil
    case Base() => sc.version.value :: Nil
    case Extends(m) => sc.version.value :: availableVersions(m.extended)
  }

  def validChain[A](implicit sc: SafeCopy[A]): Boolean = sc.kind match {
    case Primitive() => true
    case Base() => true
    case Extends(m) => _check(m.extended)
  }

  def _check[A](implicit sc: SafeCopy[A]): Boolean = sc.kind match {
    case Primitive() => false
    case Base() => true
    case Extends(m) => _check(m.extended)
  }

  def checkConsistency[A, B, M[_]](ks: M[B])(implicit sc: SafeCopy[A],  m: Monad[M]): M[B] = sc.internalConsistency match {
    case NotConsistent(msg) => error(msg) // TODO: this is `fail` on monad in SafeCopy.
    case Consistent() => ks
  }

  def computeConsistency[A](implicit sc: SafeCopy[A]): Consistency[A] = {
    val versions = availableVersions
    if (isObviouslyConsistent(sc.kind)) {
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
