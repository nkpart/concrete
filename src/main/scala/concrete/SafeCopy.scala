package concrete

import scalaz._
import Scalaz._

import sbinary._
import sbinary.DefaultProtocol._

import SBinaryScalaz._

// A migration to To from From
trait Migrate[To, From] {
  val extended: SafeCopy[From]

  def migrate(ma: From): To
}

// The Kind of a SafeCopy instance. Primitive kinds cannot be extended (migrated from).
// Extensions declare migrations, which must start from a Base (eventually).
sealed trait Kind[A]

case class Primitive[A]() extends Kind[A]

case class Base[A]() extends Kind[A]

case class Extends[A, From](m: Migrate[A, From]) extends Kind[A]

object Kind {
  def validChain[A](k: Kind[A]): Boolean = {
    // Extends must end in a Base kind, not a primitive.
    def checkExtends[A](k: Kind[A]): Boolean = k match {
      case Primitive() => false
      case Base() => true
      case Extends(m) => checkExtends(m.extended.kind)
    }
    k match {
      case Primitive() => true
      case Base() => true
      case Extends(m) => checkExtends(m.extended.kind)
    }
  }

  def isObviouslyConsistent[A](k: Kind[A]): Boolean = k match {
    case Primitive() => true
    case Base() => true
    case _ => false
  }
}

// A Version of a datatype and its serialisation method
// getCopy/putCopy cannot be used directly, use safeGet/safePut instead. These
// methods handle migrations.
trait SafeCopy[A] {
  // While the Version type wraps Int, this is just a key and does not need to be ordered.
  // Values must be unique within a migration chain, and this is enforced at runtime.
  val version: Version = Version(0)

  // Defines what version information is stored with the value on serialisation. Primitives
  // have no version information, while extensions flag that migrations are necessary.
  val kind: Kind[A] = Base()

  // These methods define how the method is serialised, without worrying about version info
  val getCopy: Contained[Reads[A]]

  def putCopy(a: A): Contained[Write]

  lazy val availableVersions: List[Int] = kind match {
    case Primitive() => Nil
    case Base() => version.value :: Nil
    case Extends(m) => version.value :: m.extended.availableVersions
  }

  lazy val internalConsistency: Consistency = {
    if (Kind.isObviouslyConsistent(kind)) {
      Consistent
    } else if (availableVersions != availableVersions.distinct) {
      NotConsistent("Duplicate version tags: %s" format availableVersions.toString)
    } else if (!(Kind.validChain(kind))) {
      NotConsistent("Primitive types cannot be extended as they have no version tag.")
    } else {
      Consistent
    }
  }

  def constructGetterFromVersion(diskVersion: Version): Reads[A] = {
    if (version == diskVersion) {
      getCopy.unsafeGet
    } else {
      kind match {
        case Primitive() => error("cannot migrate from primitive types")
        case Base() => error("Cannot find getter for version " + version)
        case Extends(m) => {
          val extendedReads = m.extended.constructGetterFromVersion(diskVersion)
          extendedReads.map(m.migrate _)
        }
      }
    }
  }
}

object SafeCopy {
  def safeGet[A: SafeCopy]: Reads[A] = getSafeGet.join

  def safePut[A: SafeCopy](a: A): Write = {
    getSafePut >>= (putter => putter(a))
  }

  def extension[A, B](implicit sc: SafeCopy[A], m: Migrate[A, B]): Kind[A] = Extends(m)

  def base[A]: Kind[A] = Base()

  def primitive[A]: Kind[A] = Primitive()

  def getSafeGet[A](implicit sc: SafeCopy[A]): Reads[Reads[A]] = {
    sc.kind match {
      case Primitive() => sc.getCopy.unsafeGet.pure[Reads]
      case _ => implicitly[Reads[Version]] map (sc.constructGetterFromVersion(_))
    }
  }

  def getSafePut[A](implicit sc: SafeCopy[A]): WriteM[A => Write] = {
    checkConsistency({
      sc.kind match {
        case Primitive() => ((a: A) => sc.putCopy(a).unsafeGet).pure[WriteM]
        case _ => write(sc.version) >|> ((a: A) => sc.putCopy(a).unsafeGet).pure[WriteM]
      }
    })
  }

  def contain[A](a: A): Contained[A] = Contained(a)

  // PRIVATES?
  def checkConsistency[A, B, M[_]](ks: M[B])(implicit sc: SafeCopy[A], m: Monad[M]): M[B] = sc.internalConsistency match {
    case NotConsistent(msg) => error(msg) // TODO: this is `fail` on monad in SafeCopy.
    case Consistent => ks
  }

}

case class Version(value: Int) extends NewType[Int]

object Version {
  implicit def serialize: Format[Version] = new Format[Version] {
    def reads(in: sbinary.Input) = Version(implicitly[Format[Int]].reads(in))

    def writes(out: Output, value: Version) {
      implicitly[Format[Int]].writes(out, value.value)
    }
  }
}

case class Contained[A](unsafeGet: A)

trait Consistency

case object Consistent extends Consistency

case class NotConsistent(reason: String) extends Consistency

object Instances {
  import SafeCopy._

  implicit val StringSC = new SafeCopy[String] {
    override val kind = primitive[String]
    val getCopy = contain(read[String])

    def putCopy(s: String) = contain(write(s))
  }

  implicit def Tuple2SafeCopy[A, B](implicit sa: SafeCopy[A], sb: SafeCopy[B]): SafeCopy[(A, B)] = new SafeCopy[Tuple2[A, B]] {
    override val kind = primitive[(A, B)]
    val getCopy = contain {
      ((a: A, b: B) => (a, b)).lift[Reads].apply(safeGet[A], safeGet[B])
    }

    def putCopy(t2: (A, B)) = contain {
      safePut(t2._1) >|> safePut(t2._2)
    }
  }

  implicit def ListSC[A](implicit sc: SafeCopy[A]): SafeCopy[List[A]] = new SafeCopy[List[A]] {
    override val kind = primitive[List[A]]
    val getCopy: Contained[Reads[List[A]]] = contain {
      read[Int] >>= {length => (getSafeGet[A] >>= (_.replicateM(length)))}
    }

    def putCopy(xs: List[A]) = contain {
      write(xs.length) >|> {getSafePut[A] >>= (f => xs.traverse_(f))}
    }
  }
}

