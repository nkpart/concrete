package concrete

import scalaz._
import Scalaz._
object Concrete {

  trait Migrate[A, From] {
    val extended: SafeCopy[From]

    def migrate(ma: From): A
  }

  trait Kind[A]
  case class Primitive[A]() extends Kind[A]
  case class Base[A]() extends Kind[A]
  case class Extends[A, From](m: Migrate[A, From], f: Proxy[From]) extends Kind[A]

  // TODO: replace with whatever from sbinary
  trait Get[A]
  trait Serialize[A] {
    val get: Get[A]
    val putter: A => Put
  }

  object Get {
    implicit val GetPure: Pure[Get] = null
    implicit val GetBind: Bind[Get] = null
    implicit val GetMonad: Monad[Get] = null
    implicit val GetFunctor: Functor[Get] = null
  }

  trait Put

  trait SafeCopy[A] {
    val version: Version[A] = Version(0)
    val kind: Kind[A] = Base()

    val getCopy: Contained[Get[A]]
    def putCopy(a: A): Contained[Put]

    val internalConsistency: Consistency[A] = {
      implicit val _ = this
      lazy val proxy: Proxy[A] = proxyFromConsistency(ret)
      lazy val ret: Consistency[A] = computeConsistency(proxy)
      ret
    }
  }

  def constructGetterFromVersion[A: SafeCopy](diskVersion: Version[A], proxy: Proxy[A]): Get[A] = {
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

  def safeGet[A: SafeCopy]: Get[A] = getSafeGet.join

  def getSafeGet[A](implicit sc: SafeCopy[A]): Get[Get[A]] = {
    val proxy = Proxy[A]()
    kindFromProxy(proxy) match {
      case Primitive() => unsafeUnPack(sc.getCopy).pure[Get]
      case _ => implicitly[Serialize[Version[A]]].get map { v =>
        constructGetterFromVersion(v, proxy)
      }
    }
  }

  // TODO safePut
  // TODO getSafePut


  def extension[A, B](implicit sc: SafeCopy[A], m: Migrate[A, B]): Kind[A] = Extends(m, Proxy())

  def base[A]: Kind[A] = Base()

  def primitive[A]: Kind[A] = Primitive()

  case class Version[A](value: Int) extends NewType[Int]

  def castVersion[A, B](v: Version[A]): Version[B] = Version(v.value)

  object Version {
    implicit def serialize[A]: Serialize[Version[A]] = null
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
}
