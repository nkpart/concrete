package concrete

import sbinary.Operations
import scalaz._
import Scalaz._
import concrete.SBinaryScalaz._
import concrete.SafeCopy._

object Example {

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

  // Next version of the data introduces a type:
  case class Contact(name: Name, address: Address, phone: Phone)

  object Contact {
    implicit val safecopy: SafeCopy[Contact] = new SafeCopy[Contact] {
      val getCopy = contain {
        for (name <- safeGet[Name]; address <- safeGet[Address]; phone <- safeGet[Phone]) yield {Contact(name, address, phone)}
      }

      def putCopy(c: Contact) = contain {
        safePut(c.name) >|> safePut(c.address) >|> safePut(c.phone)
      }
    }
  }

  // And a modification of the address book to the Contact type
  case class Contacts(contacts: List[Contact])

  object Contacts {
    // TODO: Fix the initialisation order dependencies of this. If the Migrate
    // instance is declared after the SafeCopy instance, then null pointers
    // abound.

    // First, declare how to get here from the old version that would be serialised
    implicit val m1 = new Migrate[Contacts, Contacts_v0] {
      val extended = implicitly[SafeCopy[Contacts_v0]]

      def migrate(old: Contacts_v0): Contacts = Contacts {
        old.contacts map {case (name, address) => Contact(name, address, "555-5555")}
      }
    }

    implicit val safecopy: SafeCopy[Contacts] = new SafeCopy[Contacts] {
      override val version = Version(2)
      override val kind = extension[Contacts, Contacts_v0]
      val getCopy = contain(safeGet[List[Contact]].map(Contacts(_)))
      def putCopy(cs: Contacts) = contain(safePut(cs.contacts))
    }
  }

  def main(args: Array[String]) {
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
