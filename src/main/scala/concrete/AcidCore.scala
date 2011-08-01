package concrete

import scalaz._
import Scalaz._

object AcidCore {
  type Tag = String
  type Tagged[A] = (Tag, A)

  trait Method[E, R, S] {
    val eventSc: SafeCopy[E]
    val resultSc: SafeCopy[R]

    def tag: Tag
  }

  type MethodMap[S] = Map[Tag, MethodContainer[S]]

  // TODO coreState :: MVar s
  class Core[S](var coreState: S, val methodMap: MethodMap[S]) {

  }

  object Core {
    def mk[S](methods: List[MethodContainer[S]], initial: S): Core[S] = new Core(initial, mkMethodMap(methods))

    def mkMethodMap[S](methods: List[MethodContainer[S]]): MethodMap[S] = {
      def methodType[A](m: MethodContainer[A]): Tag = m match { case PackedMethod(method, f) => method.tag }
      methods.foldLeft(Map.empty: MethodMap[S]) { (map, container) => map + ((methodType(container), container))}
    }
  }

  trait MethodContainer[S]
  case class PackedMethod[E, R, S](method: Method[E, R, S], f: E => State[S, R]) extends MethodContainer[S]
}
