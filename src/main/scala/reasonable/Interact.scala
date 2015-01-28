package reasonable

import scalaz.~>
import scalaz.Id
import scalaz.Inject
import scalaz.Free
import scalaz.Writer
import scalaz.Functor

sealed trait Interact[A]

case class Ask(prompt: String) extends Interact[String]

case class Tell(msg: String) extends Interact[Unit]

object Console extends (Interact ~> Id.Id) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) =>
      println(prompt)
      readLine
    case Tell(msg) =>
      println(msg)
  }
}

object ConsoleW extends (Interact ~> W) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) =>
      println(prompt)
      Writer(Vector(prompt), readLine)
    case Tell(msg) =>
      println(msg)
      Writer(Vector.empty, ())
  }
}

class Interacts[F[_]](implicit I: Inject[Interact, F]) {
  def tell(msg: String): Free.FreeC[F, Unit] = lift(Tell(msg))
  def ask(prompt: String): Free.FreeC[F, String] = lift(Ask(prompt))
}

object Interacts {
  implicit def interacts[F[_]](implicit ev: Inject[Interact, F]) =
    new Interacts
}

object InteractApp extends App {

  import scalaz.Coyoneda

  implicit def lift[F[_], A](f: F[A]) =
    Free.liftFC(f)

  val prg = for {
    first <- Ask("What's your first name")
    last <- Ask("What's your last name")
    _ <- Tell(s"Hello, $first, $last!")
  } yield ()

  import scalaz.std.vector._

  val driver = Coyoneda.liftTF(ConsoleW)

  val x = prg.foldMap(driver)
  println(x.run._1)
}
