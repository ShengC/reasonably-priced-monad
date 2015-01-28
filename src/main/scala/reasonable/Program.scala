package reasonable

//import scalaz.Id
//import scalaz.Coyoneda
//import scalaz.~>
//import scalaz.Coproduct
//import scalaz.Free
//import scalaz.Monad
//import scalaz.Inject._
//
//import scalaz.syntax.std.option._

import scalaz._
import Scalaz._

object Program extends App {
  type Prg[A] = Coproduct[Auth, Interact, A]
  type CoyoPrg[A] = Coyoneda[Prg, A]
  type FreeCoyo[A] = Free[CoyoPrg, A]

  val KnowSecret = "KnowSecret"

  def point[A](a: => A): Free.FreeC[Prg, A] =
    Monad[FreeCoyo].point(a)

  def prg(implicit I: Interacts[Prg], A: Auths[Prg]) = {
    import I._
    import A._

    for {
      uid <- ask("What's your User Id")
      pwd <- ask("Password, please")
      u <- login(uid, pwd)
      b <- u.cata(none = point(false), some = hasPermission(_, KnowSecret))

      _ <- if (b) tell("UUDDLRLRBA") else tell("Go away")
    } yield ()
  }

  val interpreters: Prg ~> Id.Id = or(TestAuth, Console)
  val coyoint: ({type f[x] = Coyoneda[Prg, x]})#f ~> Id.Id = Coyoneda.liftTF(interpreters)

  val runApp = prg.mapSuspension(coyoint)

  prg.foldMap(coyoint)
}
