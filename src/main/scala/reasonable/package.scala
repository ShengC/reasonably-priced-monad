import scalaz.Inject
import scalaz.Free
import scalaz.Free.FreeC
import scalaz.{ ~>, -\/, \/- }
import scalaz.Coproduct
import scalaz.NaturalTransformation
import scalaz.Coyoneda
import scalaz.Monad

package object reasonable {
  def lift[F[_], G[_], A](fa: F[A])(implicit I: Inject[F, G]): FreeC[G, A] =
    Free.liftFC(I.inj(fa))

  def or[F[_], G[_], H[_]](f: F ~> H, g: G ~> H): ({type cp[x]=Coproduct[F, G, x]})#cp ~> H =
    new NaturalTransformation[({type cp[x] = Coproduct[F, G, x]})#cp, H] {
      def apply[A](fa: Coproduct[F, G, A]): H[A] = fa.run match {
        case -\/(ff) => f(ff)
        case \/-(gg) => g(gg)
      }
    }

  type UserId = String
  type Password = String
  type Permission = String

  type W[A] = scalaz.Writer[Vector[String], A]
}
