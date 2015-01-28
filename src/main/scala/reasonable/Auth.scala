package reasonable

import scalaz.{ ~>, Inject, Free, Id }

case class User(id: UserId)

sealed trait Auth[A]

case class Login(userId: UserId, password: Password) extends Auth[Option[User]]

case class HasPermission(user: User, permission: Permission) extends Auth[Boolean]

class Auths[F[_]](implicit I: Inject[Auth, F]) {
  def login(id: UserId, pwd: Password): Free.FreeC[F, Option[User]] =
    lift(Login(id, pwd))
  def hasPermission(u: User, p: Permission): Free.FreeC[F, Boolean] =
    lift(HasPermission(u, p))
}

object Auths {
  implicit def instance[F[_]](implicit I: Inject[Auth, F]): Auths[F] =
    new Auths
}

object TestAuth extends (Auth ~> Id.Id) {
  def apply[A](a: Auth[A]) = a match {
    case Login(uid, pwd) =>
      if (uid == "john.snow" && pwd == "Ghost")
        Some(User("john.snow"))
      else None
    case HasPermission(u, _) =>
      u.id == "john.snow"
  }
}
