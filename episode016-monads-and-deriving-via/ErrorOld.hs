-- Monad instance as it could be defined prior to the Applicative Monad Proposal
module ErrorOld where

data Error a =
    Ok a
  | Fail String

instance Monad Error where
  return = Ok

  Ok a     >>= k = k a
  Fail msg >>= _ = Fail msg
