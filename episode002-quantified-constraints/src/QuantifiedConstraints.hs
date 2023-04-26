{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module QuantifiedConstraints where

import Codec.Borsh hiding (deserialiseBorsh)
import Codec.Borsh qualified as Borsh
import Control.Applicative
import Control.Monad
import Control.Monad.Combinators.Expr
import Crypto.Cipher.AES qualified as Cryptonite
import Crypto.Cipher.Types qualified as Cryptonite
import Crypto.Error qualified as Cryptonite
import Crypto.Random qualified as Cryptonite
import Data.ByteString qualified as Strict
import Data.ByteString.Lazy qualified as Lazy
import Data.Functor.Product
import Data.Kind
import Data.Proxy
import Data.Type.Equality
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

{-------------------------------------------------------------------------------
  Use case 1: monad transformers

  This is from the paper "Quantified class constraints"
  <https://www.pure.ed.ac.uk/ws/files/42495988/quantcc.pdf>.

  This definition is now used in the @transformers@ package, though this does
  have some downsides. For example, see
  <https://mail.haskell.org/pipermail/ghc-devs/2023-April/021206.html>.

  Note that the 'MonadTrans' instance for 'Stack' is really impossible
  without quantified constraints: without it, we would somehow need to add
  a constraint that @t2 m@ is a monad, but we don't have access to that @m@
  in the instance head.

  It's worth noting that trying to /define/ the instance (instead of using a
  quantified constraint) is both impossible and undesirable:

  > instance (Monad m, MonadTrans t) => Monad (t m) where

  This instance cannot be defined, and even if it could, it would then rule
  out any other more specific `Monad` instance of the form @x y@.
-------------------------------------------------------------------------------}

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a -> t m a

newtype Stack t1 t2 m a = Stack (t1 (t2 m) a)

instance (MonadTrans t1, MonadTrans t2) => MonadTrans (Stack t1 t2) where
  lift = Stack . lift . lift

-- By manually specifying the context for these instances, we get slightly
-- nicer types, but we could also just use a standard deriving clause.

deriving newtype instance
  (MonadTrans t1, MonadTrans t2, Monad m) => Functor     (Stack t1 t2 m)
deriving newtype instance
  (MonadTrans t1, MonadTrans t2, Monad m) => Applicative (Stack t1 t2 m)
deriving newtype instance
  (MonadTrans t1, MonadTrans t2, Monad m) => Monad       (Stack t1 t2 m)

{-------------------------------------------------------------------------------
  Use case 2: GADTs

  This is a (simplified) snippet from @quickcheck-dynamic@
-------------------------------------------------------------------------------}

class ( forall a. Show (Action state a)
      ) => StateModel (state :: Type) where
  data Action state :: Type -> Type

data RestServer

instance StateModel RestServer where
  data Action RestServer :: Type -> Type where
    AddUser     :: String -> Action RestServer Int
    GetUser     :: Int -> Action RestServer String
    GetAllUsers :: Action RestServer [String]

deriving instance Show (Action RestServer a)

showAction :: StateModel state => state -> Action state a -> String
showAction _ = show

{-------------------------------------------------------------------------------
  Use case 3: existentials
-------------------------------------------------------------------------------}

data Expr a where
  Val   :: a -> Expr a
  Add   :: Expr Int -> Expr Int -> Expr Int
  Equal :: Expr Int -> Expr Int -> Expr Bool
  If    :: Expr Bool -> Expr a -> Expr a -> Expr a

deriving instance Show a => Show (Expr a)

eval :: Expr a -> a
eval (Val x)       = x
eval (Add e1 e2)   = eval e1 + eval e2
eval (Equal e1 e2) = eval e1 == eval e2
eval (If c t f)    = if eval c then eval t else eval f

data SomeShowable (f :: Type -> Type) where
  ExistsShowable :: Show a => f a -> SomeShowable f

-- Note the different nature of this constraint; previously we had
--
-- > forall a. Show (Action state a)
--
-- Now we have
--
-- > forall a. Show a => Show (f a)
--
-- Neither type is "better" or "more general"; the first is a very strong
-- requirement that says that the action must be showable even if @a@ is not;
-- the second is weaker, it says that @f a@ only needs to be showable if @a@ is,
-- but note that for an action, we might have that the Action is showable even
-- if @a@ is not!
deriving instance (forall a. Show a => Show (f a)) => Show (SomeShowable f)

parseExpr :: Parser (SomeShowable Expr)
parseExpr = pExpr

{-------------------------------------------------------------------------------
  Use case 4: type families

  See <https://gitlab.haskell.org/ghc/ghc/-/issues/14860> for a detailed thread
  on this topic. The solution we use here is due to Iceland_jack and Ryan Scott.

  Key insight is that it's fine to refer to type families in constraints as
  long as that does not involve quantified variables; we therefore introduce
  a "class newtype", which has an additional type parameter @a@; /it/ refers
  to the 'Enc' type family, and then the quantified constraint in 'Encryption'
  does not need to.
-------------------------------------------------------------------------------}

class    Eq (Enc e a) => EqEnc e a
instance Eq (Enc e a) => EqEnc e a

class ( Eq (Key e)
      , forall a. Eq a => EqEnc e a
      ) => Encryption e where
  type Key e :: Type
  type Enc e :: Type -> Type

  encrypt :: ToBorsh   a => Proxy e -> Key e -> a -> Enc e a
  decrypt :: FromBorsh a => Proxy e -> Key e -> Enc e a -> Maybe a

compareEnc :: (Encryption e, Eq a) => Proxy e -> Enc e a -> Enc e a -> Bool
compareEnc _ = (==)

data AES

instance Encryption AES where
  type Key AES = Strict.ByteString
  type Enc AES = Const Strict.ByteString

  encrypt _ = encryptWithCryptonite
  decrypt _ = decryptWithCryptonite

data Mock

type MockKey   = Word
data MockEnc a = MockEnc MockKey a

deriving stock instance Eq a => Eq (MockEnc a)

instance Encryption Mock where
  type Key Mock = MockKey
  type Enc Mock = MockEnc

  encrypt _ = MockEnc
  decrypt _ = \key (MockEnc key' a) -> do
                guard $ key == key'
                return a

{-------------------------------------------------------------------------------
  EVERYTHING BELOW THIS LINE IS JUST TO MAKE THE EXAMPLE SELF-CONTAINED. IT DOES
  NOT INCLUDE ANYTHING THAT IS SPECIFIC TO QUANTIFIED CONSTRAINTS.
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Example from <https://gitlab.haskell.org/ghc/ghc/-/issues/15347>

  Just to demonstrate that the workaround works for this example, too.
-------------------------------------------------------------------------------}

data Dict (c :: Constraint) where
  Dict :: c => Dict c

class Ob p a

class    Ob (Cod f) (f a) => ObCod f a
instance Ob (Cod f) (f a) => ObCod f a

class (forall a. Ob (Dom f) a => ObCod f a) => Functor' f where
  type Dom f
  type Cod f

liftOb :: forall f a. (Functor' f, Ob (Dom f) a) => Dict (Ob (Cod f) (f a))
liftOb = Dict

{-------------------------------------------------------------------------------
  Functions required to actually try out the crypto stuff

  NOTE: We are using null IVs here. You probably don't want to do this in real
  applications!
-------------------------------------------------------------------------------}

tryAES :: IO ()
tryAES = do
    key1 :: Strict.ByteString <- Cryptonite.getRandomBytes 32
    key2 :: Strict.ByteString <- Cryptonite.getRandomBytes 32
    print $ decrypt (Proxy @AES) key1 . encrypt (Proxy @AES) key1 $ "hello"
    print $ decrypt (Proxy @AES) key2 . encrypt (Proxy @AES) key1 $ "hello"

tryMock :: IO ()
tryMock = do
    let key1, key2 :: MockKey
        key1 = 1
        key2 = 2
    print $ decrypt (Proxy @Mock) key1 . encrypt (Proxy @Mock) key1 $ "hello"
    print $ decrypt (Proxy @Mock) key2 . encrypt (Proxy @Mock) key1 $ "hello"

deserialiseBorsh :: FromBorsh a => Lazy.ByteString -> Maybe a
deserialiseBorsh = either (const Nothing) Just . Borsh.deserialiseBorsh

encryptWithCryptonite ::
     ToBorsh a
  => Strict.ByteString -> a -> Const Strict.ByteString a
encryptWithCryptonite key =
      Const
    . cryptoniteAES key
    . Lazy.toStrict
    . serialiseBorsh

decryptWithCryptonite ::
     FromBorsh a
  => Strict.ByteString -> Const Strict.ByteString a -> Maybe a
decryptWithCryptonite key =
      deserialiseBorsh
    . Lazy.fromStrict
    . cryptoniteAES key
    . getConst

cryptoniteAES :: Strict.ByteString -> Strict.ByteString -> Strict.ByteString
cryptoniteAES key =
    aux . Cryptonite.throwCryptoError $ Cryptonite.cipherInit key
  where
    aux :: Cryptonite.AES256 -> Strict.ByteString -> Strict.ByteString
    aux cipher = Cryptonite.ctrCombine cipher Cryptonite.nullIV

{-------------------------------------------------------------------------------
  Parser for 'Expr'

  This consists of two phases: a parser for untyped expressions and then
  a type checker.
-------------------------------------------------------------------------------}

pExpr :: Parser (SomeShowable Expr)
pExpr = do
    u <- pUExpr
    case typecheck u of
      Just (ExistsShowable (Pair _ e)) -> return $ ExistsShowable e
      _ -> fail "type error"

data Typ a where
  TInt  :: Typ Int
  TBool :: Typ Bool

instance TestEquality Typ where
  testEquality TInt  TInt  = Just Refl
  testEquality TBool TBool = Just Refl
  testEquality _     _     = Nothing

typecheck :: UExpr -> Maybe (SomeShowable (Product Typ Expr))
typecheck (UInt  x)    = Just $ ExistsShowable $ Pair TInt  $ Val x
typecheck (UBool x)    = Just $ ExistsShowable $ Pair TBool $ Val x
typecheck (UAdd u1 u2) = do
    ExistsShowable (Pair t1 e1) <- typecheck u1
    ExistsShowable (Pair t2 e2) <- typecheck u2
    case (t1, t2) of
      (TInt, TInt) -> Just $ ExistsShowable $ Pair TInt $ Add e1 e2
      _            -> Nothing
typecheck (UEqual u1 u2) = do
    ExistsShowable (Pair t1 e1) <- typecheck u1
    ExistsShowable (Pair t2 e2) <- typecheck u2
    case (t1, t2) of
      (TInt, TInt) -> Just $ ExistsShowable $ Pair TBool $ Equal e1 e2
      _            -> Nothing
typecheck (UIf uc ut uf) = do
    ExistsShowable (Pair tc ec) <- typecheck uc
    ExistsShowable (Pair tt et) <- typecheck ut
    ExistsShowable (Pair tf ef) <- typecheck uf
    case tc of
      TBool -> do Refl <- testEquality tt tf
                  Just $ ExistsShowable $ Pair tt $ If ec et ef
      _     -> Nothing

{-------------------------------------------------------------------------------
  Parsing untyped expressions
-------------------------------------------------------------------------------}

-- | Untyped expressions (the result of parsing)
data UExpr =
    UInt Int
  | UBool Bool
  | UAdd UExpr UExpr
  | UEqual UExpr UExpr
  | UIf UExpr UExpr UExpr
  deriving (Show)

type Parser = Parsec Void String

pUExpr :: Parser UExpr
pUExpr =
    makeExprParser pTerm operatorTable
  where
    pTerm :: Parser UExpr
    pTerm = asum [
          parens pUExpr
        , UInt        <$> lexeme L.decimal
        , UBool True  <$  pKeyword "true"
        , UBool False <$  pKeyword "false"
        , UIf         <$  pKeyword "if"
                      <*> pUExpr
                      <*  pKeyword "then"
                      <*> pUExpr
                      <*  pKeyword "else"
                      <*> pUExpr
        ]

    operatorTable :: [[Operator Parser UExpr]]
    operatorTable = [
          [ binary "+"  UAdd   ]
        , [ binary "==" UEqual ]
        ]

    binary :: String -> (UExpr -> UExpr -> UExpr) -> Operator Parser UExpr
    binary name f = InfixL (f <$ symbol name)

    pKeyword :: String -> Parser ()
    pKeyword keyword = void $
        lexeme (string keyword <* notFollowedBy alphaNumChar)

    parens :: Parser a -> Parser a
    parens = between (symbol "(") (symbol ")")

    -- lexing

    sc :: Parser ()
    sc = L.space space1 empty empty

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc

    symbol :: String -> Parser ()
    symbol = void . L.symbol sc
