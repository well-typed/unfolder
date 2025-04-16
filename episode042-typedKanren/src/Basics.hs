
-- | Haskell Unfolder episode 42: logic programming in typedKanren
--
-- Functional programming is programming with mathematical /functions/, mapping
-- inputs to outputs. By constrast, /logic/ programming, perhaps best known
-- from the language Prolog, is programming with mathematical /relations/
-- between values, without making a distinction between inputs and outputs. In
-- this two-year anniversary episode of the Haskell Unfolder we take a look at
-- @typedKanren@, an embedding of the logic programming language @miniKanren@ in
-- Haskell. We will see how we can use it to write a type checker for a simple
-- functional language in a few lines of code.
--
-- References:
--
-- * @typedKanren@ library
--     <https://github.com/SnejUgal/typedKanren>
-- * paper on @typedKanren@ by Nikolai Kudasov and Artem Starikov
--     <https://arxiv.org/abs/2408.03170>
module Basics where

import Control.Applicative

import Kanren.Core
import Kanren.Goal
import Kanren.Instances ()

-- | Relation between two integers
--
-- Try:
--
-- > run $ \()     -> example1 1 2
-- > run $ \()     -> example1 1 1
-- > run $ \x      -> example1 1 x
-- > run $ \(x, y) -> example1 x y
example1 :: Term Int -> Term Int -> Goal ()
example1 x y = x === y

-- | Predicate on integers (one-place relation), using disjunction
--
-- Try:
--
-- > run $ \() -> example2 1
-- > run $ \() -> example2 2
-- > run $ \() -> example2 3
-- > run $ \x  -> example2 x
example2 :: Term Int -> Goal ()
example2 x = x === 1 <|> x === 2

-- | Using conjunction
--
-- Try:
--
-- > run $ \(x, y) -> example3 x y
example3 :: Term Int -> Term Int -> Goal ()
example3 x y = do
    x === y
    y === 1
