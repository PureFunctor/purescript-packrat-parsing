-- | The following module is based on the Basic Expression Parser
-- | example which can be found in Appendix A, Section A.1 of:
-- |
-- | Packrat Parsing: a Practical Linear-Time Algorithm with
-- | Backtracking by Bryan Ford
-- |
-- | https://pdos.csail.mit.edu/~baford/packrat/thesis/thesis.pdf
-- |
-- | The paper originally implements its packrat parsers in Haskell,
-- | which uses lazy evaluation unlike PureScript, which requires the
-- | use of the `lazy` package to successfully implement laziness.
-- |
-- | See Also: Section 3.1.4
module Packrat.Basic where

import Prelude

import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..))
import Data.String as String
import Unsafe.Coerce (unsafeCoerce)


undefined :: forall a. a
undefined = unsafeCoerce unit


data Result v
  = Parsed v Derivs
  | NoParse

derive instance Eq v => Eq (Result v)

instance Show v => Show (Result v) where
  show (Parsed v _) = "Parsed " <> show v
  show NoParse = "NoParse"


type Derivs =
  { dvAdditive :: Lazy (Result Int)
  , dvMultitive :: Lazy (Result Int)
  , dvPrimary :: Lazy (Result Int)
  , dvDecimal :: Lazy (Result Int)
  , dvChar :: Lazy (Result String)
  }


-- | Evaluate an expression and return the unpackaged result,
-- | ignoring any unparsed remainder.
eval :: String -> Int
eval s =
  case force (force (parse s)).dvAdditive of
    Parsed v _ -> v
    NoParse -> undefined


-- | Construct a (lazy) parse result structure for an input string,
-- | in which any result can be computed in linear time
-- | with respect to the length of the input.
parse :: String -> Lazy Derivs
parse s = d
  where
    d = defer \_ ->
      { dvAdditive : add
      , dvMultitive : mult
      , dvPrimary : prim
      , dvDecimal : dec
      , dvChar : chr
      }
    add = defer \_ -> force (pAdditive (force d))
    mult = defer \_ -> force (pMultitive (force d))
    prim = defer \_ -> force (pPrimary (force d))
    dec = defer \_ -> force (pDecimal (force d))

    chr = defer \_ ->
      case String.uncons s of
        Just { head, tail } -> Parsed (String.singleton head) (force (parse tail))
        Nothing -> undefined


-- | Parse an additive-precedence expression
pAdditive :: Derivs -> Lazy (Result Int)
pAdditive d = defer \_ -> alt1
  where
    -- | Additive <- Multitive '+' Additive
    alt1 =
      case force $ d.dvMultitive of
        Parsed vleft d' ->
          case force $ d'.dvChar of
            Parsed "+" d'' ->
              case force $ d''.dvAdditive of
                Parsed vright d''' -> Parsed (vleft + vright) d'''
                _ -> alt2
            _ -> alt2
        _ -> alt2


    -- | Additive <- Multitive
    alt2 =
      case force $ d.dvMultitive of
        Parsed v d' -> Parsed v d'
        NoParse -> NoParse


-- | Parse a multiplicative-precedence expression
pMultitive :: Derivs -> Lazy (Result Int)
pMultitive d = defer \_ -> alt1
  where
    -- | Multitive <- Primary '*' Multitive
    alt1 =
      case force $ d.dvPrimary of
        Parsed vleft d' ->
          case force $ d'.dvChar of
            Parsed "*" d'' ->
              case force $ d''.dvMultitive of
                Parsed vright d''' -> Parsed (vleft * vright) d'''
                _ -> alt2
            _ -> alt2
        _ -> alt2

    -- | Multitive <- Primary
    alt2 =
      case force $ d.dvPrimary of
        Parsed v d' -> Parsed v d'
        NoParse -> NoParse


-- | Parse a primary expression
pPrimary :: Derivs -> Lazy (Result Int)
pPrimary d = defer \_ -> alt1
  where
    -- | Primary <- "(" Additive ")"
    alt1 =
      case force $ d.dvChar of
        Parsed "(" d' ->
          case force $ d'.dvAdditive of
            Parsed v d'' ->
              case force $ d''.dvChar of
                Parsed ")" d''' -> Parsed v d'''
                _ -> alt2
            _ -> alt2
        _ -> alt2

    -- | Primary <- Decimal
    alt2 =
      case force $ d.dvDecimal of
        Parsed v d' -> Parsed v d'
        NoParse -> NoParse


-- | Parse a decimal digit
pDecimal :: Derivs -> Lazy (Result Int)
pDecimal d = defer \_ ->
  case (force d.dvChar) of
    Parsed "0" d' -> Parsed 0 d'
    Parsed "1" d' -> Parsed 1 d'
    Parsed "2" d' -> Parsed 2 d'
    Parsed "3" d' -> Parsed 3 d'
    Parsed "4" d' -> Parsed 4 d'
    Parsed "5" d' -> Parsed 5 d'
    Parsed "6" d' -> Parsed 6 d'
    Parsed "7" d' -> Parsed 7 d'
    Parsed "8" d' -> Parsed 8 d'
    Parsed "9" d' -> Parsed 9 d'
    _ -> NoParse
