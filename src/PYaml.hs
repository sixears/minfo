{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

{- | YAML printer, producing 'nicer' (to my eyes) output than `Data.Aeson.Yaml`
 -}

module PYaml
  ( pyaml, tests )
where

import Prelude  ( (-), error, fromIntegral )

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Array, Bool, Null, Number, Object, String ) )

-- base --------------------------------

import Data.Bool       ( Bool( True, False ) )
import Data.Bifunctor  ( first, second )
import Data.Either     ( Either( Right ) )
import Data.Foldable   ( maximum )
import Data.Function   ( ($) )
import Data.Functor    ( fmap )
import Data.List       ( sortOn )
import Data.Ord        ( max )
import Data.String     ( String )
import Data.Tuple      ( fst )
import GHC.Exts        ( fromList, toList )
import System.Exit     ( ExitCode )
import System.IO       ( IO )
import Text.Show       ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- ListLike ----------------------------

import qualified Data.ListLike

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Monoid   ( ю )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Tasty    ( (≟) )

-- scientific --------------------------

import Data.Scientific  ( Scientific )

-- tasty -------------------------------

import Test.Tasty  ( TestName, TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import qualified  Data.Text  as  Text

import Data.Text  ( Text, init, intercalate, lines )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

-- yaml --------------------------------

import Data.Yaml  ( ToJSON( toJSON ), decodeEither', encode, object )

--------------------------------------------------------------------------------

array ∷ [Value] → Value
array = Array ∘ fromList

arrayN ∷ [Scientific] → Value
arrayN = array ∘ fmap Number

objectTs ∷ [(Text,Text)] → Value
objectTs = object ∘ fmap (second String)

convStringLike ∷ (Data.ListLike.StringLike α,Data.ListLike.StringLike β) ⇒ α → β
convStringLike = Data.ListLike.fromString ∘ Data.ListLike.toString

indent ∷ ℕ → [Text] → [Text]
indent n = fmap (spaces n ⊕)

isCompoundValue ∷ Value → Bool
isCompoundValue (Object _) = True
isCompoundValue (Array  _) = True
isCompoundValue _          = False

tlength ∷ Text → ℕ
tlength = fromIntegral ∘ Text.length

spaces ∷ ℕ → Text
spaces n = Text.replicate (fromIntegral n) " "

yamlText ∷ Text → Text
yamlText = let safeInit "" = ""
               safeInit t  = init t
            in safeInit ∘ convStringLike ∘ encode

pyaml_ ∷ Value → Text
pyaml_ Null      = "~"
pyaml_ (Bool _ ) = error "not implemented Bool"
pyaml_ (Number n ) = [fmt|%f|] n
pyaml_ (String t) = yamlText t
pyaml_ (Array (toList → [])) = "[]"
pyaml_ (Array (toList → xs)) =
  intercalate "\n" $ ю [ ("- " ⊕ t) : (("  " ⊕) ⊳ ts) | x ← toList xs, let (t:ts) = lines (pyaml_ x) ]

pyaml_ (Object m) =
    let maxLen ∷ ℕ
        maxLen = fromIntegral (maximum $ Text.length ⊳ HashMap.keys m)
        pad ∷ Text → Text
        pad t = t ⊕ spaces (fromIntegral (maxLen - tlength t) `max` 0)
     in case length m of
          0 → "{}"
          _ → intercalate "\n" $
                ю [ if isCompoundValue v
                    then ( [fmt|%t :|] (pad k) : indent 2 (t:ts) )
                    else [ [fmt|%t : %t|] (pad k) t ]
                  | (k,v) ← sortOn fst (HashMap.toList m)
                  , let (t:ts) = lines (pyaml_ v)
                  ]

pyaml ∷ ToJSON α ⇒ α → Text
pyaml = pyaml_ ∘ toJSON

pyamlTests ∷ TestTree
pyamlTests =
  let foo  = "foo" ∷ Text
      _bob = "'bob" ∷ Text
      bar  = "bar" ∷ Text
      x    = "x" ∷ Text
      y    = "y" ∷ Text
      quux = "quux" ∷ Text
      tlist = [] ∷ [Text]

      decodeText ∷ Text → Either String Value
      decodeText = first show ∘ decodeEither' ∘ convStringLike

      check ∷ TestName → Text → Value → TestTree
      check name expect val =
        testGroup name
                  [ testCase "expect" $ expect ≟ pyaml val
                  , testCase "parse"  $ Right val ≟ decodeText (pyaml val)]

   in testGroup "pyaml"
                [ check "foo"  foo       (String foo)
                  -- I would like to fix this, but not today
                , check "y"     "'y'"     (String y)
                , check "bo'b"  "bo'b"    (String "bo'b")
                , check "'bob"  "'''bob'" (String _bob)
                , check "\"bob" "'\"bob'" (String "\"bob")

                , check "7" "7" (Number 7)

                , check "list0" "[]"            (array (String ⊳ tlist))
                , check "list1" "- 1"           (arrayN [ 1 ])
                , check "list2" "- 1\n- 1"      (arrayN [ 1, 1 ])
                , check "list3" "- 1\n- 1\n- 2" (arrayN [ 1, 1, 2 ])

                , check "map0" "{}"             (object ([]∷[(Text,Value)]))
                , check "map1" "foo : bar"      (object [(foo,String bar)])
                , check "map1'" "foo : '''bob'" (object [(foo,String _bob)])
                , check "map2" "foo  : bar\nquux : 'y'\nx    : 'y'"
                               (objectTs [(foo,bar),(x,y),(quux,y)])

                , check "list of lists"
                        (intercalate "\n" [ "- - 1"
                                          , "  - 1"
                                          , "  - 2"
                                          , "- - 3"
                                          , "  - 5"
                                          , "- - 8"
                                          ])
                        (array [ arrayN [1,1,2], arrayN [3,5], arrayN [8] ])


                , check "map of maps"
                        (intercalate "\n" [ "one :"
                                          , "  foo  : bar"
                                          , "  quux : 'y'"
                                          , "  x    : 'y'"
                                          , "two :"
                                          , "  foo : bar"
                                          , "  x   : 'y'"
                                          ])
                        (object [ ("one", objectTs [(foo,bar), (x,y), (quux,y)])
                                , ("two", objectTs [(foo,bar), (x,y)]) ]
                        )

                , check "list of maps"
                        (intercalate "\n" [ "- foo  : bar"
                                          , "  quux : 'y'"
                                          , "  x    : 'y'"
                                          , "- foo : bar"
                                          , "  x   : 'y'"
                                          ])
                        (array [ objectTs [(foo,bar),(x,y),(quux,y)]
                               , objectTs [(foo,bar),(x,y)] ])

                , check "map of lists"
                        (intercalate "\n" [ "foo  :"
                                          , "  - 1"
                                          , "  - 1"
                                          , "  - 2"
                                          , "quux :"
                                          , "  - 8"
                                          , "x    :"
                                          , "  - 3"
                                          , "  - 5"
                                          ])
                        (object [ (foo, arrayN [1,1,2])
                                , (x, arrayN [3,5]), (quux, arrayN[8]) ])
                ]


--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "PYaml" [ pyamlTests ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
