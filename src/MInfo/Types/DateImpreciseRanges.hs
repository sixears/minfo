{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Date range, utilizing DateImprecise for varying precisions -}

module MInfo.Types.DateImpreciseRanges
  ( DateImpreciseRanges, {- dateImpreciseRanges, -} tests )
where

import Prelude  ( undefined )

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Number, String ), typeMismatch )

-- base --------------------------------

import Control.Monad       ( Monad, fail, foldM, return )
import Data.Bool           ( Bool( False, True ) )
import Data.Either         ( Either( Left, Right ) )
import Data.Eq             ( Eq )
import Data.Foldable       ( Foldable )
import Data.Function       ( ($) )
import Data.Functor        ( fmap )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )
import Data.Maybe          ( Maybe( Just ) )
import Data.Ord            ( (<) )
import Data.String         ( String )
import Data.Tuple          ( uncurry )
import System.Exit         ( ExitCode )
import System.IO           ( IO )
import Text.Show           ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Ord.Unicode       ( (≤) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed, Malformed ), Printable( print )
                     , Textual( textual )
                     , parseText, fromString, toString, toText
                     )

-- monaderror-io -----------------------

import MonadError  ( mapMError )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE  ( (⋗) )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec' )

-- parsers ------------------------------

import Text.Parser.Char  ( CharParsing, string )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( propInvertibleText, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Gen, suchThat, testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.Util                 ( __fromString, mkQuasiQuoterExp, tries )
import MInfo.YamlPlus             ( unYaml' )
import MInfo.YamlPlus.Error       ( YamlParseError )

import MInfo.Types.DateImprecise  ( DateImprecise
                                  , pattern DayDate , pattern MonthDate
                                  , pattern YearDate
                                  , dateDay, dateDay', dateDayM, dateDay_
                                  , dateMonth, dateImprecise, dateYear
                                  )
import MInfo.Types.DateImpreciseRange
                                  ( DateImpreciseRange, dateImpreciseRange )
import MInfo.Types.Date.Error     ( AsDateError_, DateError_, DateErrorImprecise
                                  , dateRangeError, dateRangeError_
                                  , dateRangesOverlap, emap
                                  )
import MInfo.Types.DayBounds      ( DayBounds( endDay, startDay ) )

import MInfo.Types.DayOfM         ( DayOfM, dayOfM )
import MInfo.Types.Month          ( Month, month )
import MInfo.Types.Year           ( Year, year )

--------------------------------------------------------------------------------

{- | A date, with variable precision -}
newtype DateImpreciseRanges = DateImpreciseRanges [DateImpreciseRange]
  deriving (Eq,Show)

-- For now, we disallow overlapping ranges.  Maybe in future we'll allow them,
-- or allow and merge them: but easier to add them in later than outlaw them
-- later, and it's not clear if we will need them or not.

checkR ∷ (AsDateError_ DateImpreciseRange ε, MonadError ε η) ⇒
         [DateImpreciseRange] → DateImpreciseRange → η [DateImpreciseRange]

checkR [] r = return [r]
checkR rs@(r':_) r = if endDay r < startDay r'
                     then return (r:rs)
                     else dateRangesOverlap r r'

-- THERE IS NO FAILURE

dateImpreciseRs ∷ (Foldable ψ)⇒ ψ DateImpreciseRange → DateImpreciseRanges
dateImpreciseRs = undefined

{-
dateImpreciseRs ∷ (AsDateError_ DateImprecise ε, MonadError ε η) ⇒
                 [DateImpreciseRange] → η DateImpreciseRanges
dateImpreciseRs ds = -- for each consecutive pair, check that the end of the
                     -- former strictly pre-dates the start of the latter
                     let tailDef ∷ [α] → [α] → [α]
                         tailDef def  []  = def
                         tailDef _ (_:xs) = xs
                         pairs ∷ [α] → [(α,α)]
                         pairs xs = zip xs (tailDef [] xs)
  if startDay start ≤ startDay end
                               then return $ DateImpreciseRange (start,end)
                               else dateRangeError start end
-}

{-
dateImpreciseRs' ∷ MonadError DateErrorImprecise η ⇒
                  DateImprecise → DateImprecise → η DateImpreciseRange
dateImpreciseRs' = dateImpreciseR
-}

{- | Create a `DateImpreciseRange` from two dates; else call `Monad.fail`. -}
{-
dateImpreciseRM ∷ Monad η ⇒ DateImprecise → DateImprecise → η DateImpreciseRange
dateImpreciseRM start end =
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

-} 
dateImpreciseRangesTests ∷ TestTree
dateImpreciseRangesTests =
  testGroup "dateImpreciseRanges"
            [ testCase "2019-11-14:26" $
                  testDateImpreciseRanges0
                ≟ dateImpreciseRs [testDateImpreciseRange0]
            , testCase "2019-11-14:26 (2)" $
                  testDateImpreciseRanges0
                ≟ dateImpreciseRs [ testDateImpreciseRange0
                                  , testDateImpreciseRange0 ]
            , testCase "2019-11-14:26 (3)" $
                  testDateImpreciseRanges0
                ≟ dateImpreciseRs [ testDateImpreciseRange0
                                  , testDateImpreciseRange2 ]
            , testCase "2019-11-14:26 (4)" $
                  testDateImpreciseRanges0
                ≟ dateImpreciseRs [ testDateImpreciseRange2
                                  , testDateImpreciseRange0 ]
            , testCase "2019-11:11-26" $
                  testDateImpreciseRanges3
                ≟ dateImpreciseRs [ testDateImpreciseRange0
                                  , testDateImpreciseRange3 ]
            , testCase "2019-11:11-26" $
                  testDateImpreciseRanges3
                ≟ dateImpreciseRs [ testDateImpreciseRange3
                                  , testDateImpreciseRange0 ]
            , testCase "2017-11-26:2019-11-26" $
                  testDateImpreciseRanges4
                ≟ dateImpreciseRs [ testDateImpreciseRange3
                                  , testDateImpreciseRange1 ]
            , testCase "2017-11-26:2019-11-26" $
                  testDateImpreciseRanges4
                ≟ dateImpreciseRs [ testDateImpreciseRange1
                                  , testDateImpreciseRange3 ]
            , testCase "2017-11-26:2019-11-26 (2)" $
                  testDateImpreciseRanges4
                ≟ dateImpreciseRs [ testDateImpreciseRange1
                                  , testDateImpreciseRange0
                                  , testDateImpreciseRange2
                                  , testDateImpreciseRange3 ]
{-
            , testCase "2019-11-26:2019-11-14" $
                  Left (dateRangeError_ testDateP1 testDateP0)
                ≟ dateImpreciseR @DateErrorImprecise testDateP1 testDateP0
            , testCase "2019-11-14:2019-11-14" $
                  Right (DateImpreciseRange (testDateP0, testDateP0))
                ≟ dateImpreciseR @DateErrorImprecise testDateP0 testDateP0
            , testCase "2019:2019" $
                  Right (DateImpreciseRange (testDatePY1, testDatePY1))
                ≟ dateImpreciseR @DateErrorImprecise testDatePY1 testDatePY1
            , testCase "2019:2017" $
                  Left (dateRangeError_ testDatePY1 testDatePY0)
                ≟ dateImpreciseR @DateErrorImprecise testDatePY1 testDatePY0
-}
            ]

{-

----------------------------------------

instance Printable DateImpreciseRange where
  print (DateImpreciseRange ((DayDate y0 m0 d0),(DayDate y1 m1 d1)))
            | y0 ≡ y1 ∧ m0 ≡ m1 ∧ d0 ≡ d1 = P.text $ [fmt|%T-%T-%T|] y0 m0 d0
            | y0 ≡ y1 ∧ m0 ≡ m1  = P.text $ [fmt|%T-%T-%T:%T|]    y0 m0 d0 d1
            | y0 ≡ y1            = P.text $ [fmt|%T-%T-%T:%T-%T|] y0 m0 d0 m1 d1
  print (DateImpreciseRange ((MonthDate y0 m0),(MonthDate y1 m1)))
            | y0 ≡ y1 ∧ m0 ≡ m1 = P.text $ [fmt|%T-%T|]    y0 m0
            | y0 ≡ y1           = P.text $ [fmt|%T-%T:%T|] y0 m0 m1
  print (DateImpreciseRange ((YearDate y0),(YearDate y1)))
            | y0 ≡ y1           = P.text $ [fmt|%T|] y0
  print (DateImpreciseRange (d0,d1))  = P.text $ [fmt|%T:%T|] d0 d1

--------------------

dateImpreciseRangePrintableTests ∷ TestTree
dateImpreciseRangePrintableTests =
  let check s dpr = testCase s $ s ≟ toString dpr
   in testGroup "Printable"
                [ check "2019-11-14:26"         testDateImpreciseRange
                , check "2017-11-26:2019-11-26" testDateImpreciseRange1
                , check "2017-11-26"            testDateImpreciseRange2
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2019-05:11"            testDateImpreciseRangeM1
                , check "2019-05"               testDateImpreciseRangeM2
                , check "2017:2019"             testDateImpreciseRangeY
                , check "2019"                  testDateImpreciseRangeY2
                ]

----------------------------------------

instance Parsecable DateImpreciseRange where
  parser = textual

--------------------

dateImpreciseRangeParsecableTests ∷ TestTree
dateImpreciseRangeParsecableTests =
  let check s dpr = testCase s $ Right dpr ≟ parsec' (""∷Text) s
   in testGroup "Parsecable"
                [ check "2019-11-14:2019-11-26" testDateImpreciseRange
                , check "2019-11-14:26"         testDateImpreciseRange
                , check "2019-11-14:11-26"      testDateImpreciseRange
                , check "2017-11-26:2019-11-26" testDateImpreciseRange1
                , check "2017-11-26"            testDateImpreciseRange2
                , check "2017-11-26:2017-11-26" testDateImpreciseRange2
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2019-05:2019-11"       testDateImpreciseRangeM1
                , check "2019-05:11"            testDateImpreciseRangeM1
                , check "2019-05"               testDateImpreciseRangeM2
                , check "2019-05:2019-05"       testDateImpreciseRangeM2
                , check "2017:2019"             testDateImpreciseRangeY
                , check "2019:2019"             testDateImpreciseRangeY2
                , check "2019"                  testDateImpreciseRangeY2
                ]

----------------------------------------

dateImpreciseMM ∷ Monad η ⇒ (DateImprecise,DateImprecise) → η DateImpreciseRange
dateImpreciseMM = uncurry dateImpreciseRM

----------

parseDD ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseDD = do
  (d,d') ← (,) ⊳ textual ⋪ string ":" ⊵ textual
  dateImpreciseMM (d,d')

----------

parseD ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseD = do
  d ← textual
  dateImpreciseMM (d,d)

--------------------

dateImpreciseYMDD ∷ Monad η ⇒ (Year,Month,DayOfM,DayOfM) → η DateImpreciseRange
dateImpreciseYMDD (y,m,d0,d1) = do
  start ← dateDayM y m d0
  end   ← dateDayM y m d1
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

----------

parseYMDD ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseYMDD = do
  (y,m,d0,d1) ← (,,,) ⊳ textual ⋪ string "-" ⊵ textual
                      ⋪ string "-" ⊵ textual ⋪ string ":" ⊵ textual
  dateImpreciseYMDD (y,m,d0,d1)
--------------------

dateImpreciseYMMDD ∷ Monad η ⇒
                     (Year,Month,DayOfM,Month,DayOfM) → η DateImpreciseRange
dateImpreciseYMMDD (y,m0,d0,m1,d1) = do
  start ← dateDayM y m0 d0
  end   ← dateDayM y m1 d1
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

----------

parseYMMDD ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseYMMDD = do
  (y,m0,d0,m1,d1) ← (,,,,) ⊳ textual ⋪ string "-" ⊵ textual ⋪ string "-"
                           ⊵ textual ⋪ string ":"
                           ⊵ textual ⋪ string "-" ⊵ textual
  dateImpreciseYMMDD (y,m0,d0,m1,d1)

--------------------

dateImpreciseYMM ∷ Monad η ⇒ (Year,Month,Month) → η DateImpreciseRange
dateImpreciseYMM (y,m0,m1) = do
  let start = dateMonth y m0
  let end   = dateMonth y m1
  case dateImpreciseR @DateErrorImprecise start end of
    Left  e → fail $ show e
    Right r → return r

----------

parseYMM ∷ (Monad η, CharParsing η) ⇒ η DateImpreciseRange
parseYMM = do
  (y,m0,m1) ← (,,) ⊳ textual ⋪ string "-" ⊵ textual ⋪ string ":" ⊵ textual
  dateImpreciseYMM (y,m0,m1)

--------------------

instance Textual DateImpreciseRange where
  textual =
-- Use toSeqNE to allow for, e.g., NonEmpty(List) here?
    tries $ parseYMMDD :| [ parseYMDD, parseDD, parseYMM, parseD ]

--------------------

dateImpreciseRangeTextualTests ∷ TestTree
dateImpreciseRangeTextualTests =
  let check s d = testCase s $ Just d ≟ fromString s
   in testGroup "Textual"
                [ check "2019-11-14:2019-11-26" testDateImpreciseRange
                , check "2019-11-14:26"         testDateImpreciseRange
                , check "2019-11-14:11-26"      testDateImpreciseRange
                , check "2017-11-26:2019-11-26" testDateImpreciseRange1
                , check "2017-11-26"            testDateImpreciseRange2
                , check "2017-11-26:2017-11-26" testDateImpreciseRange2
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2019-05:2019-11"       testDateImpreciseRangeM1
                , check "2019-05:11"            testDateImpreciseRangeM1
                , check "2019-05"               testDateImpreciseRangeM2
                , check "2019-05:2019-05"       testDateImpreciseRangeM2
                , check "2017:2019"             testDateImpreciseRangeY
                , check "2019:2019"             testDateImpreciseRangeY2
                , check "2019"                  testDateImpreciseRangeY2
                , testProperty "invertibleText"
                    (propInvertibleText @DateImprecise)
                ]

----------------------------------------

instance Arbitrary DateImpreciseRange where
  arbitrary ∷ Gen DateImpreciseRange
  arbitrary = let isValid ∷ DateImpreciseRange → Bool
                  isValid (DateImpreciseRange (d0,d1)) =
                    case dateImpreciseR @DateErrorImprecise d0 d1 of
                      Left  _ → False
                      Right _ → True
               in (DateImpreciseRange ⊳ ((,) ⊳ arbitrary ⊵ arbitrary))
                  `suchThat` isValid

----------------------------------------

instance FromJSON DateImpreciseRange where
  parseJSON (String t) = case parseText t of
                           Parsed      d → return d
                           Malformed _ e → fail $ [fmt|%s (%t)|] e  t
  parseJSON y@(Number _) = do y' <- parseJSON y
                              return $ DateImpreciseRange (y',y')
  parseJSON invalid    = typeMismatch "DateImprecise" invalid

--------------------

dateImpreciseRangeFromJSONTests ∷ TestTree
dateImpreciseRangeFromJSONTests =
  let check s d = testCase s $ Right d ≟ unYaml' @YamlParseError s
   in testGroup "FromJSON"
                [ check "2019-11-14:2019-11-26" testDateImpreciseRange
                , check "2019-11-14:26"         testDateImpreciseRange
                , check "2019-11-14:11-26"      testDateImpreciseRange
                , check "2017-11-26:2019-11-26" testDateImpreciseRange1
                , check "2017-11-26"            testDateImpreciseRange2
                , check "2017-11-26:2017-11-26" testDateImpreciseRange2
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2019-05:2019-11"       testDateImpreciseRangeM1
                , check "2019-05:11"            testDateImpreciseRangeM1
                , check "2019-05"               testDateImpreciseRangeM2
                , check "2019-05:2019-05"       testDateImpreciseRangeM2
                , check "2017:2019"             testDateImpreciseRangeY
                , check "2019:2019"             testDateImpreciseRangeY2
                , check "2019"                  testDateImpreciseRangeY2
                ]

----------------------------------------

instance ToJSON DateImpreciseRange where
  toJSON d = String $ toText d

--------------------

dateImpreciseRangeToJSONTests ∷ TestTree
dateImpreciseRangeToJSONTests =
  let check s d = testCase s $ String (pack s) ≟ toJSON d
   in testGroup "ToJSON"
                [ check "2019-11-14:26"         testDateImpreciseRange
                , check "2017-11-26:2019-11-26" testDateImpreciseRange1
                , check "2017-11-26"            testDateImpreciseRange2
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2019-05:11"            testDateImpreciseRangeM1
                , check "2019-05"               testDateImpreciseRangeM2
                , check "2017:2019"             testDateImpreciseRangeY
                , check "2019"                  testDateImpreciseRangeY2
                ]

----------------------------------------

dateImpreciseRange ∷ QuasiQuoter
dateImpreciseRange =
  mkQuasiQuoterExp "DateP" (\ s → ⟦ __fromString @DateImprecise s ⟧)

-}

-- testing ---------------------------------------------------------------------

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

testDateImpreciseRange0 ∷ DateImpreciseRange
testDateImpreciseRange0 = [dateImpreciseRange|2019-11-14:26|]

testDateImpreciseRange1 ∷ DateImpreciseRange
testDateImpreciseRange1 = [dateImpreciseRange|2017-11-26:2019-10|]

testDateImpreciseRange2 ∷ DateImpreciseRange
testDateImpreciseRange2 = [dateImpreciseRange|2017-11-14|]

testDateImpreciseRange3 ∷ DateImpreciseRange
testDateImpreciseRange3 = [dateImpreciseRange|2019-11:11-26|]

testDateImpreciseRange4 ∷ DateImpreciseRange
testDateImpreciseRange4 = [dateImpreciseRange|2017-11-26:2019-11-26|]

testDateImpreciseRanges0 ∷ DateImpreciseRanges
testDateImpreciseRanges0 = DateImpreciseRanges [ testDateImpreciseRange0 ]

testDateImpreciseRanges1 ∷ DateImpreciseRanges
testDateImpreciseRanges1 = DateImpreciseRanges [ testDateImpreciseRange1
                                               , testDateImpreciseRange0 ]

testDateImpreciseRanges2 ∷ DateImpreciseRanges
testDateImpreciseRanges2 = DateImpreciseRanges [ testDateImpreciseRange2 ]

testDateImpreciseRanges3 ∷ DateImpreciseRanges
testDateImpreciseRanges3 = DateImpreciseRanges [ testDateImpreciseRange3 ]

testDateImpreciseRanges4 ∷ DateImpreciseRanges
testDateImpreciseRanges4 = DateImpreciseRanges [ testDateImpreciseRange4 ]

{-
testDateP0 ∷ DateImprecise
testDateP0 = dateDay_ [year|2019|] [month|11|] [dayOfM|14|]

testDateP1 ∷ DateImprecise
testDateP1 = dateDay_ [year|2019|] [month|11|] [dayOfM|26|]

testDateP2 ∷ DateImprecise
testDateP2 = dateDay_ [year|2017|] [month|11|] [dayOfM|26|]

testDatePM0 ∷ DateImprecise
testDatePM0 = dateMonth [year|2017|] [month|11|]

testDatePM1 ∷ DateImprecise
testDatePM1 = dateMonth [year|2019|] [month|11|]

testDatePM2 ∷ DateImprecise
testDatePM2 = dateMonth [year|2019|] [month|05|]

testDateImpreciseRangeM ∷ DateImpreciseRange
testDateImpreciseRangeM = dateImpreciseRange (testDatePM0,testDatePM1)

testDateImpreciseRangeM1 ∷ DateImpreciseRange
testDateImpreciseRangeM1 = dateImpreciseRange (testDatePM2,testDatePM1)

testDateImpreciseRangeM2 ∷ DateImpreciseRange
testDateImpreciseRangeM2 = dateImpreciseRange (testDatePM2,testDatePM2)

testDatePY0 ∷ DateImprecise
testDatePY0 = dateYear [year|2017|]

testDatePY1 ∷ DateImprecise
testDatePY1 = dateYear [year|2019|]

testDateImpreciseRangeY ∷ DateImpreciseRange
testDateImpreciseRangeY = dateImpreciseRange (testDatePY0,testDatePY1)

testDateImpreciseRangeY2 ∷ DateImpreciseRange
testDateImpreciseRangeY2 = dateImpreciseRange (testDatePY1,testDatePY1)
-}

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DateImpreciseRanges" [ dateImpreciseRangesTests
                                        {- dateImpreciseRangeTests
                                        , dateImpreciseRangePrintableTests
                                        , dateImpreciseRangeTextualTests
                                        , dateImpreciseRangeParsecableTests
                                        , dateImpreciseRangeFromJSONTests
                                        , dateImpreciseRangeToJSONTests -}
                                        ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
