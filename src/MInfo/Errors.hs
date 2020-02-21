{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MInfo.Errors
  ( AsInfoError, InfoError, InfoFPCError, YamlFPathIOParseInfoFPCError
  , YamlParseInfoFPCError, throwIllegalFileName )
where

-- base --------------------------------

import Control.Exception    ( Exception )
import Data.Either          ( Either( Left, Right ) )
import Data.Eq              ( Eq )
import Data.Function        ( ($), id )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError( _FPathError ), FPathIOError )
import FPath.Error.FPathComponentError
                               ( AsFPathComponentError( _FPathComponentError )
                               , FPathComponentError )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text     ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- yaml-plus ---------------------------

import YamlPlus.Error  ( AsYamlParseError( _YamlParseError ), YamlParseError )

--------------------------------------------------------------------------------

data InfoError = IllegalFileName Text
  deriving (Eq,Show)

instance Exception InfoError

instance Printable InfoError where
  print (IllegalFileName t) = P.text $ [fmt|Illegal file name: '%t'|] t

class AsInfoError ε where
  _InfoError :: Prism' ε InfoError

instance AsInfoError InfoError where
  _InfoError = id

throwIllegalFileName :: (AsInfoError ε, MonadError ε η) ⇒ Text → η α
throwIllegalFileName t = throwError $ (_InfoError #) (IllegalFileName t)

------------------------------------------------------------

data InfoFPCError = IFPCInfoError             InfoError
                  | IFPCFPathComponenentError FPathComponentError
  deriving (Eq,Show)

instance Exception InfoFPCError

instance Printable InfoFPCError where
  print (IFPCInfoError e)             = print e
  print (IFPCFPathComponenentError e) = print e

instance AsInfoError InfoFPCError where
  _InfoError = prism IFPCInfoError
                     (\ case IFPCInfoError e → Right e; e → Left e)

instance AsFPathComponentError InfoFPCError where
  _FPathComponentError = prism IFPCFPathComponenentError
                               (\ case IFPCFPathComponenentError e → Right e
                                       e                           → Left  e)

------------------------------------------------------------

data YamlParseInfoFPCError = YPIFPCParseError   YamlParseError
                           | YPIFPCInfoFPCError InfoFPCError
  deriving (Eq,Show)

_YPIFPCInfoFPCError ∷ Prism' YamlParseInfoFPCError InfoFPCError
_YPIFPCInfoFPCError = prism YPIFPCInfoFPCError
                           (\ case YPIFPCInfoFPCError e → Right e; e → Left e)

instance Exception YamlParseInfoFPCError

instance Printable YamlParseInfoFPCError where
  print (YPIFPCParseError   e) = print e
  print (YPIFPCInfoFPCError e) = print e

instance AsYamlParseError YamlParseInfoFPCError where
  _YamlParseError = prism YPIFPCParseError
                          (\ case YPIFPCParseError  e → Right e; e → Left e)

instance AsInfoError YamlParseInfoFPCError where
  _InfoError = _YPIFPCInfoFPCError ∘ _InfoError

instance AsFPathComponentError YamlParseInfoFPCError where
  _FPathComponentError = _YPIFPCInfoFPCError ∘ _FPathComponentError

------------------------------------------------------------

data YamlFPathIOParseInfoFPCError = YFIPIFPCParseError   YamlParseError
                                  | YFIPIFPCInfoFPCError InfoFPCError
                                  | YFIPIFPCFPathIOError FPathIOError
  deriving (Eq,Show)

_YFIPIFPCInfoFPCError ∷ Prism' YamlFPathIOParseInfoFPCError InfoFPCError
_YFIPIFPCInfoFPCError = prism YFIPIFPCInfoFPCError
                           (\ case YFIPIFPCInfoFPCError e → Right e; e → Left e)

_YFIPIFPCFPathIOError ∷ Prism' YamlFPathIOParseInfoFPCError FPathIOError
_YFIPIFPCFPathIOError = prism YFIPIFPCFPathIOError
                           (\ case YFIPIFPCFPathIOError e → Right e; e → Left e)

instance Exception YamlFPathIOParseInfoFPCError

instance Printable YamlFPathIOParseInfoFPCError where
  print (YFIPIFPCParseError   e) = print e
  print (YFIPIFPCInfoFPCError e) = print e
  print (YFIPIFPCFPathIOError e) = print e

instance AsYamlParseError YamlFPathIOParseInfoFPCError where
  _YamlParseError = prism YFIPIFPCParseError
                          (\ case YFIPIFPCParseError  e → Right e; e → Left e)

instance AsInfoError YamlFPathIOParseInfoFPCError where
  _InfoError = _YFIPIFPCInfoFPCError ∘ _InfoError

instance AsFPathComponentError YamlFPathIOParseInfoFPCError where
  _FPathComponentError = _YFIPIFPCInfoFPCError ∘ _FPathComponentError

instance AsIOError YamlFPathIOParseInfoFPCError where
  _IOError = _YFIPIFPCFPathIOError ∘ _IOError

instance AsFPathError YamlFPathIOParseInfoFPCError where
  _FPathError = _YFIPIFPCFPathIOError ∘ _FPathError

-- that's all, folks! ----------------------------------------------------------
