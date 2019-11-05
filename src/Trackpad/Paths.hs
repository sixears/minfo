{-# LANGUAGE QuasiQuotes #-}

module Trackpad.Paths where

import Path  ( Abs, File, Path, absfile )

xinput :: Path Abs File
xinput = [absfile|/nix/store/1baf6mjzw51znsida4sn9nckvjz33ql4-xinput-1.6.2/bin/xinput|]
