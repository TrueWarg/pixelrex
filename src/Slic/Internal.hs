{-# LANGUAGE DataKinds #-}

module Slic.Internal where

import Data.Massiv.Array

data Params = Params

process :: Params -> Array D (IxN 3) Int -> Array D (IxN 3) Int
process _ _ = error "Not implemented"

