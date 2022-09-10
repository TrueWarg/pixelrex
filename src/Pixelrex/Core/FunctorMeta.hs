{-# LANGUAGE TypeFamilies #-}

module Pixelrex.Core.FunctorMeta where

class FunctorMeta f where
  type Meta f
  fmapWithMeta :: (Meta f -> a -> b) -> f a -> f b
