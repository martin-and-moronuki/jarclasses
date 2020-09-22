{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module StringBuilding where

import Relude

(<!>) :: (Semigroup a, IsString a) => a -> a -> a
a <!> b = a <> " " <> b

quo :: (Semigroup a, IsString a) => a -> a
quo x = "\"" <> x <> "\""
