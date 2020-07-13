{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Admin42 where

import Lorentz
import Michelson.Text

admin42Contract :: ContractCode Natural Address
admin42Contract = do
  unpair
  push @Natural 42
  assertEq $ mkMTextUnsafe "not 42"
  dup
  sender
  assertEq $ mkMTextUnsafe "not admin"
  nil
  pair

