module Main where

import Prelude

import Effect (Effect)
import FreeMonad (makeSound, run)

main :: Effect Unit
main = run makeSound *> pure unit