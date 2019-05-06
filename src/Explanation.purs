module Explanation where

-- Following is what I what want to add to my DSL. This is how you want to program.

import Prelude

import Effect (Effect)
import Data.Foldable (foldl)


data Command = Walk String
			| Run String  
			| Jump String 
			| Swim String 
			| Stand String
            | Sleep String
			| End String

-- here are the instructions that we want the system to give out.
-- Basically every DSL is an array of instructions and you want to execute these.

instructions :: Array Command
instructions = [Walk "some", Run "some", Jump "some", Swim "some", Stand "some", Jump "some", End "go home"]

-- after this step you'll have a an executor function which takes each instrcution and does some action based on it.

-- the obvious problem here is how much you are able to express. 
-- What if you could use the existing do notation which you always use to write this code.
-- So free monad actually provides this feature. 
-- Then along with an executor function we can attach our implemenation of what happens for each of these commands.







data BetterCommand next = BWalk String next
                    | BRun String next
                    | BJump String next
			        | BSwim String next
			        | BStand String next
			        | BEnd String



-- main :: Effect Unit
-- main = bindExecutor ins  *> pure unit