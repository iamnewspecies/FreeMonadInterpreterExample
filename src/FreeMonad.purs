module FreeMonad where

import Prelude
import Control.Monad.Free (Free, liftF, foldFree)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Data.Foldable (class Foldable, foldl)

-- UNDERSTANDING FOLDS

fold :: forall a b. Foldable a => Show b => a b -> String
fold value = do	
	foldl (\a b -> a <> show b) "INIT -> " value

-- WE DO THIS WHEN WRITING THE FLOW --

data Command next = Bark String (String -> next) 
			| Shout String (String -> next) 
			| Cry String (String -> next) 
			| Purr String (String -> next) 
			| End next

type Flow next = Free Command next

bark :: String -> Flow String
bark sound = liftF $ Bark sound id

shout :: String -> Flow String
shout sound = liftF $ Shout sound id

cry :: String -> Flow String
cry sound = liftF $ Cry sound id

purr :: String -> Flow String
purr sound = liftF $ Purr sound id

end :: Flow Unit
end = liftF $ End unit

makeSound :: Flow Unit
makeSound = do
	_ <- bark "bow wow"
	_ <- shout "Aaaaaaa"
	_ <- cry "ouyaaaa"
	_ <- purr "mewooon"
	end


-- THIS WE DO WHEN WE ARE WRITING THE IMPLEMENTATION --


interpret :: forall a b. Command b -> Eff ( console :: CONSOLE| a) b
interpret (Bark s nextFunction) = do 
	_ <- log s
	pure $ nextFunction "Bark"

interpret (Shout s nextFunction) = do 
	_ <- log s
	pure $ nextFunction "Shout"

interpret (Cry s nextFunction) = do 
	_ <- log s
	pure $ nextFunction "Cry"

interpret (Purr s nextFunction) = do 
	_ <- log s
	pure $ nextFunction "Purr"

interpret (End nextFunction) = do 
	_ <- log "End"
	pure $ nextFunction

{-
Some anchor points to understand Free + interpret

what is Command?

Why does command need "next"?

How should Command be structured?

What is fold?

What is the interpret?

What is the job of fold in free?

What is the dummy funcitons written for?

What is free monad + interpreter?

-}

run :: forall a b. Free Command a -> Eff ( console :: CONSOLE| b ) a
run = foldFree interpret

main :: forall a. Eff ( console :: CONSOLE | a) Unit
main = run makeSound