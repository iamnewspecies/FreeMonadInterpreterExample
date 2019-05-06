module FreeMonad where

import Prelude
import Control.Monad.Free (Free, liftF, foldFree)
import Effect.Class.Console (log)
import Effect (Effect)
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
bark sound = liftF $ Bark sound identity

shout :: String -> Flow String
shout sound = liftF $ Shout sound identity

cry :: String -> Flow String
cry sound = liftF $ Cry sound identity

purr :: String -> Flow String
purr sound = liftF $ Purr sound identity

end :: Flow Unit
end = liftF $ End unit

makeSound :: Flow String
makeSound = do
	_ <- bark "bow wow"
	_ <- shout "Aaaaaaa"
	_ <- cry "ouyaaaa"
	purr "mewooon"


-- THIS WE DO WHEN WE ARE WRITING THE IMPLEMENTATION --


interpret :: forall b. Command b -> Effect b
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

run :: forall a. Free Command a -> Effect a
run = foldFree interpret