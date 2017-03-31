module Esper where

-- Effect

foreign import
  kind Effect

foreign import
  data Effect :: # Effect -> Type -> Type

-- Pure

class HasPure m where
  pure :: forall a. a -> m a

foreign import
  effectPure :: forall a e. a -> Effect e a

instance effectHasPure :: HasPure (Effect e) where
  pure = effectPure

-- Unit

foreign import
  data Unit :: Type

foreign import
  unit :: Unit

-- Console

foreign import
  data CONSOLE :: Effect

foreign import
  log :: forall e. String -> Effect (console :: CONSOLE | e) Unit

foreign import
  warn :: forall e. String -> Effect (console :: CONSOLE | e) Unit

-- Bind

class HasBind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

foreign import
  effectBind :: forall a b e. Effect e a -> (a -> Effect e b) -> Effect e b

instance effectHasBind :: HasBind (Effect e) where
  bind = effectBind

-- Discard

class HasDiscard a where
  discard :: forall b m. HasBind m => m a -> (a -> m b) -> m b

instance unitHasDiscard :: HasDiscard Unit where
  discard = bind

-- Buffer

foreign import
  data BUFFER :: Effect

foreign import
  data Buffer :: Type

foreign import
  readUInt32LE :: forall e. Buffer -> Int -> Effect (buffer :: BUFFER | e) Int

-- Nullable

foreign import
  data Nullable :: Type -> Type

foreign import
  nullable :: forall a b. b -> (a -> b) -> Nullable a -> b

-- Error

foreign import
  data Error :: Type

-- File

foreign import
  data FILE :: Effect

foreign import
  readFile
    :: forall e
    . String
    -> (Nullable Error -> Nullable Buffer -> Effect (buffer :: BUFFER, file :: FILE | e) Unit)
    -> Effect (buffer :: BUFFER, file :: FILE | e) Unit

-- Inspect

foreign import
  inspect :: forall a. a -> String

-- Maybe

data Maybe a = Nothing | Just a

nullableToMaybe :: forall a. Nullable a -> Maybe a
nullableToMaybe = nullable Nothing Just

-- Main

main file =  readFile file \nullableError nullableBuffer ->
  case nullableToMaybe nullableError of
    Just error -> warn (inspect error)
    Nothing -> case nullableToMaybe nullableBuffer of
      Nothing -> warn "no error but also no buffer"
      Just buffer -> do
        headerSize <- readUInt32LE buffer 0
        log (inspect headerSize)
