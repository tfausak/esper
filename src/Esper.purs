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

newtype Unit = Unit {}

unit :: Unit
unit = Unit {}

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

foreign import
  toError :: String -> Error

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

-- Either

data Either a b = Left a | Right b

-- Replay

newtype Replay = Replay
  { header :: Section Header
  }

getReplay :: forall e. Buffer -> Effect (buffer :: BUFFER | e) Replay
getReplay buffer = do
  header <- getSection getHeader buffer
  pure (Replay { header })

-- Section

newtype Section a = Section
  { size :: Int
  , crc :: Int
  , value :: a
  }

getSection
  :: forall a e
  . (Buffer -> Effect (buffer :: BUFFER | e) a)
  -> Buffer
  -> Effect (buffer :: BUFFER | e) (Section a)
getSection getValue buffer = do
  size <- readUInt32LE buffer 0
  crc <- readUInt32LE buffer 4
  value <- getValue buffer
  pure (Section { size, crc, value })

-- Header

newtype Header = Header
  {
  }

getHeader :: forall e. Buffer -> Effect (buffer :: BUFFER | e) Header
getHeader _ = pure (Header {})

-- Main

main file = readFile file \nullableError nullableBuffer -> do
  let result = case nullableToMaybe nullableError of
        Just error -> Left error
        Nothing -> case nullableToMaybe nullableBuffer of
          Nothing -> Left (toError "neither error nor buffer")
          Just buffer -> Right buffer
  case result of
    Left error -> warn (inspect error)
    Right buffer -> do
      replay <- getReplay buffer
      log (inspect replay)
