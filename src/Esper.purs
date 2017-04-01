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

-- Tuple

newtype Tuple a b = Tuple
  { first :: a
  , second :: b
  }

-- State

newtype StateT s m a = StateT (s -> m (Tuple a s))

instance stateTHasBind :: HasBind m => HasBind (StateT s m) where
  bind m f = StateT \s -> do
    Tuple { first: x, second: s' } <- runStateT m s
    runStateT (f x) s'

instance stateTHasPure :: HasPure m => HasPure (StateT s m) where
  pure x = StateT \y -> pure (Tuple { first: x, second: y })

runStateT :: forall a m s. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT f) = f

state :: forall a m s. HasPure m => (s -> Tuple a s) -> StateT s m a
state f = StateT \x -> pure (f x)

get :: forall m s. HasPure m => StateT s m s
get = state \x -> Tuple { first: x, second: x }

put :: forall m s. HasPure m => s -> StateT s m Unit
put x = state \_ -> Tuple { first: unit, second: x }

-- Reader

newtype ReaderT r m a = ReaderT (r -> m a)

instance readerTHasBind :: HasBind m => HasBind (ReaderT r m) where
  bind m f = ReaderT \x -> do
    y <- runReaderT m x
    runReaderT (f y) x

instance readerTHasPure :: HasPure m => HasPure (ReaderT r m) where
  pure x = ReaderT \_ -> pure x

runReaderT :: forall a m r. ReaderT r m a -> r -> m a
runReaderT (ReaderT f) = f

ask :: forall m r. HasPure m => ReaderT r m r
ask = ReaderT pure

-- Add

class HasAdd a where
  add :: a -> a -> a

foreign import
  intAdd :: Int -> Int -> Int

instance intHasAdd :: HasAdd Int where
  add = intAdd

infixl 6 add as +

-- Helper

getUInt32LE :: forall e. ReaderT Buffer (StateT Int (Effect (buffer :: BUFFER | e))) Int
getUInt32LE = do
  buffer <- ask
  position <- ReaderT \_ -> get
  value <- ReaderT \_ -> StateT \s -> do
    x <- readUInt32LE buffer position
    pure (Tuple { first: x, second: s })
  ReaderT \_ -> put (position + 4)
  pure value

-- Replay

newtype Replay = Replay
  { header :: Section Header
  }

getReplay
  :: forall e. ReaderT Buffer (StateT Int (Effect (buffer :: BUFFER | e))) Replay
getReplay = do
  buffer <- ask
  header <- getSection getHeader
  pure (Replay { header })

-- Section

newtype Section a = Section
  { size :: Int
  , crc :: Int
  , value :: a
  }

getSection
  :: forall a e
  . (ReaderT Buffer (StateT Int (Effect (buffer :: BUFFER | e))) a)
  -> ReaderT Buffer (StateT Int (Effect (buffer :: BUFFER | e))) (Section a)
getSection getValue = do
  buffer <- ask
  size <- getUInt32LE
  crc <- getUInt32LE
  value <- getValue
  pure (Section { size, crc, value })

-- Header

newtype Header = Header
  {
  }

getHeader
  :: forall e. ReaderT Buffer (StateT Int (Effect (buffer :: BUFFER | e))) Header
getHeader = pure (Header {})

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
      Tuple { first: replay, second: _ } <- runStateT (runReaderT getReplay buffer) 0
      log (inspect replay)
