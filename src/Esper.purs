module Esper
  ( CONSOLE
  , ERROR
  , FILE
  , Effect
  , Unit
  , main
  ) where

main ::
  String -> Effect (console :: CONSOLE, error :: ERROR, file :: FILE) Unit
main file = do
  readFile file \nullableError nullableBuffer -> do
    let result = case nullable Nothing Just nullableError of
          Just error -> Left error
          Nothing -> case nullable Nothing Just nullableBuffer of
            Nothing -> Left (toError "neither error nor buffer")
            Just buffer -> Right buffer
    case result of
      Left error -> throw error
      Right buffer -> do
        Tuple replay _ <- runState (runReader getReplay buffer) (Offset 0)
        inspect replay

type Parser a = forall e.
  Reader Buffer (State Offset (Effect (error :: ERROR | e))) a

newtype Replay = Replay
  { header :: Section Header
  }

getReplay :: Parser Replay
getReplay = do
  header <- getSection getHeader
  pure (Replay { header })

newtype Section a = Section
  { size :: UInt32
  , crc :: UInt32
  , value :: a
  }

getSection :: forall a. Parser a -> Parser (Section a)
getSection getValue = do
  size <- getUInt32LE
  crc <- getUInt32LE
  value <- getValue
  pure (Section { size, crc, value })

newtype Header = Header
  { majorVersion :: UInt32
  , minorVersion :: UInt32
  , label :: Text
  , properties :: Dictionary Property
  }

getHeader :: Parser Header
getHeader = do
  majorVersion <- getUInt32LE
  minorVersion <- getUInt32LE
  label <- getText
  properties <- getDictionary getProperty
  pure (Header { majorVersion, minorVersion, label, properties })

newtype Dictionary a = Dictionary
  { value :: Array (Tuple Text a)
  , lastKey :: Text
  }

getDictionary :: forall a. Parser a -> Parser (Dictionary a)
getDictionary getValue = do
  Tuple value lastKey <- getDictionaryElements getValue
  pure (Dictionary { value, lastKey })

getDictionaryElements ::
  forall a. Parser a -> Parser (Tuple (Array (Tuple Text a)) Text)
getDictionaryElements getValue = do
  Tuple key maybeValue <- getDictionaryElement getValue
  case maybeValue of
    Nothing -> pure (Tuple [] key)
    Just value -> do
      let element = Tuple key value
      Tuple elements lastKey <- getDictionaryElements getValue
      pure (Tuple ([element] + elements) lastKey)

getDictionaryElement :: forall a. Parser a -> Parser (Tuple Text (Maybe a))
getDictionaryElement getValue = do
  key <- getText
  if (unpack key).value == "None\x00"
    then pure (Tuple key Nothing)
    else do
      value <- getValue
      pure (Tuple key (Just value))

newtype List a = List (Array a)

getList :: forall a. Parser a -> Parser (List a)
getList _ = todo "getList"

newtype Property = Property
  { kind :: Text
  , size :: Int64
  , value :: PropertyValue
  }

getProperty :: Parser Property
getProperty = do
  kind <- getText
  size <- getUInt64LE
  value <- getPropertyValue kind
  pure (Property { kind, size, value })

todo :: forall a. String -> Parser a
todo x = liftReader (liftState (throw (toError x)))

data PropertyValue
  = ArrayProperty (List (Dictionary Property))
  | BoolProperty Int
  | ByteProperty Text (Maybe Text)
  | FloatProperty Float32
  | IntProperty UInt32
  | NameProperty Text
  | QWordProperty Int
  | StrProperty Text

getPropertyValue :: Text -> Parser PropertyValue
getPropertyValue kind = case (unpack kind).value of
  "FloatProperty\x00" -> do
    x <- getFloat32LE
    pure (FloatProperty x)
  "IntProperty\x00" -> do
    x <- getUInt32LE
    pure (IntProperty x)
  "NameProperty\x00" -> do
    x <- getText
    pure (NameProperty x)
  "StrProperty\x00" -> do
    x <- getText
    pure (StrProperty x)
  x -> todo x

newtype Text = Text
  { size :: Int32
  , value :: String
  }

getText :: Parser Text
getText = do
  size <- getInt32LE
  value <- getString size
  pure (Text { size, value })

getString :: Int32 -> Parser String
getString size = do
  buffer <- ask
  liftReader (do
    start <- get
    let end = start + Offset (unpack size)
    value <- liftState (readString buffer start end)
    put end
    pure value)

newtype Float32 = Float32 Number

getFloat32LE :: Parser Float32
getFloat32LE = do
  buffer <- ask
  liftReader (do
    position <- get
    value <- liftState (readFloatLE buffer position)
    put (position + Offset 4)
    pure (Float32 value))

newtype Int32 = Int32 Int

getInt32LE :: Parser Int32
getInt32LE = do
  buffer <- ask
  liftReader (do
    position <- get
    value <- liftState (readInt32LE buffer position)
    put (position + Offset 4)
    pure (Int32 value))

newtype UInt32 = UInt32 Int

getUInt32LE :: Parser UInt32
getUInt32LE = do
  buffer <- ask
  liftReader (do
    position <- get
    value <- liftState (readUInt32LE buffer position)
    put (position + Offset 4)
    pure (UInt32 value))

newtype Int64 = Int64
  { high :: Int
  , low :: Int
  }

getUInt64LE :: Parser Int64
getUInt64LE = do
  buffer <- ask
  liftReader (do
    position <- get
    value <- liftState (readUInt64LE buffer position)
    put (position + Offset 8)
    pure (Int64 value))

-- Unpack

class HasUnpack a b | a -> b where
  unpack :: a -> b

instance int32HasUnpack :: HasUnpack Int32 Int where
  unpack (Int32 x) = x

instance offsetHasUnpack :: HasUnpack Offset Int where
  unpack (Offset x) = x

instance textHasUnpack :: HasUnpack Text { size :: Int32, value :: String } where
  unpack (Text x) = x

-- Effect

foreign import
  kind Effect

foreign import
  data Effect :: # Effect -> Type -> Type

-- Unit

foreign import
  data Unit :: Type

foreign import
  unit :: Unit

-- Equal

class HasEqual a where
  equal :: a -> a -> Boolean

infix 4 equal as ==

foreign import
  equalString :: String -> String -> Boolean

instance stringHasEqual :: HasEqual String where
  equal = equalString

-- Add

class HasAdd a where
  add :: a -> a -> a

infixl 6 add as +

foreign import
  addArray :: forall a. Array a -> Array a -> Array a

instance arrayHasAdd :: HasAdd (Array a) where
  add = addArray

foreign import
  addInt :: Int -> Int -> Int

instance intHasAdd :: HasAdd Int where
  add = addInt

instance offsetHasAdd :: HasAdd Offset where
  add x y = Offset (unpack x + unpack y)

-- Pure

class HasPure m where
  pure :: forall a. a -> m a

foreign import
  pureEffect :: forall a e. a -> Effect e a

instance effectHasPure :: HasPure (Effect e) where
  pure = pureEffect

instance readerHasPure :: HasPure m => HasPure (Reader r m) where
  pure x = liftReader (pure x)

instance stateHasPure :: (HasBind m, HasPure m) => HasPure (State s m) where
  pure x = liftState (pure x)

-- Console

foreign import
  data CONSOLE :: Effect

foreign import
  inspect :: forall a e. a -> Effect (console :: CONSOLE | e) Unit

foreign import
  log :: forall e. String -> Effect (console :: CONSOLE | e) Unit

foreign import
  warn :: forall e. String -> Effect (console :: CONSOLE | e) Unit

-- Bind

class HasBind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

foreign import
  bindEffect :: forall a b e. Effect e a -> (a -> Effect e b) -> Effect e b

instance effectHasBind :: HasBind (Effect e) where
  bind = bindEffect

instance readerHasBind :: HasBind m => HasBind (Reader r m) where
  bind m f = Reader \r -> do
    x <- runReader m r
    runReader (f x) r

instance stateHasBind :: HasBind m => HasBind (State s m) where
  bind m f = State \s -> do
    Tuple x s' <- runState m s
    runState (f x) s'

-- Discard

discard :: forall b m. HasBind m => m Unit -> (Unit -> m b) -> m b
discard = bind

-- Error

foreign import
  data ERROR :: Effect

foreign import
  data Error :: Type

foreign import
  throw :: forall a e. Error -> Effect (error :: ERROR | e) a

foreign import
  toError :: String -> Error

-- Offset

newtype Offset = Offset Int

-- Buffer

foreign import
  data Buffer :: Type

foreign import
  readFloatLE ::
    forall e. Buffer -> Offset -> Effect (error :: ERROR | e) Number

foreign import
  readInt32LE :: forall e. Buffer -> Offset -> Effect (error :: ERROR | e) Int

foreign import
  readString ::
    forall e. Buffer -> Offset -> Offset -> Effect (error :: ERROR | e) String

foreign import
  readUInt32LE :: forall e. Buffer -> Offset -> Effect (error :: ERROR | e) Int

foreign import
  readUInt64LE ::
    forall e.
    Buffer ->
    Offset ->
    Effect (error :: ERROR | e) { high :: Int, low :: Int }

-- Nullable

foreign import
  data Nullable :: Type -> Type

foreign import
  nullable :: forall a b. b -> (a -> b) -> Nullable a -> b

-- Maybe

data Maybe a = Nothing | Just a

-- Either

data Either a b = Left a | Right b

-- Tuple

data Tuple a b = Tuple a b

-- File

foreign import
  data FILE :: Effect

foreign import
  readFile ::
    forall e.
    String ->
    (Nullable Error -> Nullable Buffer -> Effect (file :: FILE | e) Unit) ->
    Effect (file :: FILE | e) Unit

-- Reader

newtype Reader r m a = Reader (r -> m a)

runReader :: forall a m r. Reader r m a -> r -> m a
runReader (Reader x) = x

ask :: forall m r. HasPure m => Reader r m r
ask = Reader pure

liftReader :: forall a m r. m a -> Reader r m a
liftReader x = Reader \_ -> x

-- State

newtype State s m a = State (s -> m (Tuple a s))

runState :: forall a m s. State s m a -> s -> m (Tuple a s)
runState (State f) x = f x

get :: forall m s. HasPure m => State s m s
get = State \x -> pure (Tuple x x)

put :: forall m s. HasPure m => s -> State s m Unit
put x = State \_ -> pure (Tuple unit x)

liftState :: forall a m s. HasBind m => HasPure m => m a -> State s m a
liftState f = State \s -> do
  x <- f
  pure (Tuple x s)
