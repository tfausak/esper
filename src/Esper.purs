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
  readFile file throw \buffer -> do
      Tuple replay _ <- runState (runReader getReplay buffer) (Offset 0)
      inspect replay

type Parser a = forall e.
  Reader Buffer (State Offset (Effect (error :: ERROR | e))) a

newtype Replay = Replay
  { header :: Section Header
  , content :: Section Content
  }

getReplay :: Parser Replay
getReplay = do
  header <- getSection getHeader
  content <- getSection getContent
  pure (Replay { header, content })

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

newtype Content = Content
  { levels :: List Text
  , keyFrames :: List KeyFrame
  , size :: UInt32
  , frames :: Array Frame
  , messages :: List Message
  , marks :: List Mark
  , packages :: List Text
  , objects :: List Text
  , names :: List Text
  , classMappings :: List ClassMapping
  , caches :: List Cache
  }

getContent :: Parser Content
getContent = do
  levels <- getList getText
  keyFrames <- getList getKeyFrame
  size <- getUInt32LE
  frames <- getFrames size
  messages <- getList getMessage
  marks <- getList getMark
  packages <- getList getText
  objects <- getList getText
  names <- getList getText
  classMappings <- getList getClassMapping
  caches <- getList getCache
  pure (Content { levels, keyFrames, size, frames, messages, marks, packages, objects, names, classMappings, caches })

newtype Cache = Cache
  { classId :: UInt32
  , parentCacheId :: UInt32
  , cacheId :: UInt32
  , attributeMappings :: List AttributeMapping
  }

getCache :: Parser Cache
getCache = todo "getCache"

newtype AttributeMapping = AttributeMapping
  { objectId :: UInt32
  , streamId :: UInt32
  }

newtype ClassMapping = ClassMapping
  { name :: Text
  , streamId :: UInt32
  }

getClassMapping :: Parser ClassMapping
getClassMapping = todo "getClassMapping"

newtype Mark = Mark
  { value :: Text
  , frame :: UInt32
  }

getMark :: Parser Mark
getMark = todo "getMark"

newtype Message = Message
  { frame :: UInt32
  , name :: Text
  , value :: Text
  }

getMessage :: Parser Message
getMessage = todo "getMessage"

newtype Frame = Frame
  {
  }

getFrames :: UInt32 -> Parser (Array Frame)
getFrames size = case unpackUInt32 size of
  0 -> pure []
  _ -> todo "getFrames"

newtype KeyFrame = KeyFrame
  { time :: Float32
  , frame :: UInt32
  , position :: UInt32
  }

getKeyFrame :: Parser KeyFrame
getKeyFrame = todo "getKeyFrame"

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
  if (unpackText key).value == "None\x00"
    then pure (Tuple key Nothing)
    else do
      value <- getValue
      pure (Tuple key (Just value))

newtype List a = List
  { size :: UInt32
  , value :: Array a
  }

getList :: forall a. Parser a -> Parser (List a)
getList getElement = do
  size <- getUInt32LE
  value <- getListElements getElement size
  pure (List { size, value })

getListElements :: forall a. Parser a -> UInt32 -> Parser (Array a)
getListElements getElement size = case unpackUInt32 size of
  0 -> pure []
  _ -> do
    element <- getElement
    elements <- getListElements getElement (size - UInt32 1)
    pure ([element] + elements)

newtype Property = Property
  { kind :: Text
  , size :: UInt64
  , value :: PropertyValue
  }

getProperty :: Parser Property
getProperty = do
  kind <- getText
  size <- getUInt64LE
  value <- getPropertyValue kind
  pure (Property { kind, size, value })

todo :: forall a. String -> Parser a
todo x = liftReader (liftState (throw (newError x)))

data PropertyValue
  = ArrayProperty (List (Dictionary Property))
  | BoolProperty UInt8
  | ByteProperty Text (Maybe Text)
  | FloatProperty Float32
  | IntProperty UInt32
  | NameProperty Text
  | QWordProperty UInt64
  | StrProperty Text

getPropertyValue :: Text -> Parser PropertyValue
getPropertyValue kind = case (unpackText kind).value of
  "ArrayProperty\x00" -> do
    x <- getList (getDictionary getProperty)
    pure (ArrayProperty x)
  "BoolProperty\x00" -> do
    x <- getUInt8
    pure (BoolProperty x)
  "ByteProperty\x00" -> do
    k <- getText
    v <- case (unpackText k).value of
      "OnlinePlatform_Steam\x00" -> pure Nothing
      _ -> do
        v <- getText
        pure (Just v)
    pure (ByteProperty k v)
  "FloatProperty\x00" -> do
    x <- getFloat32LE
    pure (FloatProperty x)
  "IntProperty\x00" -> do
    x <- getUInt32LE
    pure (IntProperty x)
  "NameProperty\x00" -> do
    x <- getText
    pure (NameProperty x)
  "QWordProperty\x00" -> do
    x <- getUInt64LE
    pure (QWordProperty x)
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
    let end = start + Offset (unpackInt32 size)
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

newtype UInt8 = UInt8 Int

getUInt8 :: Parser UInt8
getUInt8 = do
  buffer <- ask
  liftReader (do
    position <- get
    value <- liftState (readUInt8 buffer position)
    put (position + Offset 1)
    pure (UInt8 value))

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

newtype UInt64 = UInt64
  { high :: UInt32
  , low :: UInt32
  }

getUInt64LE :: Parser UInt64
getUInt64LE = do
  high <- getUInt32LE
  low <- getUInt32LE
  pure (UInt64 { high, low })

-- Unpack

unpackInt32 :: Int32 -> Int
unpackInt32 (Int32 x) = x

unpackOffset :: Offset -> Int
unpackOffset (Offset x) = x

unpackText :: Text -> { size :: Int32, value :: String }
unpackText (Text x) = x

unpackUInt32 :: UInt32 -> Int
unpackUInt32 (UInt32 x) = x

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
  add x y = Offset (unpackOffset x + unpackOffset y)

-- Subtract

class HasSubtract a where
  subtract :: a -> a -> a

infixl 6 subtract as -

foreign import
  subtractInt :: Int -> Int -> Int

instance intHasSubtract :: HasSubtract Int where
  subtract = subtractInt

instance uInt32HasSubtract :: HasSubtract UInt32 where
  subtract x y = UInt32 (unpackUInt32 x - unpackUInt32 y)

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
  newError :: String -> Error

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
  readUInt8 :: forall e. Buffer -> Offset -> Effect (error :: ERROR | e) Int

foreign import
  readUInt32LE :: forall e. Buffer -> Offset -> Effect (error :: ERROR | e) Int

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
    (Error -> Effect e Unit) ->
    (Buffer -> Effect e Unit) ->
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
