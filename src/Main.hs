{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Main where
  import Control.Monad.Reader
  import qualified Data.Map.Strict as Map
  import Data.Map.Strict
  import Data.Maybe
  import Data.IORef

  import Core
  import Object
  import Object.Class
  import qualified Object.Class as Class

  type Cache = IORef (Map (Object Name) (Object Self))

  newCache :: IO Cache
  newCache = newIORef $ fromList [
    (Class "A", Class (RawData $ Raw "A" "class A")),
    (Class "V", Class (RawData $ Raw "V" "class V"))]

  class Store a m where
    store :: a -> m ()

  instance (MonadReader Cache m, MonadIO m) => Store (Object Self) m where
    store a = ask >>= liftIO . flip modifyIORef' (Map.insert (objName a) a) >> return ()

  instance (MonadReader Cache m, MonadIO m) => Store Compiled m where
    store = store . Class @Self . CompiledData

  class Load a r m | a -> r where
    load :: r -> m a

  instance (MonadReader Cache m, MonadIO m) => Load (Maybe (Object Self)) (Object Name) m where
    load r = Map.lookup r <$> (ask >>= liftIO . readIORef)

  instance (MonadReader Cache m, MonadIO m) => Load (Object Self) (Object Name) m where
    load r = fromMaybe (error $ show r ++ " not found.") <$> load r

  instance (MonadReader Cache m, MonadIO m) => Load Compiled (Object Name) m where
    load r = objClass <$> load r >>= \case
      CompiledData a -> return a
      RawData a -> do
        c <- compile a
        store c
        return c

  test = classDecl <$> load @Compiled (class_ "A")

  main = newCache >>= runReaderT test >>= print
