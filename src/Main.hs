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
  import Object.Class hiding (Class)
  import qualified Object.Class as Obj

  type Cache = IORef (Map (Object Name) (Object Self))

  newCache :: IO Cache
  newCache = newIORef $ fromList [
    (Class "A", Class (Raw $ ClassRaw "A" "class A")),
    (Class "V", Class (Raw $ ClassRaw "V" "class V"))]

  class Store k v m where
    store :: k -> v -> m ()

  instance (MonadReader Cache m, MonadIO m) => Store (Object Name) (Object Self) m where
    store k v = ask >>= liftIO . flip modifyIORef' (Map.insert k v) >> return ()

  instance (MonadReader Cache m, MonadIO m) => Store (Object Name) Obj.Class m where
    store k v = store k (Class @Self $ Compiled v)

  class Load a r m | a -> r where
    load :: r -> m a

  instance (MonadReader Cache m, MonadIO m) => Load (Maybe (Object Self)) (Object Name) m where
    load r = Map.lookup r <$> (ask >>= liftIO . readIORef)

  instance (MonadReader Cache m, MonadIO m) => Load (Object Self) (Object Name) m where
    load r = fromMaybe (error $ show r ++ " not found.") <$> load r

  instance (MonadReader Cache m, MonadIO m) => Load Obj.Class (Object Name) m where
    load r = objClass <$> load r >>= \case
      Obj.Class a -> return a
      ClassRaw a -> do
        c <- compile a
        store c
        return c

  test = classDecl <$> load @Obj.Class (class_ "A")

  main = newCache >>= runReaderT test >>= print
