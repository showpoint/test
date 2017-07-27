{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Storage.Cache where
  import Control.Monad.Reader
  import Data.IORef
  import qualified Data.Map.Strict as Map
  import Data.Map.Strict

  type Cache k v = IORef (Map k v)

  class HasCache k v s where
    cache :: s -> Cache k v

  type MonadCache k v s m = (HasCache k v s, MonadReader s m, MonadIO m, Ord k)

  load :: MonadCache k v s m => k -> m (Maybe v)
  load k = Map.lookup k <$> (ask >>= liftIO . readIORef . cache)

  store :: MonadCache k v s m => k -> v -> m ()
  store k v = cache <$> ask >>= liftIO . flip modifyIORef' (Map.insert k v)
