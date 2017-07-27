{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Storage.Class where
  class Store a m where
    store :: a -> m ()

  class Load a r m | a -> r where
    load :: r -> m a
