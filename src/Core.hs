module Core where
  type Name = String

  class HasName a where
    name :: a -> Name

  type Source = String

  class HasSource a where
    source :: a -> Source
