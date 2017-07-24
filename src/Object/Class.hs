{-# LANGUAGE NoMonomorphismRestriction #-}
module Object.Class where
  import Core

  data ClassData
    = RawData Raw
    | CompiledData Compiled
    deriving (Show)

  data Raw
    = Raw {
      _rawName :: Name,
      _rawSource :: Source
    }
    deriving (Show)

  data Compiled
    = Compiled {
      _compiledDecl :: Integer,
      _compiledRaw :: Raw
    }
    deriving (Show)

  instance HasName Raw where name = _rawName
  instance HasSource Raw where source = _rawSource

  instance HasName Compiled where name = name . _compiledRaw
  instance HasSource Compiled where source = source . _compiledRaw

  instance HasName ClassData where
    name (RawData a) = name a
    name (CompiledData a) = name a

  instance HasSource ClassData where
    source (RawData a) = source a
    source (CompiledData a) = source a

  raw (RawData a) = a
  raw (CompiledData a) = _compiledRaw a

  compiled (RawData a) = compile a
  compiled (CompiledData a) = return a

  compile = return . Compiled 1

  classDecl = _compiledDecl
