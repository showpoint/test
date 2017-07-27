{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
module Object.Class where
  import Core

  data Stage
    = Raw RawData
    | Compiled CompiledData
    deriving (Show)

  data RawData
    = RawData {
      _className :: Name,
      _classSource :: Source
    }
    deriving (Show)

  data CompiledData
    = CompiledData {
      _classDecl :: Integer,
      _classRaw :: RawData
    }
    deriving (Show)

  instance HasName RawData where name = _className
  instance HasSource RawData where source = _classSource

  instance HasName CompiledData where name = name . _classRaw
  instance HasSource CompiledData where source = source . _classRaw

  instance HasName Stage where
    name (Raw a) = name a
    name (Compiled a) = name a

  instance HasSource Stage where
    source (Raw a) = source a
    source (Compiled a) = source a

  raw (Raw a) = a
  raw (Compiled a) = _classRaw a

  compiled (Raw a) = compile a
  compiled (Compiled a) = return a

  compile = return . CompiledData 1

  classDecl = _classDecl
