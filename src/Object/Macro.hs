module Object.Macro where
  import Core

  data MacroD
    = MacroD {
      _macroName :: Name,
      _macroSource :: Source
    } deriving (Show)

  instance HasName MacroD where name = _macroName
  instance HasSource MacroD where source = _macroSource
