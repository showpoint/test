{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Object where
  import GHC.Exts (Constraint)
  import Core
  import Object.Class
  import Object.Macro

  data Self

  data Object a
    = Class (ClassV a)
    | Macro (MacroV a)

  type family ClassV a :: *
  type family MacroV a :: *

  type ForAllV (k :: * -> Constraint) s = (k (ClassV s), k (MacroV s))

  deriving instance ForAllV Eq a => Eq (Object a)
  deriving instance ForAllV Ord a => Ord (Object a)
  deriving instance ForAllV Show a => Show (Object a)

  objName (Class a) = Class (name a)
  objName (Macro a) = Macro (name a)

  instance ForAllV HasName a => HasName (Object a) where
    name (Class a) = name a
    name (Macro a) = name a

  instance ForAllV HasSource a => HasSource (Object a) where
    source (Class a) = source a
    source (Macro a) = source a

  type instance ClassV Name = Name
  type instance MacroV Name = Name

  type instance ClassV Self = ClassData
  type instance MacroV Self = MacroD

  objClass :: Object Self -> ClassData
  objClass (Class a) = a
  objClass a         = error $ "class_: Encountered " ++ name a ++ "."

  class Class_ r where
    class_ :: r

  instance Class_ (Object Self -> ClassData) where
    class_ = objClass

  instance Class_ (Name -> Object Name) where
    class_ = Class
