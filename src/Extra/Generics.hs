{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Extra.Generics
    ( gFind
    , Extra.Generics.GShow
    , gshow
    , gshows
    , gshowsPrec
    ) where

import Control.Monad (MonadPlus, msum)
import Data.Generics (Data, listify, Proxy, Typeable)
import Generic.Data (gshowsPrec)
import Generic.Data.Internal.Show (GShow)
import GHC.Generics (Generic, Rep)

-- | @gFind a@ will extract any elements of type @b@ from
-- @a@'s structure in accordance with the MonadPlus
-- instance, e.g. Maybe Foo will return the first Foo
-- found while [Foo] will return the list of Foos found.
gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)

type GShow a = (Generic a, Generic.Data.Internal.Show.GShow Proxy (Rep a))

-- | Generic version of show based on generic-data's gshowsPrec function.
gshow :: (Generic a, Generic.Data.Internal.Show.GShow Proxy (Rep a)) => a -> String
gshow x = gshows x ""

gshows :: (Generic a, Generic.Data.Internal.Show.GShow Proxy (Rep a)) => a -> ShowS
gshows x = gshowsPrec 0 x
