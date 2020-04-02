module Fifteen where

    import Data.Monoid

    data Optional a =
        Nada | Only a deriving (Eq, Show)


    instance Monoid a => Monoid (Optional a) where
        mempty = Nada

    instance Semigroup a => Semigroup (Optional a) where
        (Only v1) <> (Only v2) = Only (v1 <> v2)
        Nada <> (Only v2) = Only v2
        (Only v1) <> Nada = Only v1
        Nada <> Nada = Nada