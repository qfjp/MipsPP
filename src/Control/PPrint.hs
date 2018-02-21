module Control.PPrint where

import           Padelude

class PPrint a where
    pprint :: a -> Text
