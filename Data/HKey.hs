module Data.HKey(  HKey
            , withKey
            , T
            , createKey
            -- * Key Monad
            , KeyM
            , getKey
            , runKeyM) where

import Data.HKeyPrivate
