module Types
  ( Birthday(..)
  , Contact(..)
  , Name (..)
  , Year
  )
  where

import Data.Text (Text)
import Data.Time.Calendar

-- https://github.com/haskell/time/commit/c69d1bd9b06d809f69fec8504896d0834d91476e
-- #if !MIN_VERSION_time(1,11,0)
-- `#if` doesn't work: `error on input ‘#’`
type Year = Integer
type MonthOfYear = Int
type DayOfMonth = Int
-- #endif

newtype Name = Name Text
  deriving Show

data Birthday
  = Full Day
  | Partial MonthOfYear DayOfMonth
  deriving Show

newtype Contact = Contact (Name, Birthday)
  deriving Show
