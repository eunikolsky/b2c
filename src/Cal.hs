{-# LANGUAGE OverloadedStrings #-}

module Cal where

import Types (Birthday(..), Contact(..), Name(..), Year)

import qualified Data.ByteString.Lazy as BSL
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text.Lazy (empty, fromStrict)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Traversable (for)

import Data.Default
import Data.UUID
import Data.UUID.V4
import Text.ICalendar hiding (Contact)

sampleContacts :: [Contact]
sampleContacts =
  [ Contact (Name "Zaphod Bebblebrox", Partial 12 31)
  , Contact (Name "Адам Форд", Full $ fromGregorian 1980 11 11)
  ]

-- |Alarm stays the same for all events. (But need to verify that an `empty`
-- |description works fine.)
alarm = VAlarmDisplay
  { vaDescription=Description empty Nothing Nothing def
  , vaTrigger=TriggerDuration (DurationTime Positive 7 0 0) Start def
  , vaRepeat=def
  , vaDuration=Nothing
  , vaOther=mempty
  , vaActionOther=def
  }

-- |Recurrence format stays the same for all events.
recurrence = Recur
  { recurFreq=Yearly
  , recurUntilCount=Nothing
  , recurInterval=1
  , recurBySecond=mempty
  , recurByMinute=mempty
  , recurByHour=mempty
  , recurByDay=mempty
  , recurByMonthDay=mempty
  , recurByYearDay=mempty
  , recurByWeekNo=mempty
  , recurByMonth=mempty
  , recurBySetPos=mempty
  , recurWkSt=Text.ICalendar.Monday
  }

replaceYear :: Year -> Day -> Day
replaceYear year day = fromGregorian year month dayOfMonth
  where (_, month, dayOfMonth) = toGregorian day

eventStartDate :: Year -> Birthday -> Day
eventStartDate year (Full day) = replaceYear year day
eventStartDate year (Partial month day) = fromGregorian year month day

event :: Year -> UUID -> UTCTime -> Contact -> VEvent
event startYear uuid createdAt (Contact (Name name, birthday)) = VEvent
  { veDTStamp = DTStamp createdAt def
  , veUID=UID (fromStrict $ toText uuid) def
  , veClass=def
  , veDTStart=Just (DTStartDate (Date startDate) def)
  , veCreated=Nothing
  , veDescription=Nothing
  , veGeo=Nothing
  , veLastMod=Nothing
  , veLocation=Nothing
  , veOrganizer=Nothing
  , vePriority=def
  , veSeq=Sequence 1 def
  , veStatus=Nothing
  , veSummary=Just (Summary (fromStrict name <> "'s Birthday") Nothing Nothing def)
  , veTransp=def
  , veUrl=Nothing
  , veRecurId=Nothing
  , veDTEndDuration=Just (Left (DTEndDate (Date endDate) def))
  , veAttach=mempty
  , veAttendee=mempty
  , veCategories=mempty
  , veComment=mempty
  , veContact=mempty
  , veExDate=mempty
  , veRStatus=mempty
  , veRelated=mempty
  , veResources=mempty
  , veRDate=mempty
  , veAlarms=S.singleton alarm
  , veOther=mempty
  , veRRule=S.singleton (RRule recurrence def) }

  where
    startDate = eventStartDate startYear birthday
    endDate = addDays 1 startDate

calendar :: [Contact] -> IO VCalendar
calendar contacts = do
  (startYear, _, _) <- toGregorian . utctDay <$> getCurrentTime
  events <- for contacts $ \contact ->
    event <$> pure startYear <*> nextRandom <*> getCurrentTime <*> pure contact
  let eventsMap = M.fromList $ events <&> \event -> ((uidValue $ veUID event, Nothing), event)

  pure $ def
    { vcMethod=Just (Method "PUBLISH" def)
    , vcEvents = eventsMap
    }

calendarData :: [Contact] -> IO BSL.ByteString
calendarData contacts = printICalendar def <$> calendar contacts
