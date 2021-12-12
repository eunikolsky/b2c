{-# LANGUAGE OverloadedStrings #-}

module Cal where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Clock

import Data.Default
import Text.ICalendar

alarm = VAlarmDisplay
  { vaDescription=Description "Зафод's Birthday" Nothing Nothing def
  , vaTrigger=TriggerDuration (DurationTime Positive 7 0 0) Start def
  , vaRepeat=def
  , vaDuration=Nothing
  , vaOther=mempty
  , vaActionOther=def
  }

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

event now = VEvent
  { veDTStamp = DTStamp now def
  , veUID=UID "uid" def
  , veClass=def
  , veDTStart=Just (DTStartDate (Date $ fromGregorian 2021 04 01) def)
  , veCreated=Nothing
  , veDescription=Nothing
  , veGeo=Nothing
  , veLastMod=Nothing
  , veLocation=Nothing
  , veOrganizer=Nothing
  , vePriority=def
  , veSeq=Sequence 1 def
  , veStatus=Nothing
  , veSummary=Just (Summary "Зафод's Birthday" Nothing Nothing def)
  , veTransp=def
  , veUrl=Nothing
  , veRecurId=Nothing
  , veDTEndDuration=Just (Left (DTEndDate (Date $ fromGregorian 2021 04 02) def))
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

calendar now = def
  { vcMethod=Just (Method "PUBLISH" def)
  , vcEvents = M.fromDistinctAscList [(("UID1", Nothing), event now)]
  }

calendarData = do
  now <- getCurrentTime
  pure $ printICalendar def (calendar now)
