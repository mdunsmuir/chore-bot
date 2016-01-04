{-
    CHORE-BOT
    Copyright (C) 2016  Michael Dunsmuir

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

module State
( Person (..)
, personName

, Duty (..)

, WeekDay (..)

, Recurrence (..)

, Chore (..)
, choreName, choreRecurrence

, Instance (..)
, instanceChore
, instanceCompleted

, State (..)
, stateChores, stateCalendar
, defaultState

, GetChores (..)
, GetCalendar (..)
, AddChore (..)
, RemoveChore (..)
, UpdateState (..)

, updateToday

, module Data.Acid
) where

import Control.Monad (forM_, when)
import Data.Typeable
import Data.List (cycle, find)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Time (getCurrentTime, getCurrentTimeZone, LocalTime (..), utcToLocalTime)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format
import Data.SafeCopy (base, deriveSafeCopy)
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Reader.Class (ask)
import Data.Acid
import Control.Lens (set, over)
import Control.Lens.TH
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as A
import Data.Aeson.TH

data Person = Person
            { _personName :: T.Text
            } deriving (Eq, Ord, Show)

makeLenses ''Person
$(deriveSafeCopy 0 'base ''Person)
$(deriveJSON defaultOptions { fieldLabelModifier = tail } ''Person)

data Duty = Only { _dutyPerson :: Person }
          | Alternating { _dutyPeople :: [Person] }
            deriving (Eq, Ord, Show)

$(deriveSafeCopy 0 'base ''Duty)
$(deriveJSON defaultOptions { fieldLabelModifier = tail } ''Duty)

nextPerson :: Duty -> Person -> Person
nextPerson (Only p) _ = p
nextPerson (Alternating ps) p =
  let cycled = cycle ps
  in  head $ tail $ dropWhile (p /=) cycled

firstOwner :: Duty -> Person
firstOwner (Only p) = p
firstOwner (Alternating (p:_)) = p

data WeekDay = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
               deriving (Eq, Ord, Enum, Show)

$(deriveSafeCopy 0 'base ''WeekDay)
$(deriveJSON defaultOptions { fieldLabelModifier = tail } ''WeekDay)

data Recurrence = EveryWeekDay { _recurrenceWeekday :: WeekDay }
                | EveryNDays { _recurrenceNDays :: Int }
                  deriving (Eq, Ord, Show)

$(deriveSafeCopy 0 'base ''Recurrence)
$(deriveJSON defaultOptions { fieldLabelModifier = tail } ''Recurrence)

recurrenceHit :: Recurrence -> Day -> Day -> Bool
recurrenceHit (EveryWeekDay wd) d1 d2 =
  let (_, _, weekDate) = toWeekDate d2
  in  if weekDate /= fromEnum wd
        then False
        else (fromEnum d2 - fromEnum d1) == 7
recurrenceHit (EveryNDays nDays) d1 d2 =
  (fromEnum d2 - fromEnum d1) == nDays

isAllowableFirstDay :: Recurrence -> Day -> Bool
isAllowableFirstDay (EveryWeekDay wd) day =
  let (_, _, weekDate) = toWeekDate day
  in  weekDate == fromEnum wd
isAllowableFirstDay _ _ = True

data Chore = Chore
           { _choreName :: T.Text
           , _choreRecurrence :: Recurrence
           , _choreDuty :: Duty
           } deriving (Eq, Ord, Show)

makeLenses ''Chore
$(deriveSafeCopy 0 'base ''Chore)
$(deriveJSON defaultOptions { fieldLabelModifier = tail } ''Chore)

data Instance = Instance
              { _instanceChore :: Chore
              , _instanceOwner :: Person
              , _instanceCompleted :: Maybe Day
              } deriving (Eq, Ord, Show)

makeLenses ''Instance
$(deriveSafeCopy 0 'base ''Instance)
$(deriveJSON defaultOptions { fieldLabelModifier = tail } ''Instance)

data CalendarItem = CalendarItem
                  { _calendarItemDay :: Day
                  , _calendarItemInstance :: Instance
                  } deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''CalendarItem)
$(deriveJSON defaultOptions { fieldLabelModifier = tail } ''CalendarItem)

instance Ord CalendarItem where
  (CalendarItem d1 i1) `compare` (CalendarItem d2 i2) =
    (d1, i1) `compare` (d2, i2)

data State = State
           { _stateChores :: S.Set Chore
           , _stateCalendar :: S.Set CalendarItem
           , _stateLastUpdate :: Day
           } deriving (Eq, Ord, Show, Typeable)

defaultState :: State
defaultState = State S.empty S.empty $ fromGregorian 2015 12 1

makeLenses ''State
$(deriveSafeCopy 0 'base ''State)

getChores :: Query State (S.Set Chore)
getChores = _stateChores <$> ask

getCalendar :: Query State (S.Set CalendarItem)
getCalendar = _stateCalendar <$> ask

addChore :: Chore -> Update State ()
addChore = modify . over stateChores . S.insert

removeChore :: Chore -> Update State ()
removeChore = modify . over stateChores . S.delete

updateState :: Day -> Update State ()
updateState day = do
  State _ _ lastUpdate <- get
  if day == lastUpdate
    then return ()
    else do forM_ (tail [lastUpdate..day]) updateDay
            modify $ set stateLastUpdate day

-- these functions are helpers for 'updateState' above.

updateDay :: Day -> Update State ()
updateDay day = do
  State chores calendar _ <- get
  let calDesc = S.toDescList calendar
  forM_ chores $ \chore@(Chore _ rec _) -> do
    let maybeFound = find ((chore ==) . _instanceChore . _calendarItemInstance) calDesc
    case maybeFound of
      Nothing -> makeFirst day chore
      Just (CalendarItem prevDay prevInstance) -> 
        when (recurrenceHit rec prevDay day) $ makeChore day prevInstance

makeChore :: Day -> Instance -> Update State ()
makeChore day (Instance chore prevOwner _) =
  let nextOwner = nextPerson (_choreDuty chore) prevOwner
      nextInstance = Instance chore nextOwner Nothing
      tup = CalendarItem day nextInstance
  in  modify $ over stateCalendar $ S.insert tup
  
makeFirst :: Day -> Chore -> Update State ()
makeFirst day chore@(Chore _ rec duty) = 
  let firstInstance = Instance chore (firstOwner duty) Nothing
      tup = CalendarItem day firstInstance
  in  when (isAllowableFirstDay rec day) $
        modify $ over stateCalendar $ S.insert tup

$(makeAcidic ''State ['getChores, 'getCalendar, 'addChore, 'removeChore, 'updateState])

updateToday :: AcidState State -> IO ()
updateToday acid = do
  t0 <- getCurrentTime
  tz <- getCurrentTimeZone
  let (LocalTime d0 _) = utcToLocalTime tz t0
  update acid (UpdateState d0)

