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

{-# LANGUAGE TupleSections, RankNTypes, TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

module State
( Id

, Person (..)

, personName

, Duty (..)

, WeekDay (..)

, Recurrence (..)

, Chore (..)
, choreName, choreRecurrence

, Instance (..)
, instanceChoreId
, instanceCompleted

, State (..)
, stateChores, stateInstances, stateCalendar
, defaultState

, GetChores (..)
, GetInstances (..)
, GetCalendar (..)
, GetChore (..)
, GetInstance (..)

, GetNextChoreId (..)
, GetNextInstanceId (..)

, AddChore (..)
, AddInstance (..)

, DeleteInstance (..)
, DeleteChore (..)

, UpdateState
, updateToday

, module Data.Acid
) where

import Control.Monad (forM_, forM, when)
import Data.Typeable
import Data.List (cycle, find)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import Data.Time (getCurrentTime, getCurrentTimeZone, LocalTime (..), utcToLocalTime)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format
import Data.SafeCopy (base, deriveSafeCopy)
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Reader.Class (ask)
import Data.Acid
import Control.Lens (view, set, over, Lens')
import Control.Lens.TH
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as A
import Data.Aeson.TH

type Id = Int

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
              { _instanceChoreId :: Id
              , _instanceOwner :: Person
              , _instanceCompleted :: Maybe Day
              } deriving (Eq, Ord, Show)

makeLenses ''Instance
$(deriveSafeCopy 0 'base ''Instance)
$(deriveJSON defaultOptions { fieldLabelModifier = tail } ''Instance)

data CalendarItem = CalendarItem
                  { _calendarItemDay :: Day
                  , _calendarItemInstanceId :: Id
                  } deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''CalendarItem)
$(deriveJSON defaultOptions { fieldLabelModifier = tail } ''CalendarItem)

instance Ord CalendarItem where
  (CalendarItem d1 i1) `compare` (CalendarItem d2 i2) =
    (d1, i1) `compare` (d2, i2)

data State = State
           { _stateChores :: IM.IntMap Chore
           , _stateInstances :: IM.IntMap Instance
           , _stateCalendar :: S.Set CalendarItem
           , _stateLastUpdate :: Day
           } deriving (Eq, Ord, Show, Typeable)

defaultState :: State
defaultState = State IM.empty IM.empty S.empty $ fromGregorian 2015 12 1

makeLenses ''State
$(deriveSafeCopy 0 'base ''State)

-- basic queries

getChores :: Query State (IM.IntMap Chore)
getChores = _stateChores <$> ask

getInstances :: Query State (IM.IntMap Instance)
getInstances = _stateInstances <$> ask

getCalendar :: Query State (S.Set CalendarItem)
getCalendar = _stateCalendar <$> ask

getMap :: Lens' State (IM.IntMap a) -> Id -> Query State (Maybe a)
getMap lens id = IM.lookup id . view lens <$> ask

getChore :: Id -> Query State (Maybe Chore)
getChore = getMap stateChores

getInstance :: Id -> Query State (Maybe Instance)
getInstance = getMap stateInstances

-- getting IDs

getNextId :: (State -> IM.IntMap a) -> Query State Id
getNextId getter = do
  choreMap <- getter <$> ask
  return $ if IM.null choreMap
    then 1
    else succ $ fst $ IM.findMax choreMap

getNextChoreId :: Query State Id
getNextChoreId = getNextId _stateChores

getNextInstanceId :: Query State Id
getNextInstanceId = getNextId _stateInstances

-- adding chores and instances

addMap :: Lens' State (IM.IntMap a)
       -> Query State Id
       -> a
       -> Update State Id
addMap lens getId value = do
  nextId <- liftQuery getId
  modify $ over lens $ IM.insert nextId value
  return nextId

addChore :: Chore -> Update State Id
addChore = addMap stateChores getNextChoreId

addInstance :: Day -> Instance -> Update State Id
addInstance day inst = do
  id <- addMap stateInstances getNextInstanceId inst
  modify $ over stateCalendar $ S.insert $ CalendarItem day id
  return id

-- removing chores and instances

removeMap :: Lens' State (IM.IntMap a)
          -> Id
          -> Update State (Maybe a)
removeMap lens id = do
  exists <- (IM.member id . view lens) <$> get
  case exists of
    False -> return Nothing
    True -> do
      value <- (IM.lookup id . view lens) <$> get
      modify $ over lens (IM.delete id)
      return value

deleteInstance :: Id -> Update State (Maybe Instance)
deleteInstance id = do
  let pred = (id /=) . _calendarItemInstanceId
  modify $ over stateCalendar $ S.filter pred
  removeMap stateInstances id

deleteChore :: Id -> Update State (Maybe (Chore, [Instance]))
deleteChore id = do
  maybeValue <- removeMap stateChores id
  case maybeValue of
    Nothing -> return Nothing
    Just value -> do
      instances <- deleteInstancesWithChoreId id
      return $ Just (value, instances)

deleteInstancesWithChoreId :: Id -> Update State [Instance]
deleteInstancesWithChoreId id = do
  toDelete <- IM.filter pred . _stateInstances <$> get
  forM (IM.toList toDelete) $ \(id, inst) -> do
    deleteInstance id
    return inst
  where
    pred = (id ==) . _instanceChoreId

-- updating the state (populating the calendar and so on)

updateState :: Day -> Update State ()
updateState day = do
  State _ _ _ lastUpdate <- get
  if day == lastUpdate
    then return ()
    else do forM_ (tail [lastUpdate..day]) updateDay
            modify $ set stateLastUpdate day

-- these functions are helpers for 'updateState' above.

updateDay :: Day -> Update State ()
updateDay day = do
  cal <- liftQuery getCalendar
  calDesc <- forM (S.toDescList cal) $ \(CalendarItem day id) ->
    (day,) . fromJust <$> liftQuery (getInstance id)

  chores <- liftQuery getChores
  forM_ (IM.toList chores) $ \(id, chore) -> do
    let maybeFound = find ((id ==) . _instanceChoreId . snd) calDesc
        Chore _ rec _ = chore

    case maybeFound of
      Nothing -> makeFirst day id
      Just (prevDay, prevInstance) -> 
        when (recurrenceHit rec prevDay day) $ makeChore day prevInstance

makeFirst :: Day -> Id -> Update State ()
makeFirst day id = do
  Chore _ rec duty <- fromJust <$> liftQuery (getChore id)
  let firstInstance = Instance id (firstOwner duty) Nothing
  when (isAllowableFirstDay rec day) $
    addInstance day firstInstance >> return ()

makeChore :: Day -> Instance -> Update State ()
makeChore day prevInstance = do
  let Instance id prevOwner _ = prevInstance
  Chore _ rec duty <- fromJust <$> liftQuery (getChore id)
  let nextOwner = nextPerson duty prevOwner
      nextInstance = Instance id nextOwner Nothing
  addInstance day nextInstance
  return ()

-- finally, deriving the acid-state stuff for all the above functions

$(makeAcidic ''State ['getChores, 'getInstances, 'getCalendar, 'getChore, 'getInstance, 'getNextChoreId, 'getNextInstanceId, 'addChore, 'addInstance, 'deleteInstance, 'deleteChore, 'updateState])

updateToday :: AcidState State -> IO ()
updateToday acid = do
  t0 <- getCurrentTime
  tz <- getCurrentTimeZone
  let (LocalTime d0 _) = utcToLocalTime tz t0
  update acid (UpdateState d0)
