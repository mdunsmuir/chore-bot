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

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Time
import State

main = do
  acid <- openLocalState defaultState

  chores <- query acid GetChores
  forM_ chores (putStrLn . show)

  updateToday acid
  calendar <- query acid GetCalendar
  forM_ calendar (putStrLn . show)

  closeAcidState acid

updateToday :: AcidState State -> IO ()
updateToday acid = do
  t0 <- getCurrentTime
  tz <- getCurrentTimeZone
  let (LocalTime d0 _) = utcToLocalTime tz t0
  update acid (UpdateState d0)

-- below here cheesy stuff for setting up

michael :: Person
michael = Person "Michael"

emily :: Person
emily = Person "Emily"

dishes :: AcidState State -> IO ()
dishes acid = do  
  let duty = Alternating [michael, emily]
      rec = EveryNDays 1
      chore = Chore "Dishes" rec duty
  update acid (AddChore chore)
