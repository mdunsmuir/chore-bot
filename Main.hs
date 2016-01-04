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

import Data.Time
import State
import API

main = do
  acid <- openLocalState defaultState
  server acid

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
