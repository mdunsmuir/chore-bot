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

module API where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Aeson hiding (json)
import Data.String.Conversions
import Web.Scotty
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import State

server :: AcidState State -> IO ()
server acid = scotty 3000 $ do

  middleware logStdoutDev

  get "/chores" $ do
    chores <- liftIO $ query acid GetChores 
    json chores

  get "/chores/:id" $ do
    id <- param "id"
    maybeChore <- liftIO $ query acid $ GetChore id
    case maybeChore of
      Just chore -> json chore
      Nothing -> do
        status status404
        json ("chore not found" :: T.Text)

  get "/instances" $ do
    instances <- liftIO $ query acid GetInstances
    json instances

  get "/instances/:id" $ do
    id <- param "id"
    maybeInstance <- liftIO $ query acid $ GetInstance id
    case maybeInstance of
      Just inst -> json inst
      Nothing -> do
        status status404
        json ("instance not found" :: T.Text)

  get "/calendar" $ do
    liftIO $ updateToday acid
    calendar <- liftIO $ query acid GetCalendar

    -- unfortunately for some reason Scotty was eating the quotation
    -- marks around my dates so I had to do this rather than use the
    -- handy 'json' action.
    setHeader "Content-Type" "application/json; charset=utf-8"
    raw $ encode $ toJSON calendar


