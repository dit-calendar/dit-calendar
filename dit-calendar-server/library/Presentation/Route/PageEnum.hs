{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Presentation.Route.PageEnum ( Sitemap(..), urlSitemapParser ) where

import           Prelude                     hiding ((.))

import           Control.Category            ((.))
import           Data.Data                   (Data, Typeable)
import           Happstack.Authenticate.Core (AuthenticateURL, authenticateURL)
import           Text.Boomerang.TH           (makeBoomerangs)
import           Web.Routes.Boomerang        ((:-), Router, int, lit, (</>),
                                              (<>))

import           Data.Domain.Types           (EntryId, TaskId, UserId)


--A url type
data Sitemap
    = Home
    | Logout
    | Authenticate AuthenticateURL
    | Userdetail
    --calendar mapping
    | CalendarEntry
    | CalendarFilter
    | CalendarEntryDetail EntryId
    | CalendarTelegramLinks EntryId
    -- tasks mapping
    | CalendarTask EntryId
    | CalendarTaskDetail EntryId TaskId
    | TaskWithTelegramLink TaskId
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(makeBoomerangs ''Sitemap)

urlSitemapParser :: Router () (Sitemap :- ())
urlSitemapParser =
       rHome
    <> rLogout . lit "logout"
    <> rAuthenticate . (lit "authenticate" </> authenticateURL)
    <> lit "users" . userMapping
    <> lit "calendarentries" . calendarTaskMapping
    <> rCalendarFilter . lit "calendarFilter"
    where
        userMapping = rUserdetail </> lit "me"
        calendarTaskMapping =
            calendarMapping
            <> taskMapping
        calendarMapping =
            rCalendarEntry --create, findAll
            <> rCalendarEntryDetail </> int -- read, update, delete
            <> rCalendarTelegramLinks </> int </> lit "telegramlinks" -- get all telegramtaskassignments of calendar
        taskMapping =
            rCalendarTask </> int </> lit "tasks" -- create, findAll
            <> rCalendarTaskDetail </> int </> lit "tasks" </> int -- read, update, delete
            <> rTaskWithTelegramLink </> lit "tasks" </> int </> lit "assignment" -- add/remove telegramLink to Task
