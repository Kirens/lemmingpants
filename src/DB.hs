{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module DB
    ( attendee
    , createAttendee
    , listAttendees
    , agendaItem
    , getAgendaItem
    , listAgendaItems
    , dbSetup
    ) where

import Control.Monad.Catch (MonadCatch)
import Data.Maybe (listToMaybe)
import Database.Selda
import Database.Selda.Backend
import Database.Selda.Generic
import Prelude hiding (id)

import qualified Types as T

attendee :: GenTable T.Attendee
attendee = genTable "attendee"
    [(T.id :: T.Attendee -> RowID) :- autoPrimaryGen
    , T.cid :- uniqueGen
    ]

-- | Creates and attendee with CID: cid' and returns the attendee.
createAttendee :: (MonadCatch m, MonadIO m) => Text -> SeldaT m T.Attendee
createAttendee cid' = do
    tryInsert (gen attendee) [ def :*: cid' ]
    fmap (fromRel . head) $ query $ do
        a@(_ :*: c) <- select (gen attendee)
        restrict (c .== text cid')
        return a

listAttendees :: (MonadCatch m, MonadIO m) => SeldaT m [T.Attendee]
listAttendees = fromRels <$> query (select (gen attendee))

-- --------------------------------------------------------------------------

agendaItem :: GenTable T.AgendaItem
agendaItem = genTable "agendaItem"
    [(T.id :: (T.AgendaItem -> RowID)) :- autoPrimaryGen
    , T.title :- uniqueGen
    ]

-- | Gets one agenda item with the corresponding id or Nothing if it doesn't correspond.
getAgendaItem :: (MonadCatch m, MonadIO m) => RowID -> SeldaT m (Maybe T.AgendaItem)
getAgendaItem i' = do
    as' <- query $ do
        as@(i :*: _) <- select (gen agendaItem)
        restrict (literal i' .== i)
        return as
    return $ listToMaybe $ fromRels $ as'

-- | Gets all agenda items ordered by order.
listAgendaItems :: (MonadCatch m, MonadIO m) => SeldaT m [T.AgendaItem]
listAgendaItems = do
    as' <- query $ do
        as@(_ :*: _ :*: _ :*: o) <- select (gen agendaItem)
        Database.Selda.order o ascending
        return as
    return $ fromRels as'

-- --------------------------------------------------------------------------

speakerList :: GenTable T.SpeakerList
speakerList = genTable "speakerList"
    [ (T.id :: (T.SpeakerList -> RowID)) :- autoPrimaryGen
    , T.agendaItem :- fkGen (gen agendaItem) aid
    ]

    where
        (aid :*: _ :*: _ :*: _) = selectors (gen agendaItem)

-- --------------------------------------------------------------------------

dbSetup :: SeldaConnection -> IO ()
dbSetup s = (flip runSeldaT) s $ do
    tryCreateTable (gen attendee)
    tryCreateTable (gen agendaItem)
    tryCreateTable (gen speakerList)
