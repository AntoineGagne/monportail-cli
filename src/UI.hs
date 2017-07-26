{-# LANGUAGE OverloadedStrings #-}

module UI
    ( displayEvents
    ) where

import Brick.Widgets.Core ( (<+>)
                          , (<=>)
                          )
import Control.Applicative ( liftA2 )
import Data.Monoid ( (<>) )

import qualified Brick
import qualified Brick.Widgets.Core as Core
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Brick.Types as BrickTypes
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified Calendar


data View = CalendarView
          | CourseView
          | NotificationView
          deriving (Show)

-- TODO: Refactor this module...
displayEvents :: [Calendar.Event] -> Brick.Widget n
displayEvents events = let (eventsByStartingDate, eventsLongerThanOneDay) = sortEvents events
                           in normalEventsWidgets eventsByStartingDate
                           <=> buildEventsLongerThanOneDayWidget eventsLongerThanOneDay
    where normalEventsWidgets = Map.foldlWithKey' (\widget day events' -> widget <=> buildCompleteEventWidget day events') Core.emptyWidget
          buildCompleteEventWidget day events' = Core.withBorderStyle BorderStyle.unicode
                                               $ Border.hBorderWithLabel (Core.txt (" " <> (Text.pack . show) day <> " "))
                                              <=> Core.vBox (map buildEventWidget events')
          buildEventsLongerThanOneDayWidget events' = Border.hBorderWithLabel (Core.txt " Others ")
                                                   <=> Core.vBox (map buildEventLongerThanOneDayWidget events')
          buildEventLongerThanOneDayWidget event' = padBox (Core.txt ( (Text.pack . show . eventDate Calendar.startingDate) event' <> " – " <> (Text.pack . show . eventDate Calendar.endingDate) event')
                                                                    <+> Core.padLeft (BrickTypes.Pad 4) (Core.txtWrap (Calendar.object event')))

buildEventWidget :: Calendar.Event -> Brick.Widget n
buildEventWidget event = padBox (Core.txt (formatEventTime event) <+> Core.padLeft (BrickTypes.Pad 4) (Core.txtWrap (Calendar.object event)))
    where
        formatEventTime event
            | Calendar.allDay event = "All day"
            | otherwise = eventTime Calendar.startingDate event <> " – " <> eventTime Calendar.endingDate event
        eventTime accessor event = Text.pack . show . Maybe.fromJust $ Time.localTimeOfDay . Calendar.fromULavalTime <$> accessor event

sortEvents :: [Calendar.Event] -> (Map.Map Time.Day [Calendar.Event], [Calendar.Event])
sortEvents events = (eventsByStartingDate, eventsLongerThanOneDay)
    where eventsByStartingDate = List.foldl' (\m event -> Map.insertWith (++) (eventStartingDate event) (pure event) m) Map.empty filteredEvents
          filteredEvents = List.sortOn Calendar.startingDate $ filter (compareEventDates (==)) events
          eventsLongerThanOneDay = filter (compareEventDates (/=)) events
          compareEventDates compare event = compare (eventStartingDate event) (eventEndingDate event)
          eventStartingDate = eventDate Calendar.startingDate
          eventEndingDate = eventDate Calendar.endingDate

eventDate :: (Calendar.Event -> Maybe Calendar.ULavalTime) -> Calendar.Event -> Time.Day
eventDate accessor event = Maybe.fromJust $ Time.localDay . Calendar.fromULavalTime <$> accessor event

padBox :: Brick.Widget n -> Brick.Widget n
padBox = Core.padTop (BrickTypes.Pad 1) . Core.padLeftRight 2
