module UI
    (
    ) where

import Brick.Widgets.Core ( (<+>)
                          , (<=>)
                          )
import Control.Applicative ( liftA2 )

import qualified Brick
import qualified Brick.Widgets.Core as Core
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Border.Style as BorderStyle
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
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
                                               $ Border.hBorderWithLabel (Core.str (' ' : show day ++ " "))
                                              <=> Core.vBox (map buildEventWidget events')
          buildEventsLongerThanOneDayWidget events' = Border.hBorderWithLabel (Core.str " Others ")
                                                   <=> Core.vBox (map buildEventLongerThanOneDayWidget events')
          buildEventLongerThanOneDayWidget event' = Core.str ( (show . Maybe.fromJust . eventDate Calendar.startingDate) event' ++ " - " ++ (show . Maybe.fromJust . eventDate Calendar.endingDate) event')
                                                 <+> Core.txtWrap (Calendar.object event')

buildEventWidget :: Calendar.Event -> Brick.Widget n
buildEventWidget event = Core.str (formatEventTime event) <+> Core.txtWrap (Calendar.object event)
    where
        formatEventTime event
            | Calendar.allDay event = "All day"
            | otherwise = eventTime Calendar.startingDate event ++ " - " ++ eventTime Calendar.endingDate event
        eventTime accessor event = show . Maybe.fromJust $ Time.localTimeOfDay . Calendar.fromULavalTime <$> accessor event

sortEvents :: [Calendar.Event] -> (Map.Map Time.Day [Calendar.Event], [Calendar.Event])
sortEvents events = (eventsByStartingDate, eventsLongerThanOneDay)
    where eventsByStartingDate = List.foldl' (\m event -> Map.insertWith (++) (Maybe.fromJust . eventStartingDate $ event) (pure event) m) Map.empty filteredEvents
          filteredEvents = List.sortOn (Maybe.fromJust . Calendar.startingDate) $ filter (compareEventDates' (==)) events
          eventsLongerThanOneDay = filter (compareEventDates (/=)) events
          compareEventDates compare event = not (Calendar.allDay event) && compareEventDates' compare event
          compareEventDates' compare event = Maybe.fromMaybe False $ liftA2 compare (eventStartingDate event) (eventEndingDate event)
          eventStartingDate = eventDate Calendar.startingDate
          eventEndingDate = eventDate Calendar.endingDate

eventDate :: (Calendar.Event -> Maybe Calendar.ULavalTime) -> Calendar.Event -> Maybe Time.Day
eventDate accessor event = Time.localDay . Calendar.fromULavalTime <$> accessor event
