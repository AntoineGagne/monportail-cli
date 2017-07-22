module UI
    (
    ) where

import Brick.Widgets.Core ( (<+>) )
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

displayEvents :: [Calendar.Event] -> [Brick.Widget n]
displayEvents events = undefined
    where normalEventsWidgets = undefined

buildEventWidget :: Calendar.Event -> Brick.Widget n
buildEventWidget event = Core.str (formatEventTime event) <+> Core.txtWrap (Calendar.object event)
    where
        formatEventTime event
            | Calendar.allDay event = "All day"
            | otherwise = eventTime Calendar.startingDate event ++
                          " - " ++
                          eventTime Calendar.endingDate event
        eventTime accessor event = show . Maybe.fromJust 
                                 $ Time.localTimeOfDay . Calendar.fromULavalTime
                                <$> accessor event

sortEvents :: [Calendar.Event] -> (Map.Map Time.Day [Calendar.Event], [Calendar.Event])
sortEvents events = (eventsByStartingDate, eventsLongerThanOneDay)
    where eventsByStartingDate =
            List.foldl' (\m event -> 
                Map.insertWith (++) (Maybe.fromJust . eventStartingDate $ event) [event] m)
                Map.empty filteredEvents
          filteredEvents =
            List.sortOn (Maybe.fromJust . Calendar.startingDate) $ filter (compareEventDates' (==)) events
          eventsLongerThanOneDay = filter (compareEventDates (/=)) events
          compareEventDates compare event = not (Calendar.allDay event) && compareEventDates' compare event
          compareEventDates' compare event =
              Maybe.fromMaybe False $ liftA2 compare (eventStartingDate event) (eventEndingDate event)
          eventStartingDate = eventDate Calendar.startingDate
          eventEndingDate = eventDate Calendar.endingDate

eventDate :: (Calendar.Event -> Maybe Calendar.ULavalTime) -> Calendar.Event -> Maybe Time.Day
eventDate accessor event = Time.localDay . Calendar.fromULavalTime <$> accessor event
