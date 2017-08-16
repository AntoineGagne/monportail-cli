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

import Calendar ( sortEvents
                , eventDate
                , Event (..)
                )

import qualified Calendar


data View = CalendarView
          | CourseView
          | NotificationView
          deriving (Show)

-- TODO: Refactor this module...
displayEvents :: [Event] -> Brick.Widget n
displayEvents events = let (eventsByStartingDate, eventsLongerThanOneDay) = sortEvents events
                           in normalEventsWidgets eventsByStartingDate
                           <=> buildEventsLongerThanOneDayWidget eventsLongerThanOneDay
    where
        normalEventsWidgets = Map.foldlWithKey' (\widget day events' -> widget <=> buildCompleteEventWidget day events') Core.emptyWidget
        buildCompleteEventWidget day events' = Core.withBorderStyle BorderStyle.unicode
                                             $ Border.hBorderWithLabel (Core.txt (" " <> fromShowable day <> " "))
                                            <=> Core.vBox (map buildEventWidget events')
        buildEventsLongerThanOneDayWidget events' = Border.hBorderWithLabel (Core.txt " Others ")
                                                 <=> Core.vBox (map buildEventLongerThanOneDayWidget events')
        buildEventLongerThanOneDayWidget event' = padBox ( Core.txt ( fromStartingDate event' <> " – " <> fromEndingDate event')
                                                        <+> padLeft 4 (Core.txtWrap (Calendar.object event'))
                                                         )
        fromStartingDate = fromDate Calendar.startingDate
        fromEndingDate = fromDate Calendar.endingDate
        fromDate f = fromShowable . eventDate f

buildEventWidget :: Event -> Brick.Widget n
buildEventWidget event = padBox (Core.txt (formatEventTime event) <+> padLeft 4 (Core.txtWrap (Calendar.object event)))
    where
        formatEventTime event
            | Calendar.allDay event = "All day"
            | otherwise = eventTime Calendar.startingDate event <> " – " <> eventTime Calendar.endingDate event
        eventTime accessor event = fromShowable . Maybe.fromJust $ Time.localTimeOfDay . Calendar.fromULavalTime <$> accessor event

fromShowable :: Show s => s -> Text.Text
fromShowable = Text.pack . show

padLeft :: Int -> Brick.Widget n -> Brick.Widget n
padLeft n = Core.padLeft (BrickTypes.Pad n)

padBox :: Brick.Widget n -> Brick.Widget n
padBox = Core.padTop (BrickTypes.Pad 1) . Core.padBottom (BrickTypes.Pad 1) . Core.padLeftRight 2
