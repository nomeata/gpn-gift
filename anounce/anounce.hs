{-# OPTIONS_GHC -fimplicit-params #-}

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo 
import Text.Printf
import System.Time

width :: Num a => a
width = 1024
height :: Num a => a
height = 768 

data EventState = Passed | Running | Future | FarFuture

data Time = Time { tDay :: Int, tHour :: Int, tMin :: Int } deriving (Eq, Ord)
data RunTime = RunTime { rtHour :: Int, rtMin :: Int } deriving (Eq, Ord)

instance Show Time where
	show time = printf "Tag %d − %02.0d:%02.0d" (tDay time) (tHour time) (tMin time)


main = do
	initGUI
	window <- windowNew
	canvas <- drawingAreaNew
	windowSetResizable window False
	widgetSetSizeRequest window width height
	onButtonPress window $ const (do widgetDestroy window; return True)
	onDestroy window mainQuit
	onExpose canvas $ const $ render canvas
	timeoutAdd (render canvas) 500
	set window [containerChild := canvas]
	widgetShowAll window
	mainGUI

render canvas = do
	time <- now
	win <- widgetGetDrawWindow canvas
	--(width, height) <- widgetGetSize canvas
	renderWithDrawable win $ renderC time test_data

renderC now events = do
	selectFontFace "Mono" FontSlantNormal FontWeightNormal
	drawbg
	clock now
	fahrplan now events
	return True

drawbg = do
	setSourceRGB 1 1 1
	paint

clock time = do
	let pad = 20
	let text = show time
	setFontSize 20
	setSourceRGB 0 0 0
  	(TextExtents xb yb w h _ _) <- textExtents text
	showText (show xb)
	moveTo (width - w - pad) (h + pad)
	showText text

fahrplan now events = do
	let show_events = take 10 $ filter (not . isPassed) $ map (label now) $ events
	mapM_ (uncurry markup) (zip show_events [1..])

isPassed (Passed, _) = True
isPassed _           = False

label time event | tDay (eTime event) > tDay time = (FarFuture, event)
                 | eTime event < time             = (Passed,    event)
                 | eTime event < time             = (Passed,    event)

markup (lable, event) line = do
	let place_and_time = printf "[%s @ %s]"
		(show (eTime event)) (eRoom event)
	let y = 50 * line
	moveTo 20 y
	setFontSize 20
	setSourceRGB 0.3 0.3 0.3
	showText place_and_time
	setFontSize 40
	moveTo 350 y
	setSourceRGB 0 0 0
	showText (eName event)

now = do
	time <- getClockTime >>= toCalendarTime
	return $ Time { tDay = ctDay time - 20, tHour = ctHour time, tMin = ctMin time}

eName    (_, s, _, _, _) = s
eRoom    (_, _, s, _, _) = s
eTime    (_, _, _, t, _) = t
eRunTime (_, _, _, _, t) = t
eEndTime event = fix $ start {tHour = tHour start + rtHour rt, tMin = tMin start + rtMin rt}
  where start = eTime event
	rt = eRunTime event
	fix = fixd . fixh
	fixh time = let (hd, m) = tMin time `divMod` 60 in
			time {tHour = tHour time + hd, tMin = m}
	fixd time = let (dd, h) = tHour time `divMod` 24 in
			time {tDay = tDay time + dd, tHour = h}
		
test_data :: [(Int, String, String, Time, RunTime)]
test_data = [
	(1,
	"Gulasch",
	"Chaos",
	Time 0 12 23,
	RunTime 1 15
	),
	(1,
	"Gulaschgh",
	"Chaos",
	Time 1 13 23,
	RunTime 1 15
	),
	(2,
	"Hackfleisch",
	"Ordnung",
	Time 1 15 23,
	RunTime 1 30
	)
	]


foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

