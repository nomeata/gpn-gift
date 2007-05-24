{-# OPTIONS_GHC -fimplicit-params #-}


import Graphics.UI.Gtk
import Graphics.Rendering.Cairo 
import Text.Printf

import Time
import DatT

width :: Num a => a
width = 1024
height :: Num a => a
height = 768 

data EventState = Passed | Running | Future 


main = do
	initGUI
	window <- windowNew
	canvas <- drawingAreaNew
	windowSetResizable window False
	widgetSetSizeRequest window width height
	onButtonPress window $ const (widgetDestroy window >> return True)
	onDestroy window mainQuit
	withImageSurfaceFromPNG "GPN6_logo.png" $ \logo -> do
		onExpose canvas $ const $ render canvas logo
		timeoutAdd (widgetQueueDraw canvas >> return True) 500
		set window [containerChild := canvas]
		widgetShowAll window
		mainGUI

render canvas logo = do
	time <- now
	win <- widgetGetDrawWindow canvas
	--(width, height) <- widgetGetSize canvas
	renderWithDrawable win $ renderC logo time test_data

renderC logo now events = do
	selectFontFace "Mono" FontSlantNormal FontWeightNormal
	drawbg logo
	clock now
	fahrplan now events
	return True

drawbg logo = do
	save
	setSourceRGB 0.2 0.2 0.2
	paint
	w <- imageSurfaceGetWidth logo
	h <- imageSurfaceGetHeight logo
	setSourceSurface logo
		(fromIntegral ((width - w) `div` 2))
		(fromIntegral  (height - h - 20))
	paint
	restore

clock time = do
	save
	let pad = 20
	let text = printTime time
	setFontSize 20
	setSourceRGB 1 1 1
  	(TextExtents xb yb w h _ _) <- textExtents text
	moveTo (width - w - pad) (h + pad)
	showText text
	restore

fahrplan now events = do
	let show_events = take 10 $ filter (not . isPassed) $ map (label now) $ events
	mapM_ (uncurry markup) (zip show_events [1..])

isPassed (Passed, _) = True
isPassed _           = False

label time event | eEndTime event < time  = (Passed,    event)
                 | eTime event    < time  = (Running,   event)
                 | otherwise              = (Future,    event)

markup (lable, event) line = do
	save
	let place_and_time = printf "[%s @ %s]" 
		(printTime (eTime event)) (show (eRoom event))
	let y = 50 * line
	moveTo 20 y
	setFontSize 20
	setSourceRGB 0.8 0.8 0.8
	showText place_and_time
	setFontSize 40
	moveTo 350 y
	case lable of
		Running -> setSourceRGB 1 0 0 
		Future  -> setSourceRGB 1 1 1
	showText (eName event)
	restore

test_data :: Fahrplan
test_data = [
	(1,
	"Gulasch",
	Chaos,
	Time 0 12 23,
	RunTime 1 15
	),
	(1,
	"Gulaschgh",
	Chaos,
	Time 1 00 23,
	RunTime 1 15
	),
	(2,
	"Hackfleisch",
	HackCenter,
	Time 1 15 23,
	RunTime 1 30
	)
	]


foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

