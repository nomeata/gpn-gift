{-# OPTIONS_GHC -fimplicit-params #-}


import Graphics.UI.Gtk
import Graphics.Rendering.Cairo 
import System.Glib.MainLoop
import Control.Concurrent
import Text.Printf
import Data.IORef
import Data.Maybe
import Control.Monad
import IO
import Network

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
	fahrplan_ref <- newIORef ([] :: Fahrplan)

	windowSetResizable window False
	widgetSetSizeRequest window width height
	onButtonPress window $ const (widgetDestroy window >> return True)
	onDestroy window mainQuit

	conf <- read `liftM` readFile "data/anounce.cnf" -- reading the server.cnf
	let port = read (fromJust $ lookup "port" conf) :: Int
	h <- connectTo "localhost"  (PortNumber (fromIntegral port))
	hSetBuffering h LineBuffering
	login h conf
	hPrint h ShowFahrplan
	fahrplan_raw <- hGetLine h
	writeIORef fahrplan_ref (read fahrplan_raw)

	forkIO $ sequence_ $ repeat $ do
		hPrint h Listen
		fahrplan_raw <- hGetLine h
		writeIORef fahrplan_ref (read fahrplan_raw)
		hPrint h Listen
		return True
		widgetQueueDraw canvas


	withImageSurfaceFromPNG "GPN6_logo.png" $ \logo -> do
		onExpose canvas $ const $ render canvas logo fahrplan_ref
		timeoutAdd (widgetQueueDraw canvas >> return True) 500
		set window [containerChild := canvas]
		widgetShowAll window
		mainGUI
	
login h conf = do
	hPutStrLn h "guest"
	hPutStrLn h "guest"

render canvas logo fahrplan_ref = do
	time <- now
	win <- widgetGetDrawWindow canvas
	fahrplan <- readIORef fahrplan_ref
	--(width, height) <- widgetGetSize canvas
	renderWithDrawable win $ renderC logo time fahrplan

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


foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

