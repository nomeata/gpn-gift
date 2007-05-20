{-# OPTIONS_GHC -fimplicit-params #-}

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo 
import Text.Printf
import System.Time

width :: Num a => a
width = 1024
height :: Num a => a
height = 768 

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
	drawbg
	clock now
	return True

drawbg = do
	setSourceRGB 1 1 1
	paint

clock (day, hour, minute, second) = do
	let pad = 20
	let text = printf "Tag %d − %02.0d:%02.0d:%02.0d" day hour minute second
	setFontSize 20
	setSourceRGB 0 0 0
  	te@(TextExtents xb yb w h _ _) <- textExtents text
	showText (show xb)
	moveTo (width - w - pad) (h + pad)
	showText text


now = do
	time <- getClockTime >>= toCalendarTime
	return (ctDay time - 20, ctHour time, ctMin time, ctSec time)

test_data = [
	(1,
	"Gulasch",
	"Chaos",
	1,
	12,23,
	1,15),
	(2,
	"Hackfleisch",
	"Ordnung",
	1,
	13,23,
	1,15)
	]

