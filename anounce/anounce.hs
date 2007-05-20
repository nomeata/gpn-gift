{-# OPTIONS_GHC -fimplicit-params #-}

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo 

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
	set window [containerChild := canvas]
	widgetShowAll window
	mainGUI

render canvas = do
	win <- widgetGetDrawWindow canvas
	--(width, height) <- widgetGetSize canvas
	renderWithDrawable win $ renderC now test_data

renderC now events = do
	clock now
	return True

clock (day, hour, minute) = do
	return ()



now = (1,12,00)

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

