import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import CfgPars

main :: IO()
main = do
	g <- readFile "data/client.cnf"
	initGUI
        Just xml <- xmlNew "client.glade"
        window <- xmlGetWidget xml castToWindow "window1"
	windowSetTitle window (parser "name" g ++  " - G*I*F*T Client")
	onDestroy window mainQuit
	mainGUI
