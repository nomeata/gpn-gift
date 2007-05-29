import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Network
import Control.Monad
import IO
import Data.Maybe

import Time
import DatT

main :: IO()
main = do
	conf <- read `liftM` readFile "data/client.cnf" -- reading the server.cnf
	let port = read (fromJust $ lookup "port" conf) :: Int
	h <- connectTo "localhost"  (PortNumber (fromIntegral port))
	hSetBuffering h LineBuffering
	login h conf

	initGUI

        Just xml <- xmlNew "client.glade"
        window <- xmlGetWidget xml castToWindow "window1"

	tv <- xmlGetWidget xml castToTreeView "treeview1"
	b_quit <- xmlGetWidget xml castToButton "quit"
	b_refresh <- xmlGetWidget xml castToButton "refresh"


	onClicked b_quit $ do widgetDestroy window

	onDestroy window mainQuit
	mainGUI

login h conf = do
	hPutStrLn h "guest"
	hPutStrLn h "guest"
