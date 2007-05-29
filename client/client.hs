import Graphics.UI.Gtk hiding (Event)
import Graphics.UI.Gtk.Glade
import qualified Graphics.UI.Gtk.ModelView as New
import Network
import Control.Monad
import IO
import Data.Maybe

import Time
import DatT

empty_event :: Event
empty_event = (0,"",Chaos,read "(0,0,0)", read "(0,0)")

setup_liststore tv = do
	fahrplan <- New.listStoreNew [empty_event]
	col1 <- New.treeViewColumnNew
	col2 <- New.treeViewColumnNew
	col3 <- New.treeViewColumnNew
	col4 <- New.treeViewColumnNew
	col5 <- New.treeViewColumnNew
	set col1 [ New.treeViewColumnTitle := "ID" ]
	set col2 [ New.treeViewColumnTitle := "Name" ]
	set col3 [ New.treeViewColumnTitle := "Raum" ]
	set col4 [ New.treeViewColumnTitle := "Zeit" ]
	set col5 [ New.treeViewColumnTitle := "Dauer" ]
	renderer1 <- New.cellRendererTextNew
	renderer2 <- New.cellRendererTextNew
	renderer3 <- New.cellRendererTextNew
	renderer4 <- New.cellRendererTextNew
	renderer5 <- New.cellRendererTextNew
	New.cellLayoutPackStart col1 renderer1 True
	New.cellLayoutPackStart col2 renderer2 True
	New.cellLayoutPackStart col3 renderer3 True
	New.cellLayoutPackStart col4 renderer4 True
	New.cellLayoutPackStart col5 renderer5 True
	New.cellLayoutSetAttributes col1 renderer1 fahrplan $ \event -> [ New.cellText := show $ eID event ]
	New.cellLayoutSetAttributes col2 renderer2 fahrplan $ \event -> [ New.cellText := eName event ]
	New.cellLayoutSetAttributes col3 renderer3 fahrplan $ \event -> [ New.cellText := show $ eRoom event ]
	New.cellLayoutSetAttributes col4 renderer4 fahrplan $ \event -> [ New.cellText := show $ eTime event ]
	New.cellLayoutSetAttributes col5 renderer5 fahrplan $ \event -> [ New.cellText := show $ eRunTime event ]
	New.treeViewAppendColumn tv col1
	New.treeViewAppendColumn tv col2
	New.treeViewAppendColumn tv col3
	New.treeViewAppendColumn tv col4
	New.treeViewAppendColumn tv col5

	set tv [ New.treeViewModel := fahrplan , New.treeViewHeadersVisible := True ]

	return fahrplan


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

	tv <- xmlGetWidget xml New.castToTreeView "treeview1"
	b_quit <- xmlGetWidget xml castToButton "quit"
	b_refresh <- xmlGetWidget xml castToButton "refresh"

	-- based on /usr/share/doc/gtk2hs-doc/examples/treeList/ListDemo.hs
	fahrplan <- setup_liststore tv

	onClicked b_quit $ do widgetDestroy window
	
	onClicked b_refresh $ do
		hPrint h ShowFahrplan
		res <- read `fmap` hGetLine h
		New.listStoreClear fahrplan 
		mapM_ (New.listStoreAppend fahrplan) res

	onDestroy window $ do 
		writable <- hIsWritable h
		if writable then
			hPrint h Quit
		  else  return ()
		mainQuit
	mainGUI

login h conf = do
	hPutStrLn h "guest"
	hPutStrLn h "guest"
