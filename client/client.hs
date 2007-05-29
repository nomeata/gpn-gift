import Graphics.UI.Gtk hiding (Event)
import Graphics.UI.Gtk.Glade
import qualified Graphics.UI.Gtk.ModelView as New
import Network
import Control.Monad
import IO 
import Data.Maybe
import Data.List
import Data.Ord
import Control.Exception

import Time
import DatT

empty_event :: Event
empty_event = Event 0 "" Chaos (read "(0,0,0)") (read "(0,0)")

setup_liststore fahrplan tv edit = do
	col1 <- New.treeViewColumnNew
	col2 <- New.treeViewColumnNew
	col3 <- New.treeViewColumnNew
	col4 <- New.treeViewColumnNew
	col5 <- New.treeViewColumnNew
	set col1 [ New.treeViewColumnTitle := "ID"   , New.treeViewColumnMinWidth := 20  ]
	set col2 [ New.treeViewColumnTitle := "Name" , New.treeViewColumnMinWidth := 200 ]
	set col3 [ New.treeViewColumnTitle := "Raum" , New.treeViewColumnMinWidth := 100 ]
	set col4 [ New.treeViewColumnTitle := "Zeit" , New.treeViewColumnMinWidth := 100 ]
	set col5 [ New.treeViewColumnTitle := "Dauer", New.treeViewColumnMinWidth := 100 ]
	renderer1 <- New.cellRendererTextNew
	renderer2 <- New.cellRendererTextNew
	renderer3 <- New.cellRendererTextNew
	--renderer3 <- New.cellRendererComboNew
	renderer4 <- New.cellRendererTextNew
	renderer5 <- New.cellRendererTextNew
	set renderer2 [ New.cellEditable := True ]
	set renderer3 [ New.cellEditable := True ] 
	set renderer3 [ New.cellEditable := True ] --, New.cellComboHasEntry := False ]
	set renderer4 [ New.cellEditable := True ]
	set renderer5 [ New.cellEditable := True ]
	New.onEdited renderer2 $ \[n] text -> do
		event <- New.listStoreGetValue fahrplan n
		let new = event {eName = text}
		when (new /= event) $ edit new
	New.onEdited renderer3 $ \[n] text -> do
		event <- New.listStoreGetValue fahrplan n
		handle (\_ -> return ()) $ do
			let new = event {eRoom = (read text)}
			when (new /= event) $ edit new
	New.onEdited renderer4 $ \[n] text -> do
		event <- New.listStoreGetValue fahrplan n
		handle (\_ -> return ()) $ do
			let new = event {eTime = (read text)}
			when (new /= event) $ edit new
	New.onEdited renderer5 $ \[n] text -> do
		event <- New.listStoreGetValue fahrplan n
		handle (\_ -> return ()) $ do
			let new = event {eRunTime = (read text)}
			when (new /= event) $ edit new
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
	windowResize window 500 300

	tv <- xmlGetWidget xml New.castToTreeView "treeview1"
	b_quit <- xmlGetWidget xml castToButton "quit"
	b_refresh <- xmlGetWidget xml castToButton "refresh"
	b_delete <- xmlGetWidget xml castToButton "delete"

	fahrplan <- New.listStoreNew [empty_event]

	let update_fahrplan = do 
		hPrint h ShowFahrplan
		res <- ( sortBy (comparing eTime) . read ) `fmap` hGetLine h

		New.listStoreClear fahrplan 
		mapM_ (New.listStoreAppend fahrplan) res
		New.listStoreAppend fahrplan empty_event
	
	update_fahrplan

	let send_command cmd = do
		hPrint h cmd 
		reply <- hGetLine h 
		dialog <- messageDialogNew (Just window) [] MessageInfo ButtonsOk reply
		dialogRun dialog
		widgetDestroy dialog
		update_fahrplan
		return ()


	-- based on /usr/share/doc/gtk2hs-doc/examples/treeList/ListDemo.hs
	setup_liststore fahrplan tv $ \event -> do
		if eID event == 0 then send_command (Commit event)
		                  else send_command (Edit event)

	onClicked b_delete $ do
		(path,_) <- New.treeViewGetCursor tv
		case path of
			[] -> return ()
			[n] -> do
				event <- New.listStoreGetValue fahrplan n
				when (eID event /= 0) $ send_command (Delete (eID event))

	onClicked b_quit $ widgetDestroy window
	
	onClicked b_refresh update_fahrplan

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
