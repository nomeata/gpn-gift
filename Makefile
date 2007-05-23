all:
	make -C anounce
	make -C server

test: all
	echo "Done testing gift..."
	
