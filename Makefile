all: progs doc

progs:
	make -C anounce
	make -C server

test: progs
	echo "Done testing gift..."
	
doc:	doc-stamp

doc-stamp: common/*.hs
	haddock --html --odir ./haddock $+ 
	touch $@
