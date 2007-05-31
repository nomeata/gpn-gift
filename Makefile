all: progs doc gift.tar.gz

progs:
	make -C anounce
	make -C server
	make -C client

test: progs
	echo "Done testing gift..."
	
doc:	doc-stamp

doc-stamp: common/*.hs
	haddock --html --odir ./haddock --source-module=../%F \
		--title "GIFT Libraries" \
		-i /usr/share/doc/ghc6-doc/html/libraries/base/,/usr/share/doc/ghc6-doc/html/libraries/base/base.haddock \
		$+ 
	touch $@

gift.tar.gz: server/server client/client anounce/anounce */data/*
	tar vczf $@ $+
