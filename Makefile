# Makefile

libqsigex.so:
	cd ./src; $(MAKE)
	cp ./src/libqsigex.so ./lib/

htmldoc: force
	root -b -n -q -l src/htmlgen.C
clean:
	cd ./src; $(MAKE) clean

force: ;
