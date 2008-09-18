# Makefile

libqsigex.so:
	cd ./src; $(MAKE)
	cp ./src/libqsigex2.so ./lib/

htmldoc: force
	cd ./src; root -b -n -q -l htmlgen.C
clean:
	cd ./src; $(MAKE) clean

force: ;
