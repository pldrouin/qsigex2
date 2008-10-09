# Makefile

#libqsigex2.a:
#	cd ./src; $(MAKE)
#	cp ./src/$@ ./lib/

libqsigex2.so:
	cd ./src; $(MAKE)
	cp ./src/$@ ./lib/

htmldoc: force
	cd ./src; root -b -n -q -l htmlgen.C; mv htmldoc ../
clean:
	cd ./src; $(MAKE) clean

force: ;
