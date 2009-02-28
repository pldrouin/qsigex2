QSLIB		=	libqsigex2.so
ALLINCS		=	$(addprefix include/,$(notdir $(wildcard src/*.h)))

QSSAFE		=	1

ifneq (${QSSAFE},"")
CXXFLAGS	+=	-DQSSAFE
endif

all: force $(ALLINCS)

force: 
	cd ./src; $(MAKE) $(QSLIB)
	cp ./src/$(QSLIB) ./lib/

htmldoc: force
	cd ./src; root -b -n -q -l htmlgen.C; mv htmldoc ../
clean:
	cd ./src; $(MAKE) clean

clear:
	cd ./src; $(MAKE) clear
	rm -f lib/* include/*

$(ALLINCS): include/%: src/%
	cp $^ $@
