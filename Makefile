ALLINCS		=	$(addprefix include/,$(notdir $(wildcard src/*.h)))

ifeq (${QSFAST},1)
CXXFLAGS	+=	-DQSFAST
endif

ifeq (${CXX},icc)
AR	=	xiar cru
else
AR	=	ar rcs
endif

all: libso $(ALLINCS)

liba: 
	cd ./src; $(MAKE) liba
	cp ./src/libqsigex*.a ./lib/

libso: 
	cd ./src; $(MAKE) libs
	cp ./src/libqsigex*.so ./lib/

htmldoc: force
	cd ./src; root -b -n -q -l htmlgen.C; mv htmldoc ../
clean:
	cd ./src; $(MAKE) clean

clear:
	cd ./src; $(MAKE) clear
	rm -f lib/* include/*

$(ALLINCS): include/%: src/%
	cp $^ $@
