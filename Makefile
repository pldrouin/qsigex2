ALLINCS		=	$(addprefix include/,$(notdir $(wildcard src/*.h)))

ifdef QSFAST
CXXFLAGS	+=	-DQSFAST
endif

ifdef PROFGEN
CXXFLAGS	:=	$(filter-out -ipo -ip,$(CXXFLAGS)) -prof-gen
else
ifdef PROFUSE
CXXFLAGS	+=	-prof-use
endif
endif

all: $(ALLINCS)
	cd ./src; $(MAKE) shared static
	cp ./src/libqsigex*.a ./lib/

static: $(ALLINCS)
	cd ./src; $(MAKE) static
	cp ./src/libqsigex*.a ./lib/

shared: $(ALLINCS)
	cd ./src; $(MAKE) shared
	cp ./src/libqsigex*.so ./lib/

htmldoc: shared
	cd ./src; root -b -n -q -l htmlgen.C; mv htmldoc ../
clean:
	cd ./src; $(MAKE) clean

clear:
	cd ./src; $(MAKE) clear
	rm -f lib/* include/*

$(ALLINCS): include/%: src/%
	cp $^ $@
