ALLINCS		=	$(addprefix include/,$(notdir $(wildcard src/*.h)))

ifeq ($(QSFAST),1)
CXXFLAGS	+=	-DQSFAST
endif

ifeq ($(PROFGEN),1)
CXXFLAGS	+=	-prof-gen
else
ifeq ($(PROFUSE),1)
CXXFLAGS	+=	-prof-use
endif
endif

all: shared static

static: $(ALLINCS)
	cd ./src; $(MAKE) static
	cp ./src/libqsigex*.a ./lib/

shared: $(ALLINCS)
	cd ./src; $(MAKE) shared
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
