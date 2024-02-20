ALLINCS_ORIG	=	$(wildcard src/*.h)
ALLINCS		=	$(addprefix include/,$(notdir $(ALLINCS_ORIG)))

ifndef CXXFLAGS
export CXXFLAGS	=	
endif

ifdef QSFAST
CXXFLAGS	+=	-DQSFAST
endif

ifdef WITH_LIBPROCINFO
CXXFLAGS	+=	-DWITH_LIBPROCINFO
endif

ifeq ($(CXX),icpc)
ifdef PROFGEN
CXXFLAGS	:=	$(filter-out -ipo -ip,$(CXXFLAGS)) -prof-gen
else
ifdef PROFUSE
CXXFLAGS	+=	-prof-use
endif
endif

else
ifdef PROFGEN
CXXFLAGS        +=      -fprofile-generate
else
ifdef PROFUSE
CXXFLAGS        +=      -fprofile-use -fprofile-correction
endif
endif

ifdef SANITIZE
CXXFLAGS	+=	-fsanitize=address -fno-sanitize-recover=all -fsanitize=float-divide-by-zero -fsanitize=float-cast-overflow -fno-sanitize=alignment
endif
endif

all: lib $(ALLINCS)
	cd ./src; $(MAKE) shared static
	cp ./src/libqsigex*.a ./src/libqsigex*.so ./src/*_rdict.pcm ./lib/

static: lib $(ALLINCS)
	cd ./src; $(MAKE) static
	cp ./src/libqsigex*.a ./src/*_rdict.pcm ./lib/

shared: lib $(ALLINCS)
	cd ./src; $(MAKE) shared
	cp ./src/libqsigex*.so ./src/*_rdict.pcm ./lib/

lib:
	mkdir -p $@

htmldoc:
	rm -rf htmldoc
	cd ./src; root -b -n -q -l htmlgen.C; mv htmldoc ../
clean:
	cd ./src; $(MAKE) clean

clear:
	cd ./src; $(MAKE) clear
	rm -f lib/* include/*

bindist:
	rm -rf src Makefile

$(ALLINCS): include/% : src/% include
	cp $< $@

include:
	mkdir -p $@
