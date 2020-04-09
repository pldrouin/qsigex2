# Makefile
#
# Compile all QSigEX sources taking care of all dependencies.
# This Makefile can recompile QSigEx if any source/header
# file is modified
#
# AUTHOR: Pierre-Luc Drouin (pldrouin@poynting.phy.ulaval.ca)
#
# LOG
# Version 1.00  13/08/2002 PL

INCLUDEDIRS	=	-I$(shell root-config --incdir)
ROOTLIBS	=	$(shell root-config --libs) -lMinuit

ifdef WITH_LIBPROCINFO
INCLUDEDIRS	+=	-I$(LIBPROCINFODIR)
endif

QSIGEX_QCXX_2	=	QAxis.cxx QPHN.cxx QPHNE.cxx
QSIGEX_QCXX_1	=	$(filter-out $(QSIGEX_QCXX_2),$(wildcard Q*.cxx))
QSIGEX_QCXX	=	$(QSIGEX_QCXX_1) $(QSIGEX_QCXX_2)
QSIGEX_OBJS_1	=	$(QSIGEX_QCXX_1:.cxx=.o)
QSIGEX_OBJSS_1	=	$(QSIGEX_QCXX_1:.cxx=.os)
QSIGEX_OBJS_2	=	$(QSIGEX_QCXX_2:.cxx=.o)
QSIGEX_OBJSS_2	=	$(QSIGEX_QCXX_2:.cxx=.os)
QSIGEX_OBJS	=	$(QSIGEX_OBJS_1) $(QSIGEX_OBJS_2)
QSIGEX_OBJSS	=	$(QSIGEX_OBJSS_1) $(QSIGEX_OBJSS_2)
QSIGEX_CLHD_1	=	$(QSIGEX_QCXX_1:.cxx=.h) QList.h QNamedVar.h QProcNamedVar.h
QSIGEX_CLHD_2	=	$(QSIGEX_QCXX_2:.cxx=.h) QList.h QNamedVar.h QProcNamedVar.h QHN.h QHNF.h QHNDL.h
QSIGEX_CLHD	=	$(QSIGEX_CLHD_1) $(QSIGEX_CLHD_2)

QSIGEX_DICTCXX_1=	qsigex_1_Dict.cxx
QSIGEX_DICTOBJ_1=	$(QSIGEX_DICTCXX_1:.cxx=.o)
QSIGEX_DICTOBJS_1=	$(QSIGEX_DICTCXX_1:.cxx=.os)
QSIGEX_DICTCXX_2=	qsigex_2_Dict.cxx
QSIGEX_DICTOBJ_2=	$(QSIGEX_DICTCXX_2:.cxx=.o)
QSIGEX_DICTOBJS_2=	$(QSIGEX_DICTCXX_2:.cxx=.os)
QSIGEX_DICTOBJ	=	$(QSIGEX_DICTOBJ_1) $(QSIGEX_DICTOBJ_2)
QSIGEX_DICTOBJS	=	$(QSIGEX_DICTOBJS_1) $(QSIGEX_DICTOBJS_2)
QSIGEX_1_LINKDEF=	qsigex_1_LinkDef.h
QSIGEX_2_LINKDEF=	qsigex_2_LinkDef.h

QSIGEX_OCXX	=	$(filter-out $(QSIGEX_QCXX) $(QSIGEX_DICTOBJ:.o=.cxx),$(wildcard *.cxx))
QSIGEX_OOBJS	=	$(QSIGEX_OCXX:.cxx=.o)
QSIGEX_OOBJSS	=	$(QSIGEX_OCXX:.cxx=.os)

QSIGEX_OBJSDEP	=	$(QSIGEX_OBJS:.o=.d)
QSIGEX_OOBJSDEP	=	$(QSIGEX_OOBJS:.o=.d)
QSIGEX_DICTOBJDEP=	$(QSIGEX_DICTOBJ:.o=.d)

CXXFLAGSSH	=	$(CXXFLAGS)
CXXFLAGSST	=	$(CXXFLAGS)

ifeq ($(CXX),icpc)
STCOM	=	xiar cru
ifdef PROFGEN
STD     =       $(QSIGEX_OBJSS) $(QSIGEX_OOBJSS) $(QSIGEX_DICTOBJS)
SHD     =       $(QSIGEX_OBJS) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ)
else
ifeq ($(findstring -ipo,$(CXXFLAGS)),-ipo)
STD	=	qsigex_1.ipos qsigex_2.ipos
SHD	=	$(QSIGEX_OBJS) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ)
CXXFLAGSSH	:=	$(filter-out -ipo,$(CXXFLAGSSH))
else
STD     =       $(QSIGEX_OBJSS) $(QSIGEX_OOBJSS) $(QSIGEX_DICTOBJS)
SHD     =       $(QSIGEX_OBJS) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ)
endif
endif
else
STCOM	=	ar rcs
STD	=	$(QSIGEX_OBJSS) $(QSIGEX_OOBJSS) $(QSIGEX_DICTOBJS)
SHD	=	$(QSIGEX_OBJS) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ)
endif

shared: libqsigex2.so
static: libqsigex2.a

libqsigex2.a: $(STD)
	rm -rf $@ && $(STCOM) $@ $^

libqsigex2.so: $(SHD)
	$(CXX) $(CXXFLAGSSH) -shared -o $@ $^ $(ROOTLIBS)

clean:
	rm -f *.d
	rm -f *.o *.os *.ipo *.ipos
	rm -f *Dict.o
	rm -f *Dict.h
	rm -f *Dict.cxx
	rm -f *Dict_rdict.pcm
	rm -f libqsigex*.so libqsigex*.a

clear: clean
	rm -f lib*.{so,a}

$(QSIGEX_OBJSDEP) $(QSIGEX_OOBJSDEP): %.d: %.cxx %.h
	@echo "Generating dependency file $@"
	@set -e; rm -f $@
	@$(CXX) $(CXXFLAGS) -M $(INCLUDEDIRS) $< > $@.tmp
	@sed 's,\($*\)\.o[ :]*,\1.o \1.os $@ : ,g' < $@.tmp > $@
	@rm -f $@.tmp

$(QSIGEX_DICTOBJDEP): %.d: %.cxx
	@echo "Generating dependency file $@"
	@set -e; rm -f $@
	@$(CXX) $(CXXFLAGS) -M $(INCLUDEDIRS) $< > $@.tmp
	@sed 's,\($*\)\.o[ :]*,\1.o \1.os $@ : ,g' < $@.tmp > $@
	@rm -f $@.tmp

include $(QSIGEX_OBJSDEP) $(QSIGEX_OOBJSDEP) $(QSIGEX_DICTOBJDEP)

qsigex_1.ipos: $(QSIGEX_OBJSS_1) $(QSIGEX_OOBJSS) $(QSIGEX_DICTOBJS_1)
	$(CXX) -ipo_c -o $@ $(CXXFLAGSST) $^

qsigex_2.ipos: $(QSIGEX_OBJSS_2) $(QSIGEX_DICTOBJS_2)
	$(CXX) -ipo_c -o $@ $(CXXFLAGSST) $^

$(QSIGEX_OBJSS) $(QSIGEX_OOBJSS): %.os: %.cxx
	$(CXX) -c -o $@ $(CXXFLAGSST) $(INCLUDEDIRS) $<

$(QSIGEX_DICTOBJS): %.os: %.cxx
	$(CXX) -c -o $@ $(CXXFLAGSST) $(INCLUDEDIRS) $<

qsigex_1.ipo: $(QSIGEX_OBJS_1) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ_1)
	$(CXX) -ipo_c -o $@ -fPIC $(CXXFLAGSSH) $^

qsigex_2.ipo: $(QSIGEX_OBJS_2) $(QSIGEX_DICTOBJ_2)
	$(CXX) -ipo_c -o $@ -fPIC $(CXXFLAGSSH) $^

$(QSIGEX_OBJS) $(QSIGEX_OOBJS): %.o: %.cxx
	$(CXX) -c -o $@ -fPIC $(CXXFLAGSSH) $(INCLUDEDIRS) $<

$(QSIGEX_DICTOBJ): %.o: %.cxx
	$(CXX) -c -o $@ -fPIC $(CXXFLAGSSH) $(INCLUDEDIRS) $<

#$(QSIGEX_DICTOBJ_1:.o=.h): $(QSIGEX_DICTOBJ_1:.o=.cxx)
$(QSIGEX_DICTOBJ_1:.o=.cxx): $(QSIGEX_CLHD_1) $(QSIGEX_1_LINKDEF)
	rm -f $@ $(@:.cxx=.h) && rootcint $@ -I./ $(INCLUDEDIRS) $^

#$(QSIGEX_DICTOBJ_2:.o=.h): $(QSIGEX_DICTOBJ_2:.o=.cxx)
$(QSIGEX_DICTOBJ_2:.o=.cxx): $(QSIGEX_CLHD_2) $(QSIGEX_2_LINKDEF)
	rm -f $@ $(@:.cxx=.h) && rootcint $@ -I./ $(INCLUDEDIRS) $^
