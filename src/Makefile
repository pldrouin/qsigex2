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

INCLUDES	=	-I$(shell root-config --incdir)

QSIGEX_QCXX_2	=	QAxis.cxx QPHN.cxx
QSIGEX_QCXX_1	=	$(filter-out $(QSIGEX_QCXX_2),$(wildcard Q*.cxx))
QSIGEX_QCXX	=	$(QSIGEX_QCXX_1) $(QSIGEX_QCXX_2)
QSIGEX_OBJS_1	=	$(QSIGEX_QCXX_1:.cxx=.o)
QSIGEX_OBJSS_1	=	$(QSIGEX_QCXX_1:.cxx=.os)
QSIGEX_OBJS_2	=	$(QSIGEX_QCXX_2:.cxx=.o)
QSIGEX_OBJSS_2	=	$(QSIGEX_QCXX_2:.cxx=.os)
QSIGEX_OBJS	=	$(QSIGEX_OBJS_1) $(QSIGEX_OBJS_2)
QSIGEX_OBJSS	=	$(QSIGEX_OBJSS_1) $(QSIGEX_OBJSS_2)
QSIGEX_CLHD_1	=	$(QSIGEX_QCXX_1:.cxx=.h) QList.h QNamedVar.h
QSIGEX_CLHD_2	=	$(QSIGEX_QCXX_2:.cxx=.h) QList.h QNamedVar.h QHN.h QHNF.h QHNDL.h
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
QSIGEX_OOBJS_1	=	$(QSIGEX_OCXX_1:.cxx=.o)
QSIGEX_OOBJSS_1	=	$(QSIGEX_OCXX_1:.cxx=.os)
QSIGEX_OOBJS_2	=	$(QSIGEX_OCXX_2:.cxx=.o)
QSIGEX_OOBJSS_2	=	$(QSIGEX_OCXX_2:.cxx=.os)
QSIGEX_OOBJS	=	$(QSIGEX_OOBJS_1) $(QSIGEX_OOBJS_2)
QSIGEX_OOBJSS	=	$(QSIGEX_OOBJSS_1) $(QSIGEX_OOBJSS_2)

QSIGEX_OBJSDEP	=	$(QSIGEX_OBJS:.o=.d)
QSIGEX_OOBJSDEP	=	$(QSIGEX_OOBJS:.o=.d)
QSIGEX_DICTOBJDEP=	$(QSIGEX_DICTOBJ:.o=.d)

ifeq ($(CXX),icpc)
STCOM	=	xiar cru
ifdef PROFGEN
STD     =       $(QSIGEX_OBJSS) $(QSIGEX_OOBJSS) $(QSIGEX_DICTOBJS)
SHD     =       $(QSIGEX_OBJS) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ)
else
STD	=	qsigex_1.ipos qsigex_2.ipos
SHD	=	qsigex_1.ipo qsigex_2.ipo
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
	$(CXX) $(CXXFLAGS) -shared -o $@ $^

clean:
	rm -f *.d
	rm -f *.o *.os *.ipo *.ipos
	rm -f *Dict.o
	rm -f *Dict.h
	rm -f *Dict.cxx
	rm -f libqsigex*.so libqsigex*.a

clear: clean
	rm -f lib*.{so,a}

$(QSIGEX_OBJSDEP) $(QSIGEX_OOBJSDEP) $(QSIGEX_DICTOBJDEP): %.d: %.cxx %.h
	@echo "Generating dependency file $@"
	@set -e; rm -f $@
	@$(CXX) -M $(INCLUDES) $< > $@.tmp
	@sed 's,\($*\)\.o[ :]*,\1.o \1.os $@ : ,g' < $@.tmp > $@
	@rm -f $@.tmp

include $(QSIGEX_OBJSDEP) $(QSIGEX_OOBJSDEP) $(QSIGEX_DICTOBJDEP)

qsigex_1.ipos: $(QSIGEX_OBJSS_1) $(QSIGEX_OOBJSS_1) $(QSIGEX_DICTOBJS_1)
	$(CXX) -ipo_c -o $@ $(CXXFLAGS) $^

qsigex_2.ipos: $(QSIGEX_OBJSS_2) $(QSIGEX_OOBJSS_2) $(QSIGEX_DICTOBJS_2)
	$(CXX) -ipo_c -o $@ $(CXXFLAGS) $^

$(QSIGEX_OBJSS) $(QSIGEX_OOBJSS) $(QSIGEX_DICTOBJS): %.os: %.cxx
	$(CXX) -c -o $@ $(CXXFLAGS) $(INCLUDES) $<

qsigex_1.ipo: $(QSIGEX_OBJS_1) $(QSIGEX_OOBJS_1) $(QSIGEX_DICTOBJ_1)
	$(CXX) -ipo_c -o $@ -fPIC $(CXXFLAGS) $^

qsigex_2.ipo: $(QSIGEX_OBJS_2) $(QSIGEX_OOBJS_2) $(QSIGEX_DICTOBJ_2)
	$(CXX) -ipo_c -o $@ -fPIC $(CXXFLAGS) $^

$(QSIGEX_OBJS) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ): %.o: %.cxx
	$(CXX) -c -o $@ -fPIC $(CXXFLAGS) $(INCLUDES) $<

$(QSIGEX_DICTOBJ_1:.o=.h): $(QSIGEX_DICTOBJ_1:.o=.cxx)
$(QSIGEX_DICTOBJ_1:.o=.cxx): $(QSIGEX_CLHD_1) $(QSIGEX_1_LINKDEF)
	rm -f $@ $(@:.cxx=.h) && rootcint $@ -c -I./ $^

$(QSIGEX_DICTOBJ_2:.o=.h): $(QSIGEX_DICTOBJ_2:.o=.cxx)
$(QSIGEX_DICTOBJ_2:.o=.cxx): $(QSIGEX_CLHD_2) $(QSIGEX_2_LINKDEF)
	rm -f $@ $(@:.cxx=.h) && rootcint $@ -c -I./ $^
