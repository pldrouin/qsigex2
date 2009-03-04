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

CXXFLAGS        +=      -fPIC -I$(shell root-config --incdir)
#CXXFLAGS        +=      -I$(shell root-config --incdir)

QSIGEX_QCXX_2	=	QPHN.cxx
QSIGEX_QCXX_1	=	$(filter-out $(QSIGEX_QCXX_2),$(wildcard Q*.cxx))
QSIGEX_QCXX	=	$(QSIGEX_QCXX_1) $(QSIGEX_QCXX_2)
QSIGEX_OBJS_1	=	$(QSIGEX_QCXX_1:.cxx=.o)
QSIGEX_OBJS_2	=	$(QSIGEX_QCXX_2:.cxx=.o)
QSIGEX_OBJS	=	$(QSIGEX_OBJS_1) $(QSIGEX_OBJS_2)
QSIGEX_CLHD_1	=	$(QSIGEX_QCXX_1:.cxx=.h) QList.h QNamedVar.h
QSIGEX_CLHD_2	=	$(QSIGEX_QCXX_2:.cxx=.h) QList.h QNamedVar.h QHN.h QHNF.h QHNDL.h
QSIGEX_CLHD	=	$(QSIGEX_CLHD_1) $(QSIGEX_CLHD_2)

QSIGEX_DICTOBJ_1=	qsigex_1_Dict.o
QSIGEX_DICTOBJ_2=	qsigex_2_Dict.o
QSIGEX_DICTOBJ	=	$(QSIGEX_DICTOBJ_1) $(QSIGEX_DICTOBJ_2)
QSIGEX_LINKDEF_1=	qsigex_1_LinkDef.h
QSIGEX_LINKDEF_2=	qsigex_2_LinkDef.h

QSIGEX_OCXX	=	$(filter-out $(QSIGEX_QCXX) $(QSIGEX_DICTOBJ:.o=.cxx),$(wildcard *.cxx))
QSIGEX_OOBJS_1	=	$(QSIGEX_OCXX_1:.cxx=.o)
QSIGEX_OOBJS_2	=	$(QSIGEX_OCXX_2:.cxx=.o)
QSIGEX_OOBJS	=	$(QSIGEX_OOBJS_1) $(QSIGEX_OOBJS_2)

QSIGEX_OBJSDEP	=	$(QSIGEX_OBJS:.o=.d)
QSIGEX_OOBJSDEP	=	$(QSIGEX_OOBJS:.o=.d)
QSIGEX_DICTOBJDEP=	$(QSIGEX_DICTOBJ:.o=.d)

libs: libqsigex-1.so libqsigex-2.so

#libqsigex2.a: $(QSIGEX_OBJS) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ)
#	rm -rf $@ && ar rcs $@ $^

libqsigex-1.so: $(QSIGEX_OBJS_1) $(QSIGEX_OOBJS_1) $(QSIGEX_DICTOBJ_1)
#	$(CXX) $(CXXFLAGS) -shared -L$(shell root-config --libdir) $(shell root-config --libs) -lMinuit -o $@ $^
	$(CXX) $(CXXFLAGS) -shared -o $@ $^

libqsigex-2.so: $(QSIGEX_OBJS_2) $(QSIGEX_OOBJS_2) $(QSIGEX_DICTOBJ_2)
	$(CXX) $(CXXFLAGS) -shared -o $@ $^

clean:
	rm -f *.d
	rm -f *.o
	rm -f *Dict.o
	rm -f *Dict.h
	rm -f *Dict.cxx
	rm -f libqsigex*.so

clear: clean
	rm -f lib*.{so,a}

$(QSIGEX_OBJSDEP) $(QSIGEX_OOBJSDEP) $(QSIGEX_DICTOBJDEP): %.d: %.cxx %.h
	@echo "Generating dependency file $@"
	@set -e; rm -f $@
	@$(CXX) -M $(CXXFLAGS) $< > $@.tmp
	@sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.tmp > $@
	@rm -f $@.tmp

include $(QSIGEX_OBJSDEP) $(QSIGEX_OOBJSDEP) $(QSIGEX_DICTOBJDEP)

$(QSIGEX_OBJS) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ): %.o: %.cxx
	$(CXX) -c -o $@ $(CXXFLAGS) $<

$(QSIGEX_DICTOBJ_1:.o=.cxx): $(QSIGEX_CLHD_1) $(QSIGEX_LINKDEF_1)
	rm -f $@ $(@:.cxx=.h) && rootcint $@ -c -I./ $^

$(QSIGEX_DICTOBJ_2:.o=.cxx): $(QSIGEX_CLHD_2) $(QSIGEX_LINKDEF_2)
	rm -f $@ $(@:.cxx=.h) && rootcint $@ -c -I./ $^
