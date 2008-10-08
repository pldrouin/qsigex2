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

CXXFLAGS    	+=	-I$(shell root-config --incdir) -pipe -Wall -W -Woverloaded-virtual -fPIC -DSWAP_BYTES -DUSE_GETTIMEOFDAY -DUSE_PTHREAD -DIGNORE_DISP_ERRORS -DFORTRAN_UNDERSCORE -fno-strict-aliasing

QSIGEX_QCXX	=	$(wildcard Q*.cxx)
QSIGEX_OBJS	=	$(QSIGEX_QCXX:.cxx=.o)
QSIGEX_CLHD     =       $(QSIGEX_QCXX:.cxx=.h) QList.h QNamedVar.h QTHN.h QDisTHN.h

QSIGEX_DICTOBJ	=	qsigex_Dict.o
QSIGEX_LINKDEF	=	qsigex_LinkDef.h

QSIGEX_OCXX	=	$(filter-out $(QSIGEX_QCXX) $(QSIGEX_DICTOBJ:.o=.cxx),$(wildcard *.cxx))
QSIGEX_OOBJS	=	$(QSIGEX_OCXX:.cxx=.o)

QSIGEX_OBJSDEP	=	$(QSIGEX_OBJS:.o=.d)
QSIGEX_OOBJSDEP	=	$(QSIGEX_OOBJS:.o=.d)
QSIGEX_DICTOBJDEP	=	$(QSIGEX_DICTOBJ:.o=.d)

libqsigex2.so: $(QSIGEX_OBJS) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ)
	$(CXX) $(CXXFLAGS) -shared -L$(shell root-config --libdir) -lMinuit -o $@ $^

clean:
	rm -f *.d
	rm -f *.o
	rm -f *Dict.o
	rm -f *Dict.h
	rm -f *Dict.cxx
	rm -f libqsigex2.so

$(QSIGEX_OBJSDEP) $(QSIGEX_OOBJSDEP) $(QSIGEX_DICTOBJDEP): %.d: %.cxx %.h
	@echo "Generating dependency file $@"
	@set -e; rm -f $@
	@$(CXX) -M $(CXXFLAGS) $< > $@.tmp
	@sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.tmp > $@
	@rm -f $@.tmp

include $(QSIGEX_OBJSDEP) $(QSIGEX_OOBJSDEP) $(QSIGEX_DICTOBJDEP)

$(QSIGEX_OBJS) $(QSIGEX_OOBJS) $(QSIGEX_DICTOBJ): %.o: %.cxx
	$(CXX) -c -o $@ $(CXXFLAGS) -fPIC $<

$(QSIGEX_DICTOBJ:.o=.cxx): $(QSIGEX_CLHD) $(QSIGEX_LINKDEF)
	rm -f $@ $(@:.cxx=.h) && rootcint $@ -c -I./ $^
