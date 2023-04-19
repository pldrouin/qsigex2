# Makefile
#
ROOTLIBS	=	$(shell root-config --libs) -lMinuit
ROOTINCLUDE	?=	$(shell root-config --incdir)

CXXFLAGS    	+=	-I$(ROOTINCLUDE) -I$(QSIGEXDIR)/include

ifdef QSFAST
CXXFLAGS        +=      -DQSFAST
endif

ifdef WITH_LIBPROCINFO
CXXFLAGS        +=      -DWITH_LIBPROCINFO
endif


MOBJ		=	mcmc.o
EXEC		=	$(MOBJ:.o=)
OBJ		=	$(MOBJ)
DEP		=	$(OBJ:.o=.d)

examples: $(EXEC)

$(EXEC): %: %.o $(POBJ)
	$(CXX) $(CXXFLAGS) $^ $(ROOTLIBS) -L$(QSIGEXDIR)/lib -lqsigex2 -lrt -o $@

$(DEP): %.d: %.cxx
	@echo "Generating dependency file $@"
	@set -e; rm -f $@
	@$(CXX) -M $(CXXFLAGS) $< > $@.tmp
	@sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.tmp > $@
	@rm -f $@.tmp

include $(DEP)

$(OBJ):	%.o: %.cxx
	$(CXX) -c $(CXXFLAGS) $<

clean:
	rm -f *.o
	rm -f *.d



