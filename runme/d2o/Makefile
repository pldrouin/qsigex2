# Makefile
#
# Compile QSigEx program to fit the d2o dataset
# Phys. Rev. Lett. volume 89, No. 1, 011301 (2002)
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


MOBJ		=	rund2o_tree.o rund2o_qoa.o rund2o_buffers.o
EXEC		=	$(MOBJ:.o=)
OBJ		=	$(MOBJ)
DEP		=	$(OBJ:.o=.d)

examples: $(EXEC)

$(EXEC): %: %.o $(POBJ)
	$(CXX) $(CXXFLAGS) $^ $(ROOTLIBS) -L$(QSIGEXDIR)/lib -lqsigex2 -lrt -o $@
#	$(CXX) $(CXXFLAGS) -o $@ $^ $(QSIGEXDIR)/lib/libqsigex2.a -lrt $(ROOTLIBS)
#	$(CXX) $(CXXFLAGS) -o $@ $^ $(QSIGEXDIR)/src/*.os -lrt $(ROOTLIBS)

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



