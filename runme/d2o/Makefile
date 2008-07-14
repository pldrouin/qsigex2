# Makefile
#
# Compile QSigEx program to fit the d2o dataset
# Phys. Rev. Lett. volume 89, No. 1, 011301 (2002)
#
# LOG
# Version 1.00  13/08/2002 PL
# Version 2.00  01/04/2004 AB

ROOTLIBS	=	$(shell root-config --libs) -lMinuit
ROOTINCLUDE	?=	$(shell root-config --incdir)

CXXFLAGS    	?=	-g -O2 -Wall   -fPIC -I$(ROOTINCLUDE) -I$(QSIGEXDIR)/src

MOBJ		=	rund2o_tree.o rund2o_qoa.o
EXEC		=	$(MOBJ:.o=)
OBJ		=	$(MOBJ)
DEP		=	$(OBJ:.o=.d)

examples: $(EXEC)

$(EXEC): %: %.o $(POBJ)
	$(CXX) $(CXXFLAGS) $(ROOTLIBS)  -L$(QSIGEXDIR)/lib -lqsigex -o $@ $^

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



