# Makefile
#
# Compile the bias QSigEx program for ensemble test
#
# LOG
# Version 1.00  13/08/2002 PL
# Version 2.00  23/03/2004 AB (use datan by Brandt libraries)

ROOTLIBS	=	$(shell root-config --libs) -lMinuit -lHtml

ROOTINCLUDE	=	$(shell root-config --incdir)

DATANDIR	=	$(PWD)/local

CXXFLAGS    	=	-O2 -Wall   -fPIC -I$(ROOTINCLUDE) -I$(QSIGEXDIR)/src -I$(DATANDIR)/datan/include

MOBJ		=	bias.o
EXEC		=	$(MOBJ:.o=)
POBJ		=	generator.o mcgen.o datagen.o
OBJ		=	$(MOBJ) $(POBJ)
DEP		=	$(OBJ:.o=.d)

examples: datan $(EXEC)

datan:
	cd local/datan/utils ; \
	make -f makefile.exe clean ; \
	makedlib.sh ; \
	make -f makefile.lib ; \
	cd ../../../

$(EXEC): %: %.o $(POBJ)
	$(CXX) $(CXXFLAGS) $(ROOTLIBS) -L$(DATANDIR)/lib -ldatan -L$(QSIGEXDIR)/lib -lqsigex \
        -L/usr/X11R6/lib -lXm -lXmu -lXt -lX11 -lm -lc -lrt -o $@ $^

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
	cd local/datan/utils ; \
	make -f makefile.exe clean ; \
	cd ../../../
	rm -f *.o
	rm -f *.d
