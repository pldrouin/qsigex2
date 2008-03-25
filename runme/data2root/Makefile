# Makefile
#
# Compile all runme QSigEx test program
#
# AUTHOR: Pierre-Luc Drouin (pldrouin@poynting.phy.ulaval.ca)
#
# LOG
# Version 1.00  13/08/2002 PL

ROOTLIBS	=	$(shell root-config --libs) -lMinuit -lHtml -L$(QSIGEXDIR)/lib -lqsigex
ROOTINCLUDE	=	$(shell root-config --incdir)
QSIGEXINCLUDE   =       $(QSIGEXDIR)/src

CXX		=	g++
CXXFLAGS    	=	-O2 -Wall   -fPIC -I$(ROOTINCLUDE) -I$(QSIGEXINCLUDE)

OBJS		=	data2root.o

data2root: $(OBJS)
	$(CXX) $(CXXFLAGS) $(ROOTLIBS) -o $@ $^

data2root.o:	data2root.cxx data2root.h
	$(CXX) -c $(CXXFLAGS) $<

clean:
	rm -f *.o
	rm -f *Dict*



