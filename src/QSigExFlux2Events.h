// Author: J.Wendland <mailto:juergen@physics.ubc.ca>

#ifndef _QSIGEXFLUX2EVENTS_
#define _QSIGEXFLUX2EVENTS__

#ifndef __CINT__
#include "Api.h"
#endif
#include "TMinuit.h"
#include "TNamed.h"
#include "TList.h"
#include "TCollection.h"
#include "TMatrixDSym.h"
#include "QSigExDirHandler.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QSigExFlux2Events: public QSigExDirHandler {
 public:
  QSigExFlux2Events():QSigExDirHandler(),fF2EDir(NULL),fF2EList(NULL){
    
    PRINTF2(this,"\tQSigExFlux2Events::QSigExFlux2Events()\n")
  }

// constructor
  QSigExFlux2Events(const QSigExFlux2Events& rhs):QSigExDirHandler(*this),fF2EDir(rhs.fF2EDir),fF2EList(NULL),fFlux2EventsCard(rhs.fFlux2EventsCard){
        PRINTF2(this,"\tQSigExFlux2Events::QSigExFlux2Events(const QSigExFlux2Events& rhs)\n")
  }

// constructor
  QSigExFlux2Events(QSigExStruct* dir,const Char_t* cardfilename=NULL):QSigExDirHandler(),fF2EDir(NULL)
    {
      PRINTF6(this,"\tQSigExFlux2Events::QSigExFlux2Events(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
      try{
	SetDir(dir);
	LoadCardFile(cardfilename);
      }catch(int e){
	cout << "Exception handled by QSigExFlux2Events::QSigExFlux2Events\n";
	throw e;
      }
    }

  virtual ~QSigExFlux2Events(){if(fF2EList)delete fF2EList;}

  void LoadCardFile(const Char_t* cardfilename=NULL);

  void CleanDir();
  void ClearCardBuf();

  Int_t Get();

 protected:
  void FormatDir();
  void GetFluxToEventMapping();

 private:
  
  TDirectory* fF2EDir; //Pointer to the "Flux2Events" TDirectory 
  TList *fF2EList; // list of flux2events objects in the directory 
  QList<QList<TString> > fFlux2EventsCard; //Flux2EventsCard configuration parameters in card file format


  ClassDef (QSigExFlux2Events,1) //
};


#include "debugger.h"
#endif





