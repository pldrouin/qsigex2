// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _QPROCQOAHANDLER_
#define _QPROCQOAHANDLER_

#include "QList.h"
#include "QFileUtils.h"
#include "QProcArray.h"
#include "QProcQOA.h"
#include "QProcQSA.h"

class QProcQOA;
class QProcQSA;

class QProcQOAHandler
{
  public:
    QProcQOAHandler(){}
    virtual ~QProcQOAHandler(){}
    static QProcArray* LoadQOA(const char *arraylocation, const char *adesc, Bool_t isoutput, Bool_t incrdeps=kTRUE);
    static void SharedInputs(Bool_t sharedinputs=kTRUE){fSharedInputs=sharedinputs;}
    static void SaveOutputs(Bool_t saveoutputs=kTRUE, const Float_t &savecompfrac=0){fSaveOutputs=saveoutputs; fSaveCompFrac=savecompfrac;}
    static void SetDefArrayParams(const UInt_t &nentriesperdiskbuffer=131072, const Int_t &nprecachedbuffers=1, const UInt_t &nentriesperallocblock=13108);
    friend class QProcQOA;
    friend class QProcQSA;
  protected:
    static QList<TString> fFiles;    //Filenames of QOA objects required by this class
    static QList<void*> fQOAObjs;     //QOA objects required by this class
    static QList<Int_t> fNObjReqQOA;     //Number of objects requiring the QOA objects
    static Bool_t fSharedInputs;         //Use QProcQSA objects for inputs
    static Bool_t fSaveOutputs;          //Save output arrays loaded by this class before closing the files
    static Float_t fSaveCompFrac; //Default fraction of modified QOA buffers to compress when QOversizeArray::Save is called
    static UInt_t fDefNOPerBuffer;   //Default number of objects per disk buffer
    static Int_t fDefNPCBuffers;     //Default number of pre-cached disk buffers
    static UInt_t fDefNOAllocBlock;  //Default number of objects per memory allocation block
  private:
    static void UnloadQOA(QProcArray *array);
    const QProcQOAHandler& operator=(const QProcQOAHandler &){return *this;}
    ClassDef(QProcQOAHandler,1) //Class handling loading/unloading of QOversizeArray instances
};

#endif
