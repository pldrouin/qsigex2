#include "QProcQOAHandler.h"

ClassImp(QProcQOAHandler)

QList<TString> QProcQOAHandler::fFiles;
QList<TObject*> QProcQOAHandler::fQOAObjs;
QList<Int_t> QProcQOAHandler::fNObjReqQOA;
Bool_t QProcQOAHandler::fSaveOutputs=kTRUE;
Float_t QProcQOAHandler::fSaveCompFrac=0;
UInt_t QProcQOAHandler::fDefNOPerBuffer=131072;
Int_t QProcQOAHandler::fDefNPCBuffers=3;
UInt_t QProcQOAHandler::fDefNOAllocBlock=13108;

QProcArray* QProcQOAHandler::LoadQOA(const char *arraylocation, const char *adesc, Bool_t isoutput, Bool_t incrdeps)
{

  TString pathname=QFileUtils::SimplifyPathName(gSystem->ExpandPathName(arraylocation));
  Int_t i;

  switch (isoutput) {
    case kTRUE:

      //If the array has been opened previously by QProcQOAHandler
      if((i=fFiles.FindFirst(pathname)) != -1) {

	//If the array is opened in read mode
	if(dynamic_cast<QProcQOA*>(fQOAObjs[i])->GetQOA()->GetOpenMode()==QOversizeArray::kRead) {
	  fprintf(stderr,"QProcQOAHandler::LoadQOA: Error: Array '%s' located in file '%s' is not writable\n",adesc,pathname.Data());
	}

	//Increment the number of output branches that require that file
	if(incrdeps) fNObjReqQOA[i]++;
	return (QProcArray*)fQOAObjs[i];

	//Else if the array is not opened
      } else {
	fFiles.Add(pathname);
	if(incrdeps) fNObjReqQOA.Add(1);
	else fNObjReqQOA.Add(0);
	fQOAObjs.Add((TObject*)new QProcQOA(pathname,adesc,QOversizeArray::kRecreate,fDefNOPerBuffer,fDefNPCBuffers,fDefNOAllocBlock));

	return (QProcQOA*)fQOAObjs.GetLast();
      }
      break;

    case kFALSE:


      //If the array has been opened previously by QProcQOAHandler
      if((i=fFiles.FindFirst(pathname)) != -1) {
	if(incrdeps) fNObjReqQOA[i]++;
	return (QProcArray*)fQOAObjs[i];

	//Else if the file is not opened
      } else {
	fFiles.Add(pathname);
	if(incrdeps) fNObjReqQOA.Add(1);
	else fNObjReqQOA.Add(0);
	fQOAObjs.Add((TObject*)new QProcQOA(pathname,adesc,QOversizeArray::kRead,0,fDefNPCBuffers,fDefNOAllocBlock));

	return (QProcQOA*)fQOAObjs.GetLast();
      }
      break;

    default:
      return NULL;
  }
}

void QProcQOAHandler::SetDefArrayParams(const UInt_t &nentriesperdiskbuffer, const Int_t &nprecachedbuffers, const UInt_t &nentriessperallocblock)
{
  fDefNOPerBuffer=nentriesperdiskbuffer;
  fDefNPCBuffers=nprecachedbuffers;
  fDefNOAllocBlock=nentriessperallocblock;
}

void QProcQOAHandler::UnloadQOA(QProcQOA *array)
{
  //printf("void QProcQOAHandler::UnloadQOA(QProcQOA *array<%p>)\n",array);
  Int_t i;

  //If the file is an input file loaded by this class
  if((i=fQOAObjs.FindFirst((TObject*)array)) != -1) {
    //Decrement the number of objects requiring this array
    fNObjReqQOA[i]--;

    //If no object requires this array anymore
    if(!fNObjReqQOA[i]) {

      if(array->GetQOA()->GetOpenMode()!=QOversizeArray::kRead && fSaveOutputs) array->GetQOA()->Save(fSaveCompFrac);
      delete array;
      fFiles.Del(i);
      fQOAObjs.Del(i);
      fNObjReqQOA.Del(i);
      //      printf("Done\n");
    }
  }
  //printf("Status of QProcQOAHandler:\n");
  //printf("Number of files: %i\n",fFiles.Count());
  //printf("Number of QOversizeArray objects: %i\n",fQOAObjs.Count());
}
