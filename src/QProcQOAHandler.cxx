#include "QProcQOAHandler.h"

ClassImp(QProcQOAHandler)

QList<TString> QProcQOAHandler::fFiles;
QList<TObject*> QProcQOAHandler::fQOAObjs;
QList<Int_t> QProcQOAHandler::fNObjReqQOA;
Bool_t QProcQOAHandler::fSaveOutputs=kTRUE;

QProcArray* QProcQOAHandler::LoadQOA(const char *arraylocation, const char *arrayname, Bool_t isoutput)
{

  TString pathname=QFileUtils::SimplifyPathName(gSystem->ExpandPathName(arraylocation));
  Int_t i;

  switch (isoutput) {
    case kTRUE:

      //If the array has been opened previously by QProcQOAHandler
      if((i=fFiles.FindFirst(pathname)) != -1) {

	//If the array is opened in read mode
	if(dynamic_cast<QOversizeArray*>(fQOAObjs[i])->GetOpenMode()==QOversizeArray::kRead) {
	  fprintf(stderr,"QProcQOAHandler::LoadQOA: Error: Array '%s' located in file '%s' is not writable\n",arrayname,pathname.Data());
	}

	//Increment the number of output branches that require that file
	fNObjReqQOA[i]++;
	return (QProcArray*)fQOAObjs[i];

	//Else if the array is not opened
      } else {
	fFiles.Add(pathname);
	fNObjReqQOA.Add(1);
	fQOAObjs.Add((TObject*)new QProcQOA(pathname,arrayname,QOversizeArray::kRecreate,sizeof(Double_t),131072,3));

	return (QProcQOA*)fQOAObjs.GetLast();
      }
      break;

    case kFALSE:


      //If the array has been opened previously by QProcQOAHandler
      if((i=fFiles.FindFirst(pathname)) != -1) {
	fNObjReqQOA[i]++;
	return (QProcArray*)fQOAObjs[i];

	//Else if the file is not opened
      } else {
	fFiles.Add(pathname);
	fNObjReqQOA.Add(1);
	fQOAObjs.Add((TObject*)new QProcQOA(pathname,arrayname,QOversizeArray::kRead,sizeof(Double_t),131072,3));

	return (QProcQOA*)fQOAObjs.GetLast();
      }
      break;

    default:
      return NULL;
  }
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

      if(dynamic_cast<QOversizeArray*>(array)->GetOpenMode()!=QOversizeArray::kRead && fSaveOutputs) dynamic_cast<QOversizeArray*>(array)->Save();
      dynamic_cast<QOversizeArray*>(array)->CloseFile();
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
