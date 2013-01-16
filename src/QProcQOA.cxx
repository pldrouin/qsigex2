// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QProcQOA.h"

ClassImp(QProcQOA)

QProcQOA::QProcQOA(const char *filename, const char* adesc, Int_t openmode, const UInt_t &nobjectsperbuffer, const Int_t &npcbuffers, const UInt_t &nobjectsallocblock): QProcArray()
{
  TString sbuf;
  fBTypeID=GetNameTypeID(adesc,&sbuf);
  UInt_t ui;

  if(fBTypeID==-1 && openmode==QOversizeArray::kRecreate) fBTypeID=kDouble;

  if(fBTypeID!=-1) {
    sbuf=sbuf+"/"+GetTypeName(fBTypeID);
    ui=GetTypeSize(fBTypeID);

    if(openmode==QOversizeArray::kRecreate) fArray=new QOversizeArray(filename,sbuf,openmode,ui,nobjectsperbuffer,npcbuffers,nobjectsallocblock);
    else fArray=new QOversizeArray(filename,sbuf,openmode,ui,0,npcbuffers,nobjectsallocblock);

  } else {
    fArray=new QOversizeArray(filename,sbuf,openmode,0,0,npcbuffers,nobjectsallocblock);

    if(strlen(fArray->GetObjTypeName())) {
      fBTypeID=GetTypeID(fArray->GetObjTypeName());

    } else if(fArray->GetObjSize()!=sizeof(Double_t)) {
      fprintf(stderr,"QProcQOA::QProcQOA: Error: Array saved in file '%s' contains an unknown type which size does not corresponds to a Double_t\n",filename);
      throw 1;

    } else {
      fBTypeID=kDouble;
    }
    ui=GetTypeSize(fBTypeID);
  }
  fOwnsBuffer=kTRUE;
  fBuffer=new Char_t[ui];
  fArray->SetBuffer(fBuffer);
}

void QProcQOA::Streamer(TBuffer &){}

void QProcQOA::UnloadArray()
{
  QProcQOAHandler::UnloadQOA(this);
}
