#include "QProcQOA.h"

ClassImp(QProcQOA)

QProcQOA::QProcQOA(const char *filename, const char* adesc, QOversizeArray::omode openmode, const UInt_t &qoabuffersize, const Int_t &npcbuffers, const UInt_t &allocblocksize): QProcArray()
{
  TString sbuf;
  fBTypeID=GetNameTypeID(adesc,&sbuf);
  UInt_t ui;

  if(fBTypeID==-1 && openmode==QOversizeArray::kRecreate) fBTypeID=kDouble;

  if(fBTypeID!=-1) {
    sbuf=sbuf+"/"+GetTypeName(fBTypeID);
    ui=GetTypeSize(fBTypeID);
    fArray=new QOversizeArray(filename,sbuf,openmode,ui,qoabuffersize/ui,npcbuffers,allocblocksize/ui);

  } else {
    fArray=new QOversizeArray(filename,sbuf,openmode,0,0,npcbuffers,0);

    if(strlen(fArray->GetObjTypeName())) {
      fBTypeID=GetTypeID(fArray->GetObjTypeName());

    } else if(fArray->GetObjSize()!=sizeof(Double_t)) {
      fprintf(stderr,"QProcQOA::QProcQOA: Error: Array saved in file '%s' contains an unknown type which size does not corresponds to a Double_t\n");
      throw 1;

    } else {
      fBTypeID=kDouble;
    }
    ui=GetTypeSize(fBTypeID);
    fArray->SetNOAllocBlock(allocblocksize/ui);
  }
  fBuffer=new Char_t[ui];
  fArray->SetBuffer(fBuffer);
}

void QProcQOA::Streamer(TBuffer &){}

void QProcQOA::UnloadArray()
{
  QProcQOAHandler::UnloadQOA(this);
}
