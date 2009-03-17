#include "QProcQOA.h"

ClassImp(QProcQOA)

QProcQOA::QProcQOA(const char *filename, const char* adesc, QOversizeArray::omode openmode, const UInt_t &qoabuffersize, const Int_t &npcbuffers, const UInt_t &allocblocksize): QProcArray()
{
  TString sbuf;
  Int_t type=GetNameTypeID(adesc,&sbuf);
  UInt_t ui;

  if(type==-1 && openmode==QOversizeArray::kRecreate) type=kDouble;

  if(type!=-1) {
    sbuf=sbuf+"/"+GetTypeName(type);
    fBTypeID=type;
    ui=GetTypeSize(type);
    fArray=new QOversizeArray(filename,sbuf,openmode,ui,qoabuffersize/ui,npcbuffers,allocblocksize/ui);

  } else {
    fArray=new QOversizeArray(filename,sbuf,openmode,0,0,npcbuffers,0);
    fBTypeID=GetTypeID(fArray->GetObjTypeName());
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
