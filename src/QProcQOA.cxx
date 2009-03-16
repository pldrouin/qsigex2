#include "QProcQOA.h"

ClassImp(QProcQOA)

QProcQOA::QProcQOA(const char *filename, const char* adesc, QOversizeArray::omode openmode, const UInt_t &qoabuffersize, const Int_t &npcbuffers, const UInt_t &allocblocksize): QProcArray()
{
  TString sbuf;
  fBTypeID=GetNameTypeID(adesc,&sbuf);
  UInt_t ui;

  if(fBTypeID==-1 && openmode==QOversizeArray::kRecreate) fBTypeID=kDouble;

  if(fBTypeID==-1) {
    fArray=new QOversizeArray(filename,sbuf,openmode,0,0,npcbuffers,1);
    ui=fArray->GetObjSize();
    fArray->SetNOAllocBlock(allocblocksize/ui);

  } else {
    ui=GetTypeIDSize(fBTypeID);
    fArray=new QOversizeArray(filename,sbuf,openmode,ui,qoabuffersize/ui,npcbuffers,allocblocksize/ui);
  }
  fBuffer=new Char_t[ui];
  fArray->SetBuffer(fBuffer);
}

void QProcQOA::Streamer(TBuffer &){}

void QProcQOA::UnloadArray()
{
  QProcQOAHandler::UnloadQOA(this);
}
