#include "QProcQOA.h"

ClassImp(QProcQOA)

QProcQOA::QProcQOA(const char *filename, const char* adesc, QOversizeArray::omode openmode, const UInt_t &qoabuffersize, const Int_t &npcbuffers, const UInt_t &allocblocksize): QProcArray()
{
  TString sbuf;
  Int_t type=GetNameTypeID(adesc,&sbuf);
  UInt_t ui;

  if(type==-1) type=kDouble;
  sbuf=sbuf+"/"+GetTypeName(type);

  ui=GetTypeIDSize(type);
  fArray=new QOversizeArray(filename,sbuf,openmode,ui,qoabuffersize/ui,npcbuffers,allocblocksize/ui);
  fBuffer=new Char_t[ui];
  fArray->SetBuffer(fBuffer);
}

void QProcQOA::Streamer(TBuffer &){}

void QProcQOA::UnloadArray()
{
  QProcQOAHandler::UnloadQOA(this);
}
