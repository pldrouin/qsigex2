#include "QProcQOA.h"

ClassImp(QProcQOA)

QProcQOA::QProcQOA(const char *filename, const char* name, QOversizeArray::omode openmode, const UInt_t &objectsize, const UInt_t &nobjectsperbuffer, const Int_t &npcbuffers, const UInt_t &nobjectsallocblock): QProcArray(), QOversizeArray(filename,name,openmode,objectsize,nobjectsperbuffer,npcbuffers,nobjectsallocblock), fBuffer(NULL)
{
  //printf("QProcQOA::QProcQOA()\t%p\n",this);
  fBuffer=new Char_t[objectsize];
  QOversizeArray::SetBuffer(fBuffer);
}

void QProcQOA::Streamer(TBuffer &){}

void QProcQOA::UnloadArray()
{
  QProcQOAHandler::UnloadQOA(this);
}
