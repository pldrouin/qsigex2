#include "QProcQOA.h"

ClassImp(QProcQOA)

QProcQOA::QProcQOA(const char *filename, const char* name, QOversizeArray::omode openmode, const UInt_t &objectsize, const UInt_t &nobjectsperbuffer, const Int_t &npcbuffers): QProcArray(), QOversizeArray(filename,name,openmode,objectsize,nobjectsperbuffer,npcbuffers), fBuffer(NULL)
{
    fBuffer=new Double_t;
    QOversizeArray::SetBuffer(fBuffer);
}

void QProcQOA::Streamer(TBuffer &){}
