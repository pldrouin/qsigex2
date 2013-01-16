// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QProcQSA.h"

ClassImp(QProcQSA)

QProcQSA::QProcQSA(const char *filename, const char* adesc, const UInt_t &nobjectsperbuffer): QProcArray()
{
  TString sbuf;
  fBTypeID=GetNameTypeID(adesc,&sbuf);
  UInt_t ui;

  if(fBTypeID!=-1) {
    sbuf=sbuf+"/"+GetTypeName(fBTypeID);
    ui=GetTypeSize(fBTypeID);

    fArray=new QSharedArray(filename,sbuf,ui,0);

  } else {
    fArray=new QSharedArray(filename,sbuf,0,0);

    if(strlen(fArray->GetObjTypeName())) {
      fBTypeID=GetTypeID(fArray->GetObjTypeName());

    } else if(fArray->GetObjSize()!=sizeof(Double_t)) {
      fprintf(stderr,"QProcQSA::QProcQSA: Error: Array saved in file '%s' contains an unknown type which size does not corresponds to a Double_t\n",filename);
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

void QProcQSA::Streamer(TBuffer &){}

void QProcQSA::UnloadArray()
{
  QProcQOAHandler::UnloadQOA(this);
}
