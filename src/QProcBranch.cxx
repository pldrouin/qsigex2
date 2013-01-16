// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QProcBranch.h"

ClassImp(QProcBranch)

void QProcBranch::ClearBuffer()
{
  if(fOwnsBuffer) {
    free(fBuffer);
    fBuffer=NULL;
    SetAddress(NULL);
  }
}

void QProcBranch::SetBuffer(void *buffer)
{
  ClearBuffer();

  TLeaf *lbuf;

  //Get a pointer to the leaf named according to the branch name
  if(!(lbuf=GetLeaf(GetName()))) {
    fprintf(stderr,"QProcBranch::SetBuffer(): Error: There is no leaf '%s' contained in branch '%s' from tree '%s:%s'\n",GetName(),GetName(),GetTree()->GetDirectory()->GetPath(),GetTree()->GetName());
    throw 1;
  }

  fBTypeID=GetTypeID(lbuf->GetTypeName());

  //If there is no buffer assigned to the branch or if a buffer pointer is provided
  if(!GetAddress() || buffer) {

    if(buffer) {
      fBuffer=buffer;
      SetAddress(buffer);
      fOwnsBuffer=kFALSE;

    } else {
      //Create a new buffer
      fBuffer=malloc(GetTypeSize(fBTypeID));
      SetAddress(fBuffer);
      fOwnsBuffer=kTRUE;
    }

    //Else if there is already a buffer assigned to the branch and no buffer pointer is provided
  } else {
    //Get the buffer address from the branch
    fBuffer=GetAddress();
    fOwnsBuffer=kFALSE;
  }
}

void QProcBranch::Streamer(TBuffer &R__b)
{
  // Stream an object of class QProcBranch.

  UInt_t R__s, R__c;
  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    TBranch::Streamer(R__b);
    QProcArray::Streamer(R__b);
    R__b.CheckByteCount(R__s, R__c, QProcBranch::IsA());
    SetBuffer();
  } else {
    R__c = R__b.WriteVersion(QProcBranch::IsA(), kTRUE);
    TBranch::Streamer(R__b);
    QProcArray::Streamer(R__b);
    R__b.SetByteCount(R__c, kTRUE);
  }
}

void QProcBranch::UnloadArray()
{
  QProcBranchHandler::UnloadBranch(this);
}
