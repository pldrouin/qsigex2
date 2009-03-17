#include "QProcBranch.h"

ClassImp(QProcBranch)

const Int_t QProcBranch::cBType=8;

QProcBranch::QProcBranch(TTree* tree, const char* name, void* address, const char* leaflist, Int_t basketsize, Int_t compress): QProcArray(), TBranch(tree,name,address,leaflist,basketsize,compress), fBuffer(NULL), fOwnsBuffer(kFALSE), fOwnsCBuffer(kFALSE), fCBType(0), fCBuffer(NULL)
{
  SetBuffer();
}

QProcBranch::~QProcBranch()
{
  ClearBuffer();
}

void QProcBranch::ClearBuffer()
{
  if(fCBuffer == NULL) {

    if(fOwnsBuffer) {
      delete fBuffer;
      fBuffer=NULL;
      SetAddress(NULL);
    }

  } else {

    if(fOwnsCBuffer) {
      free(fCBuffer);
      fCBuffer=NULL;
      SetAddress(NULL);
    }

    if(fOwnsBuffer) {
      delete fBuffer;
      fBuffer=NULL;
    }
  }
}

Int_t QProcBranch::Fill()
{
  if(fCBuffer) {

    switch(fCBType) {
      case kFloat_t:
	*((Float_t*)fCBuffer)=(Float_t)*fBuffer;
	break;
      case kUInt_t:
	*((UInt_t*)fCBuffer)=(UInt_t)*fBuffer;
	break;
      case kInt_t:
	*((Int_t*)fCBuffer)=(Int_t)*fBuffer;
	break;
      case kUShort_t:
	*((UShort_t*)fCBuffer)=(UShort_t)*fBuffer;
	break;
      case kShort_t:
	*((Short_t*)fCBuffer)=(Short_t)*fBuffer;
	break;
      case kUChar_t:
	*((UChar_t*)fCBuffer)=(UChar_t)*fBuffer;
	break;
      case kChar_t:
	*((Char_t*)fCBuffer)=(Char_t)*fBuffer;
	break;
      case kBool_t:
	*((Bool_t*)fCBuffer)=(Bool_t)*fBuffer;
    }
  }
  return TBranch::Fill();
}

void QProcBranch::LoadEntry(const Long64_t &entry)
{
  TBranch::GetEntry(entry);

  if(fCBuffer) {

    switch(fCBType) {
      case kFloat_t:
	*fBuffer=*((Float_t*)fCBuffer);
	break;
      case kUInt_t:
	*fBuffer=*((UInt_t*)fCBuffer);
	break;
      case kInt_t:
	*fBuffer=*((Int_t*)fCBuffer);
	break;
      case kUShort_t:
	*fBuffer=*((UShort_t*)fCBuffer);
	break;
      case kShort_t:
	*fBuffer=*((Short_t*)fCBuffer);
	break;
      case kUChar_t:
	*fBuffer=*((UChar_t*)fCBuffer);
	break;
      case kChar_t:
	*fBuffer=*((Char_t*)fCBuffer);
	break;
      case kBool_t:
	*fBuffer=*((Bool_t*)fCBuffer);
    }
  }
}

void QProcBranch::SetBuffer(void *buffer)
{
  ClearBuffer();

  TLeaf *lbuf;
  const char *cabuf;

  //Get a pointer to the leaf named according to the branch name
  if(!(lbuf=GetLeaf(GetName()))) {
    fprintf(stderr,"QProcBranch::SetBuffer(): Error: There is no leaf '%s' contained in branch '%s' from tree '%s:%s'\n",GetName(),GetName(),GetTree()->GetDirectory()->GetPath(),GetTree()->GetName());
    throw 1;
  }

  //Get the data type for the current branch from the tree
  cabuf=lbuf->GetTypeName();

  //If the data type is a Double_t, the buffer is assigned directly to the branch
  if(!strcmp(cabuf,"Double_t")) {

    //If there is no buffer assigned to the branch or if a buffer pointer is provided
    if(!GetAddress() || buffer) {

      if(buffer) {
	fBuffer=(Double_t*)buffer;
	SetAddress(buffer);
	fOwnsBuffer=kFALSE;

      } else {
	//Create a new buffer
	fBuffer=new Double_t;
	SetAddress(fBuffer);
	fOwnsBuffer=kTRUE;
      }

    //Else if there is already a buffer assigned to the branch and no buffer pointer is provided
    } else {
      //Get the buffer address from the branch
      fBuffer=(Double_t*)GetAddress();
      fOwnsBuffer=kFALSE;
    }

    //Else if the input branch has a different basic data type, assign a different temporary buffer
  } else {

    if(buffer) {
      fBuffer=(Double_t*)buffer;
      fOwnsBuffer=kFALSE;

    } else {
      //Create a new Double_t buffer for the branch
      fBuffer=new Double_t;
      fOwnsBuffer=kTRUE;
    }

    if(!strcmp(cabuf,"Float_t")) {
      fCBType=kFloat_t;

    } else if(!strcmp(cabuf,"UInt_t")) {
      fCBType=kUInt_t;

    } else if(!strcmp(cabuf,"Int_t")) {
      fCBType=kInt_t;

    } else if(!strcmp(cabuf,"UShort_t")) {
      fCBType=kUShort_t;

    } else if(!strcmp(cabuf,"Short_t")) {
      fCBType=kShort_t;

    } else if(!strcmp(cabuf,"UChar_t")) {
      fCBType=kUChar_t;

    } else if(!strcmp(cabuf,"Char_t")) {
      fCBType=kChar_t;

    } else if(!strcmp(cabuf,"Bool_t")) {
      fCBType=kBool_t;

    } else {
      fprintf(stderr,"QProcBranch::SetBuffer(): Error: The data type '%s' contained in branch '%s' from tree '%s:%s' is not supported\n",cabuf,GetName(),GetTree()->GetDirectory()->GetPath(),GetTree()->GetName());
      throw 1;
    }

    //If there is no buffer associated to that branch
    if(!GetAddress()) {

      //Create a new buffer having the proper size and assign it to the branch
      switch(fCBType) {
	case kFloat_t:
	  fCBuffer=malloc(sizeof(Float_t));
	  break;
	case kUInt_t:
	  fCBuffer=malloc(sizeof(UInt_t));
	  break;
	case kInt_t:
	  fCBuffer=malloc(sizeof(Int_t));
	  break;
	case kUShort_t:
	  fCBuffer=malloc(sizeof(UShort_t));
	  break;
	case kShort_t:
	  fCBuffer=malloc(sizeof(Short_t));
	  break;
	case kUChar_t:
	  fCBuffer=malloc(sizeof(UChar_t));
	  break;
	case kChar_t:
	  fCBuffer=malloc(sizeof(Char_t));
	  break;
	case kBool_t:
	  fCBuffer=malloc(sizeof(Bool_t));
      }
      SetAddress(fCBuffer);
      fOwnsCBuffer=kTRUE;

      //Else if there is already a buffer for this branch
    } else {
      fCBuffer=GetAddress();
      fOwnsCBuffer=kFALSE;
    }
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
