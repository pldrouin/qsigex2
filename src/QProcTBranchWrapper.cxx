#include "QProcTBranchWrapper.h"

ClassImp(QProcTBranchWrapper)

void QProcTBranchWrapper::ClearBuffer()
{
  if(fOwnsBuffer) {
    free(fBuffer);
    fBuffer=NULL;
    fBranch->SetAddress(NULL);
  }
}

void QProcTBranchWrapper::SetBuffer(void *buffer)
{
  ClearBuffer();

  TLeaf *lbuf;

  //Get a pointer to the leaf named according to the branch name
  if(!(lbuf=fBranch->GetLeaf(fBranch->GetName()))) {
    fprintf(stderr,"QProcTBranchWrapper::SetBuffer(): Error: There is no leaf '%s' contained in branch '%s' from tree '%s:%s'\n",fBranch->GetName(),fBranch->GetName(),fBranch->GetTree()->GetDirectory()->GetPath(),fBranch->GetTree()->GetName());
    throw 1;
  }

  fBTypeID=GetTypeID(lbuf->GetTypeName());

  //If there is no buffer assigned to the branch or if a buffer pointer is provided
  if(!fBranch->GetAddress() || buffer) {

    if(buffer) {
      fBuffer=buffer;
      fBranch->SetAddress(buffer);
      fOwnsBuffer=kFALSE;

    } else {
      //Create a new buffer
      fBuffer=malloc(GetTypeSize(fBTypeID));
      fBranch->SetAddress(fBuffer);
      fOwnsBuffer=kTRUE;
    }

    //Else if there is already a buffer assigned to the branch and no buffer pointer is provided
  } else {
    //Get the buffer address from the branch
    fBuffer=fBranch->GetAddress();
    fOwnsBuffer=kFALSE;
  }
}

void QProcTBranchWrapper::UnloadArray()
{
  QProcBranchHandler::UnloadBranch(fBranch);
}
