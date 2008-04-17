// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>

#include "QSigExUtils.h"

#include "debugger.h"

ClassImp(QSigExUtils)

QList<TString> QSigExUtils::DecodeObjName(const TString name)
{
  QList<TString> ret;
  TString objpwd=gDirectory->GetPath();

  Int_t index=name.Index(".root:");

  if(index+6==name.Length()) return ret;

  if(index != kNPOS) {
    TString sbuf=name(0,index+5);
    sbuf=gSystem->ExpandPathName(sbuf.Data());
    if(!sbuf.Length()) return ret;
    if(sbuf[0] != '/') sbuf = (TString)gSystem->pwd() + sbuf;
    sbuf=DecodePathName(sbuf);
    if(!sbuf.Length()) return ret;

    if(!sbuf.Length()) {
      ret.Clear();
      return ret;
    }
    ret.RedimList(2);
    ret[0]=DecodePathName(name(index+6,name.Length()-index-6));

    if(!ret[0].Length()) {
      ret.Clear();
      return ret;
    }
    ret[1]=sbuf;

  } else {
    ret.Add(DecodePathName(objpwd+name));

    if(!ret[0].Length()) {
      ret.Clear();
      return ret;
    }
  }

  return ret;
}

TString QSigExUtils::DecodePathName(TString path)
{
  Int_t pos=0;
  Int_t index;
  Int_t index2;
  Int_t counter;

  path.ReplaceAll("//",2,"/",1);
  path.ReplaceAll("/./",3,"/",1);

  while((index=path.Index("../",pos)) != kNPOS) {
    index+=2;
    index2=index-1;
    counter=0;

    while(index2 >=0 && counter < 2) {
      if(path[index2] == '/') counter++;
      index2--;
    }

    if(counter != 2) return "";
    index2+=2;

    path.Replace(index2,index-index2+1,NULL,0);
  }

  return path;
}

#include "debugger.h"
