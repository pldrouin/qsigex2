// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>

#include "QFileUtils.h"

#include "debugger.h"

ClassImp(QFileUtils)

QList<TString> QFileUtils::DecodeObjName(TString name)
{
  QList<TString> ret;
  TString objpwd=gDirectory->GetPath();

  Int_t index=name.Index(".root:");

  if(index == kNPOS) {
    name=objpwd+name;
    index=name.Index(".root:");
  }

  if(index+6==name.Length()) return ret;

  if(index != kNPOS) {
    TString sbuf=name(0,index+5);
    sbuf=gSystem->ExpandPathName(sbuf.Data());
    if(!sbuf.Length()) return ret;
  //  if(sbuf[0] != '/') sbuf = (TString)gSystem->pwd() + "/" + sbuf;
  //  sbuf=SimplifyPathName(sbuf);
  //  if(!sbuf.Length()) return ret;

    ret.RedimList(2);
    ret[0]=SimplifyPathName(name(index+6,name.Length()-index-6));

    if(!ret[0].Length()) {
      ret.Clear();
      return ret;
    }
    ret[1]=sbuf;

  } else {
    ret.Add(name);
//    ret.Add(SimplifyPathName(name));

    if(!ret[0].Length()) {
      ret.Clear();
      return ret;
    }
  }

  return ret;
}

QList<TString> QFileUtils::DecodePathName(const TString path)
{
  QList<TString> ret;
  Int_t index;
  Int_t pos=0;

  while((index=path.Index("/",pos)) != kNPOS) {
    if(index != pos) ret.Add(path(pos,index-pos));
    pos=index+1;
  }

  if(pos<=path.Length()-1) ret.Add(path(pos,path.Length()-pos));

  return ret;
}

TString QFileUtils::SimplifyPathName(TString path)
{
  Int_t index;
  Int_t index2;
  Int_t counter;

  path.ReplaceAll("//",2,"/",1);
  path.ReplaceAll("/./",3,"/",1);

  while((index=path.Index("../",0)) != kNPOS) {
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
