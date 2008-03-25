#include "data2root.h"

int main(int nargs,char* args[]){
  if(nargs!=3){
    cout << "Wrong number of arguments\n";
    exit(1);
  }
  QDatReader reader(args[1]);
  QList<QList<TString> > header;

  reader.SetKeyword("header:");
  header=reader.GetMany(0);
  Int_t i,j;
  for(i=0;i<header.Count();i++){
    header[i].Del(0);
  }
  TFile outfile(args[2],"NEW");
  TTree tree("Tree","Tree");
  Float_t* event=new Float_t[header[0].Count()];
  for(i=0;i<header[0].Count();i++){
    tree.Branch(header[0][i],event+i,header[0][i]+"/F");
  }
  QList<TString> eventstr;
  reader.SetKeyword();
  i=header.Count();
  while((eventstr=reader.Get(i)).Count()){
    for(j=0;j<header[0].Count();j++){
      sscanf(eventstr[j],"%f",event+j);
    }
    tree.Fill();
    i++;
  }
  cout << i-header.Count() << " entries have been written\n";
  outfile.Write();
  delete[] event;
}
