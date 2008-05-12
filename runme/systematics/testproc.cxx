#include "TRandom2.h"
#include "QTTreeProcessor.h"
#include "QDisTH.h"

TRandom2 rnd;

int main();
Bool_t GIShift(QProcArgs &args);
Bool_t GCShift(QProcArgs &args);
Bool_t GIScale(QProcArgs &args);
Bool_t GCScale(QProcArgs &args);

Bool_t Radius(QProcArgs &args);
Bool_t Pipe(QProcArgs &args);

Bool_t FillHist(QProcArgs &args);

int main()
{
  QTTreeProcessor *ttp=new QTTreeProcessor;

  QDisTH *radius=new QDisTH("radpdf","radpdf",50,0.,500.);
  ((TH1&)*radius).GetXaxis()->SetTitle("Radius [cm]");
  ((TH1&)*radius).GetYaxis()->SetTitle("Number of events");

  ttp->AddParam("VShiftXM");
  ttp->AddParam("VShiftXW",4);
  ttp->AddParam("VShiftYM",1);
  ttp->AddParam("VShiftYW",4);
  ttp->AddParam("VShiftZM",2);
  ttp->AddParam("VShiftZW",4);
  ttp->AddParam("VScaleM",1);
  ttp->AddParam("VScaleW",0.1);
  ttp->AddParam("Rmin",0);
  ttp->AddParam("Rmax",100);
  ttp->AddParam("Dummy",0);
  ttp->AddParam("Dummy2",0);

  ttp->AddProc("VShift","VShift",GIShift);
  ttp->AddProc("VScale","VScale",GCScale);
  ttp->AddProc("Pipe","Pipe",Pipe);
  ttp->AddProc("Radius","Radius",Radius,NULL,kTRUE);
  ttp->AddProc("Pipe2","Pipe2",Pipe);
  ttp->AddPSProc("radpdf","radpdf",FillHist);

  ttp->GetProc("VShift").AddParam("VShiftXM");
  ttp->GetProc("VShift").AddParam("VShiftXW");
  ttp->GetProc("VShift").AddParam("VShiftYM");
  ttp->GetProc("VShift").AddParam("VShiftYW");
  ttp->GetProc("VShift").AddParam("VShiftZM");
  ttp->GetProc("VShift").AddParam("VShiftZW");
  ttp->GetProc("VScale").AddParam("VScaleM");
  ttp->GetProc("VScale").AddParam("VScaleW");
  ttp->GetProc("Radius").AddParam("Rmin");
  ttp->GetProc("Radius").AddParam("Rmax");
  ttp->GetProc("Pipe").AddParam("Dummy");
  ttp->GetProc("Pipe2").AddParam("Dummy2");

  ttp->GetProc("VShift").AddIVar("Xcoord","mc_CALIB_010278.root:t");
  ttp->GetProc("VShift").AddIVar("Ycoord","mc_CALIB_010278.root:t");
  ttp->GetProc("VShift").AddIVar("Zcoord","mc_CALIB_010278.root:t");
  ttp->GetProc("VShift").AddOVar("Xcoord","VShift");
  ttp->GetProc("VShift").AddOVar("Ycoord","VShift");
  ttp->GetProc("VShift").AddOVar("Zcoord","VShift");

  ttp->GetProc("VScale").AddIVar("Xcoord","VShift");
  ttp->GetProc("VScale").AddIVar("Ycoord","VShift");
  ttp->GetProc("VScale").AddIVar("Zcoord","VShift");
  ttp->GetProc("VScale").AddOVar("Xcoord","VScale");
  ttp->GetProc("VScale").AddOVar("Ycoord","VScale");
  ttp->GetProc("VScale").AddOVar("Zcoord","VScale");

  ttp->GetProc("Radius").AddIVar("Xcoord","VScale");
  ttp->GetProc("Radius").AddIVar("Ycoord","VScale");
  ttp->GetProc("Radius").AddIVar("Zcoord","VScale");
  ttp->GetProc("Radius").AddOVar("Radius","SmearedMC");

  ttp->GetProc("Pipe").AddIVar("Cstsun","mc_CALIB_010278.root:t");
  ttp->GetProc("Pipe").AddOVar("Cstsun","results.root:MC");

  ttp->GetProc("Pipe2").AddIVar("Radius","SmearedMC");
  ttp->GetProc("Pipe2").AddOVar("Radius","results.root:Pipe2");

  ttp->GetProc("radpdf").AddIVar("Radius","results.root:Pipe2");
  ttp->GetProc("radpdf").AddOObj(radius);

  printf("Analyze\n");
  ttp->Analyze();
  printf("InitProcess\n");
  ttp->PrintAnalysisResults();
  ttp->InitProcess();
  printf("Exec\n");
  ttp->Exec();
  //ttp->SetParam("Dummy",1);
  ttp->SetParam("Dummy2",1);
  ((TH1&)*radius).Reset();
  ttp->Exec();
  printf("Terminate\n");
  ttp->TerminateProcess();

  TFile pdfsf("pdfs.root","recreate");
  pdfsf.Add(radius);
  pdfsf.Add(ttp);
  pdfsf.Write();
  pdfsf.Close();

  return 0;
}

Bool_t GIShift(QProcArgs& args)
{

  for(Int_t i=0; i<args.GetNIVars(); i++) {
    args.OVar(i)=args.IVar(i)+rnd.Gaus(args.Param(2*i),args.Param(2*i+1));
  }
  return kTRUE;
}

Bool_t GCShift(QProcArgs& args)
{
  for(Int_t i=0; i<args.GetNIVars(); i++) {
    args.OVar(i)=args.IVar(i)+rnd.Gaus(args.Param(0),args.Param(1));
  }
  return kTRUE;
}

Bool_t GIScale(QProcArgs& args)
{
  for(Int_t i=0; i<args.GetNIVars(); i++) {
    args.OVar(i)=args.IVar(i)*rnd.Gaus(args.Param(2*i),args.Param(2*i+1));
  }
  return kTRUE;
}

Bool_t GCScale(QProcArgs& args)
{
  for(Int_t i=0; i<args.GetNIVars(); i++) {
    args.OVar(i)=args.IVar(i)*rnd.Gaus(args.Param(0),args.Param(1));
  }
  return kTRUE;
}

Bool_t Radius(QProcArgs& args){
  args.OVar(0)=TMath::Sqrt(args.IVar(0)*args.IVar(0)+args.IVar(1)*args.IVar(1)+args.IVar(2)*args.IVar(2));

  switch(args.GetNParams()) {
    case 2:
      if(args.OVar(0)<args.Param(0) || args.OVar(0)>args.Param(1)) return kFALSE;
      break;
    case 1:
      if(args.OVar(0)>args.Param(0)) return kFALSE;
      break;
  }
  return kTRUE;
}

Bool_t Pipe(QProcArgs& args){
  for(Int_t i=0; i<args.GetNIVars(); i++) {
    args.OVar(i)=args.IVar(i);
  }
  return kTRUE;
}

Bool_t FillHist(QProcArgs &args)
{
  switch(args.GetNIVars()) {
    case 1:
      ((QDisTH*)args.OObj(0))->Fill(args.IVar(0));
      break;
    case 2:
      ((QDisTH*)args.OObj(0))->Fill(args.IVar(0),args.IVar(1));
      break;
    case 3:
      ((QDisTH*)args.OObj(0))->Fill(args.IVar(0),args.IVar(1),args.IVar(2));
      break;
    default:
      fprintf(stderr,"FillHist: Error: Number of input variables (%i) is invalid\n",args.GetNIVars());
  }
  return kTRUE;
}
