#include "TRandom2.h"
#include "QArrayProcessor.h"
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
  QArrayProcessor *ttp=new QArrayProcessor;
  QOversizeArray::SetMemConstraints(200*1024*1024,1,0.8,0.85,2);
  QProcBranchHandler::SaveOutputs(kFALSE);
  QProcQOAHandler::SaveOutputs(kFALSE);

  QDisTH *radius=new QDisTH("radpdf","radpdf",50,0.,500.);
  ((TH1&)*radius).GetXaxis()->SetTitle("Radius [cm]");
  ((TH1&)*radius).GetYaxis()->SetTitle("Number of events");

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

  ttp->GetProc("VShift").AddIVar("Xcoord","tree://mc_CALIB_010278.root:t");
  ttp->GetProc("VShift").AddIVar("Ycoord","tree://mc_CALIB_010278.root:t");
  ttp->GetProc("VShift").AddIVar("Zcoord","tree://mc_CALIB_010278.root:t");
  ttp->GetProc("VShift").AddOVar("Xcoord","qoa://VShift_Xcoord.qoa");
  ttp->GetProc("VShift").AddOVar("Ycoord","qoa://VShift_Ycoord.qoa");
  ttp->GetProc("VShift").AddOVar("Zcoord","qoa://VShift_Zcoord.qoa");

  ttp->GetProc("VScale").AddIVar("Xcoord","qoa://VShift_Xcoord.qoa");
  ttp->GetProc("VScale").AddIVar("Ycoord","qoa://VShift_Ycoord.qoa");
  ttp->GetProc("VScale").AddIVar("Zcoord","qoa://VShift_Zcoord.qoa");
  ttp->GetProc("VScale").AddOVar("Xcoord","qoa://VScale_Xcoord.qoa");
  ttp->GetProc("VScale").AddOVar("Ycoord","qoa://VScale_Ycoord.qoa");
  ttp->GetProc("VScale").AddOVar("Zcoord","qoa://VScale_Zcoord.qoa");

  ttp->GetProc("Radius").AddIVar("Xcoord","qoa://VScale_Xcoord.qoa");
  ttp->GetProc("Radius").AddIVar("Ycoord","qoa://VScale_Ycoord.qoa");
  ttp->GetProc("Radius").AddIVar("Zcoord","qoa://VScale_Zcoord.qoa");
  ttp->GetProc("Radius").AddOVar("Radius","qoa://SmearedMC.qoa");

  ttp->GetProc("Pipe").AddIVar("Cstsun","tree://mc_CALIB_010278.root:t");
  ttp->GetProc("Pipe").AddOVar("Cstsun","qoa://results_MC.qoa");

  ttp->GetProc("Pipe2").AddIVar("Radius","qoa://SmearedMC.qoa");
  ttp->GetProc("Pipe2").AddOVar("Radius","qoa://results_Pipe2.qoa");

  ttp->GetProc("radpdf").AddIVar("Radius","qoa://results_Pipe2.qoa");
  ttp->GetProc("radpdf").AddOObj(radius);

  printf("Analyze\n");
  ttp->Analyze();
  ttp->PrintAnalysisResults();
  printf("InitProcess\n");
  ttp->InitProcess();

  ttp->SetParam("VShiftXM");
  ttp->SetParam("VShiftXW",4);
  ttp->SetParam("VShiftYM",1);
  ttp->SetParam("VShiftYW",4);
  ttp->SetParam("VShiftZM",2);
  ttp->SetParam("VShiftZW",4);
  ttp->SetParam("VScaleM",1);
  ttp->SetParam("VScaleW",0.1);
  ttp->SetParam("Rmin",0);
  ttp->SetParam("Rmax",100);
  ttp->SetParam("Dummy",0);
  ttp->SetParam("Dummy2",0);

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
