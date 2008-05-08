#include "TRandom2.h"
#include "QTTreeProcessor.h"
#include "QDisTH.h"

TRandom2 rnd;

int main();
Bool_t GIShift(Double_t** inputs, Double_t** outputs, Double_t** params, const Int_t* ns);
Bool_t GCShift(Double_t** inputs, Double_t** outputs, Double_t** params, const Int_t* ns);
Bool_t GIScale(Double_t** inputs, Double_t** outputs, Double_t** params, const Int_t* ns);
Bool_t GCScale(Double_t** inputs, Double_t** outputs, Double_t** params, const Int_t* ns);

Bool_t Radius(Double_t** inputs, Double_t** outputs, Double_t** params, const Int_t* ns);

int main()
{
  QTTreeProcessor ttp;

  QDisTH *radius=new QDisTH("radpdf","radpdf",50,0.,500.);
  ((TH1&)*radius).GetXaxis()->SetTitle("Radius [cm]");
  ((TH1&)*radius).GetYaxis()->SetTitle("Number of events");

  ttp.AddParam("VShiftXM");
  ttp.AddParam("VShiftXW",4);
  ttp.AddParam("VShiftYM",1);
  ttp.AddParam("VShiftYW",4);
  ttp.AddParam("VShiftZM",2);
  ttp.AddParam("VShiftZW",4);
  ttp.AddParam("VScaleM",1);
  ttp.AddParam("VScaleW",0.1);
  ttp.AddParam("Rmin",0);
  ttp.AddParam("Rmax",100);

  ttp.AddProc("VShift","VShift",GIShift);
  ttp.AddProc("VScale","VScale",GCScale);
  ttp.AddProc("Radius","Radius",Radius,NULL,kTRUE);
  ttp.AddHist("radpdf","radpdf",radius,kTRUE);

  ttp.GetProc("VShift").AddParam("VShiftXM");
  ttp.GetProc("VShift").AddParam("VShiftXW");
  ttp.GetProc("VShift").AddParam("VShiftYM");
  ttp.GetProc("VShift").AddParam("VShiftYW");
  ttp.GetProc("VShift").AddParam("VShiftZM");
  ttp.GetProc("VShift").AddParam("VShiftZW");
  ttp.GetProc("VScale").AddParam("VScaleM");
  ttp.GetProc("VScale").AddParam("VScaleW");
  ttp.GetProc("Radius").AddParam("Rmin");
  ttp.GetProc("Radius").AddParam("Rmax");

  ttp.GetProc("VShift").AddInput("Xcoord","mc_CALIB_010278.root:t");
  ttp.GetProc("VShift").AddInput("Ycoord","mc_CALIB_010278.root:t");
  ttp.GetProc("VShift").AddInput("Zcoord","mc_CALIB_010278.root:t");
  ttp.GetProc("VShift").AddOutput("Xcoord","results.root:VShift");
  ttp.GetProc("VShift").AddOutput("Ycoord","results.root:VShift");
  ttp.GetProc("VShift").AddOutput("Zcoord","results.root:VShift");

  ttp.GetProc("VScale").AddInput("Xcoord","results.root:VShift");
  ttp.GetProc("VScale").AddInput("Ycoord","results.root:VShift");
  ttp.GetProc("VScale").AddInput("Zcoord","results.root:VShift");
  ttp.GetProc("VScale").AddOutput("Xcoord","results.root:VScale");
  ttp.GetProc("VScale").AddOutput("Ycoord","results.root:VScale");
  ttp.GetProc("VScale").AddOutput("Zcoord","results.root:VScale");

  ttp.GetProc("Radius").AddInput("Xcoord","results.root:VScale");
  ttp.GetProc("Radius").AddInput("Ycoord","results.root:VScale");
  ttp.GetProc("Radius").AddInput("Zcoord","results.root:VScale");
  ttp.GetProc("Radius").AddOutput("Radius","results.root:SmearedMC");

  ttp.GetHist("radpdf").AddInput("Radius","results.root:SmearedMC");

  printf("Analyze\n");
  ttp.Analyze();
  printf("InitProcess\n");
  ttp.PrintAnalysisResults();
  ttp.InitProcess();
  printf("Exec\n");
  ttp.Exec();
  ttp.SetParam(6,1.1);
  ttp.Exec();
  printf("Terminate\n");
  ttp.TerminateProcess();

  TFile pdfsf("pdfs.root","recreate");
  pdfsf.Add(radius);
  pdfsf.Write();
  pdfsf.Close();

  return 0;
}

Bool_t GIShift(Double_t** inputs, Double_t** outputs, Double_t** params, const Int_t* ns)
{

  for(Int_t i=0; i<ns[0]; i++) {
    *(outputs[i])=*(inputs[i])+rnd.Gaus(*(params[2*i]),*(params[2*i+1]));
  }
  return kTRUE;
}

Bool_t GCShift(Double_t** inputs, Double_t** outputs, Double_t** params, const Int_t* ns)
{
  for(Int_t i=0; i<ns[0]; i++) {
    *(outputs[i])=*(inputs[i])+rnd.Gaus(*(params[0]),*(params[1]));
  }
  return kTRUE;
}

Bool_t GIScale(Double_t** inputs, Double_t** outputs, Double_t** params, const Int_t* ns)
{
  for(Int_t i=0; i<ns[0]; i++) {
    *(outputs[i])=*(inputs[i])*rnd.Gaus(*(params[2*i]),*(params[2*i+1]));
  }
  return kTRUE;
}

Bool_t GCScale(Double_t** inputs, Double_t** outputs, Double_t** params, const Int_t* ns)
{
  for(Int_t i=0; i<ns[0]; i++) {
    *(outputs[i])=*(inputs[i])*rnd.Gaus(*(params[0]),*(params[1]));
  }
  return kTRUE;
}

Bool_t Radius(Double_t** inputs, Double_t** outputs, Double_t** params, const Int_t* ns){
  *(outputs[0])=TMath::Sqrt(*(inputs[0])**(inputs[0])+*(inputs[1])**(inputs[1])+*(inputs[2])**(inputs[2]));

  switch(ns[2]) {
    case 2:
      if(*(outputs[0])<*(params[0]) || *(outputs[0])>*(params[1])) return kFALSE;
      break;
    case 1:
      if(*(outputs[0])>*(params[0])) return kFALSE;
      break;
  }
  return kTRUE;
}
