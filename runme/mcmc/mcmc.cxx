#include <typeinfo>

#include "mcmc.h"

int main(int nargs, char* args[])
{
  if(nargs>2){
    cout <<
      "Error: Wrong arguments format\nShould be: runme [rootfile]\nwhere rootfile is the optional\nargument that specifies the file that will be created to put the results\n(default is results.root)\n";
    exit(1);
  }
  const Char_t* outfile;
  if(nargs==1){
    outfile="results.root";
  }else{
    outfile=args[1];
  }

  TFile f1(outfile,"RECREATE");

  QDisTH *mcth0=new QDisTH("mcth0","mcth0",100,-5.,5.);
  mcth0->GetTH()->FillRandom("gaus",1000000);
  mcth0->SetNormFlags(QDis::kEventsFilled);
  mcth0->Normalize();
  f1.Add(mcth0);

  QDisTH *mcth1=new QDisTH("mcth1","mcth1",100,-5.,5.);
  for(Int_t i=1; i<=100; i++) mcth1->GetTH()->SetBinContent(i,1);
  mcth1->SetNormFlags(QDis::kEventsFilled);
  mcth1->Normalize();
  f1.Add(mcth1);

  QDisTH *dth=new QDisTH("dth","dth",100,-5.,5.);
  dth->GetTH()->FillRandom(mcth0->GetTH(),100);
  TH1D *tmp=new TH1D("tmp","tmp",100,-5.,5.);
  tmp->FillRandom(mcth1->GetTH(),50);
  dth->GetTH()->Add(tmp);
  delete tmp;
  f1.Add(dth);
  QProcDouble fcnval;

  QProcObjProcessor qpopbuf("","");
  qpopbuf.AddProc("pdf","pdf",pdf);
  qpopbuf.GetProc("pdf").AddParam("ngaus");
  qpopbuf.GetProc("pdf").AddParam("nflat");
  qpopbuf.GetProc("pdf").AddIObj(dth);
  qpopbuf.GetProc("pdf").AddIObj(mcth0);
  qpopbuf.GetProc("pdf").AddIObj(mcth1);
  qpopbuf.GetProc("pdf").AddOObj(&fcnval);

  qpopbuf.Analyze();
  qpopbuf.InitProcess();

  QSigExFitMinuit fitter;
  fitter.SetProcessor(&qpopbuf);
  fitter.AddProcOutput(&fcnval);
  fitter.Param("ngaus").Setup(1,3.0);
  fitter.Param("nflat").Setup(2,3.0);
  fitter.SetFCN(ELLFunction);
  fitter.SetFCNError(0.5);

  fitter.SetVerbose(0);
  fitter.InitFit();
  fitter.Fit();
  fitter.PrintParams();

  QSigExFitMCMC mcmc;
  mcmc.SetProcessor(&qpopbuf);
  mcmc.AddProcOutput(&fcnval);

  mcmc.AddParOutput("ngaus","ngaus","tree://params.root:params");
  mcmc.AddParOutput("nflat","nflat","tree://params.root:params");

  mcmc.Param("ngaus").Setup(1,3.0);
  mcmc.Param("nflat").Setup(2,3.0);

  //fitter.SetFCN(PDFFunction);
  //fitter.SetFCNError(0.5);

  mcmc.SetVerbose(0);
  mcmc.InitFit();
  mcmc.SetNBurnInIterations(100000);
  mcmc.Fit();

  f1.Write();
  f1.Close();
}

Bool_t pdf(QProcArgs &args)
{
  static Int_t i;
  static TH1 *dth;
  static TH1 *mcth0;
  static TH1 *mcth1;
  static Double_t P;

  dth=((QDisTH*)args.IObj(0))->GetTH();
  mcth0=((QDisTH*)args.IObj(1))->GetTH();
  mcth1=((QDisTH*)args.IObj(2))->GetTH();
  P=-args.Param(0)-args.Param(1);

  for(i=1; i<=dth->GetXaxis()->GetNbins(); i++) {

    if(dth->GetBinContent(i) && (mcth0->GetBinContent(i) || mcth1->GetBinContent(i))) {

      if(args.Param(0)+args.Param(1)>0) {
	P+=dth->GetBinContent(i)*log(args.Param(0)*mcth0->GetBinContent(i)+args.Param(1)*mcth1->GetBinContent(i));

      } else {
	P+=dth->GetBinContent(i)*(-1e99);
      }
    }
  }
  *((QProcDouble*)args.OObj(0))=TMath::Exp(P);

  return kTRUE;
}

void PDFFunction(Int_t&, Double_t*, Double_t &f, Double_t *, Int_t)
{
  static Int_t i;
  static Double_t const* const* pars;
  pars=QSigExFit::GetCurInstance().GetProcessor()->GetParams();
  QSigExFit::GetCurInstance().ExecProc();
  f=(QProcDouble&)QSigExFit::GetCurInstance().GetProcOutput();
}

void ELLFunction(Int_t&, Double_t*, Double_t &f, Double_t *, Int_t)
{
  static Int_t i;
  static Double_t const* const* pars;
  pars=QSigExFit::GetCurInstance().GetProcessor()->GetParams();
  QSigExFit::GetCurInstance().ExecProc();
  f=-log((QProcDouble&)QSigExFit::GetCurInstance().GetProcOutput());
}
