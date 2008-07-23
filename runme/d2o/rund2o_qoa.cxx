#include <typeinfo>

// Check:
// http://www.physics.carleton.ca/research/sno/anal/software/qsigex_run.html
//

#include "rund2o.h"

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

  QDisTH cce("TH1F","signal_pdf.root","h1001");
  QDisTH ccr("TH1F","signal_pdf.root","h1002");
  QDisTH ccc("TH1F","signal_pdf.root","h1003");

  QDisTH ese("TH1F","signal_pdf.root","h2001");
  QDisTH esr("TH1F","signal_pdf.root","h2002");
  QDisTH esc("TH1F","signal_pdf.root","h2003");

  QDisTH nce("TH1F","signal_pdf.root","h3001");
  QDisTH ncr("TH1F","signal_pdf.root","h3002");
  QDisTH ncc("TH1F","signal_pdf.root","h3003");

  QDisTH bke("TH1F","bck_pdf.root","h4001");
  QDisTH bkr("TH1F","bck_pdf.root","h4002");
  QDisTH bkc("TH1F","bck_pdf.root","h4003");

  cce.Normalize();
  ccr.Normalize();
  ccc.Normalize();

  ese.Normalize();
  esr.Normalize();
  esc.Normalize();

  nce.Normalize();
  ncr.Normalize();
  ncc.Normalize();

  bke.Normalize();
  bkr.Normalize();
  bkc.Normalize();

  QArrayProcessor dataproc("Data Processor","Data Processor");
  QOversizeArray::SetMemConstraints(200*1024*1024,160*1024*1024,170*1024*1024,1000*1024*1024);
  QProcBranchHandler::SaveOutputs(kFALSE);
  QProcQOAHandler::SaveOutputs(kFALSE);

  dataproc.AddParam("ncc",2000);
  dataproc.AddParam("nes",200);
  dataproc.AddParam("nnc",500);
  dataproc.AddParam("nbk",124.2);

  dataproc.AddProc("Selector","Selector",Selector,NULL,kTRUE);
  dataproc.GetProc("Selector").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("Selector").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("Selector").AddIVar("cthsun","tree://d2o_data.root:Tree");


  dataproc.AddPSProc("CCE PDF Eval","CCE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("CCE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("CCE PDF Eval").AddIObj(&cce);
  dataproc.GetProc("CCE PDF Eval").AddOVar("f_e","qoa://ccpdf_f_e.qoa");

  dataproc.AddPSProc("CCR PDF Eval","CCR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("CCR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("CCR PDF Eval").AddIObj(&ccr);
  dataproc.GetProc("CCR PDF Eval").AddOVar("f_r","qoa://ccpdf_f_r.qoa");

  dataproc.AddPSProc("CCC PDF Eval","CCC PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("CCC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("CCC PDF Eval").AddIObj(&ccc);
  dataproc.GetProc("CCC PDF Eval").AddOVar("f_c","qoa://ccpdf_f_c.qoa");


  dataproc.AddPSProc("ESE PDF Eval","ESE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("ESE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("ESE PDF Eval").AddIObj(&ese);
  dataproc.GetProc("ESE PDF Eval").AddOVar("f_e","qoa://espdf_f_e.qoa");

  dataproc.AddPSProc("ESR PDF Eval","ESR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("ESR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("ESR PDF Eval").AddIObj(&esr);
  dataproc.GetProc("ESR PDF Eval").AddOVar("f_r","qoa://espdf_f_r.qoa");

  dataproc.AddPSProc("ESC PDF Eval","ESE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("ESC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("ESC PDF Eval").AddIObj(&esc);
  dataproc.GetProc("ESC PDF Eval").AddOVar("f_c","qoa://espdf_f_c.qoa");


  dataproc.AddPSProc("NCE PDF Eval","NCE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("NCE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("NCE PDF Eval").AddIObj(&nce);
  dataproc.GetProc("NCE PDF Eval").AddOVar("f_e","qoa://ncpdf_f_e.qoa");

  dataproc.AddPSProc("NCR PDF Eval","NCR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("NCR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("NCR PDF Eval").AddIObj(&ncr);
  dataproc.GetProc("NCR PDF Eval").AddOVar("f_r","qoa://ncpdf_f_r.qoa");

  dataproc.AddPSProc("NCC PDF Eval","NCC PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("NCC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("NCC PDF Eval").AddIObj(&ncc);
  dataproc.GetProc("NCC PDF Eval").AddOVar("f_c","qoa://ncpdf_f_c.qoa");


  dataproc.AddPSProc("BKE PDF Eval","BKE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("BKE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("BKE PDF Eval").AddIObj(&bke);
  dataproc.GetProc("BKE PDF Eval").AddOVar("f_e","qoa://bkpdf_f_e.qoa");

  dataproc.AddPSProc("BKR PDF Eval","BKR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("BKR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("BKR PDF Eval").AddIObj(&bkr);
  dataproc.GetProc("BKR PDF Eval").AddOVar("f_r","qoa://bkpdf_f_r.qoa");

  dataproc.AddPSProc("BKC PDF Eval","BKC PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("BKC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("BKC PDF Eval").AddIObj(&bkc);
  dataproc.GetProc("BKC PDF Eval").AddOVar("f_c","qoa://bkpdf_f_c.qoa");


  dataproc.AddPSProc("CC JP","CC JP",Multiply);
  dataproc.GetProc("CC JP").AddIVar("f_e","qoa://ccpdf_f_e.qoa");
  dataproc.GetProc("CC JP").AddIVar("f_r","qoa://ccpdf_f_r.qoa");
  dataproc.GetProc("CC JP").AddIVar("f_c","qoa://ccpdf_f_c.qoa");
  dataproc.GetProc("CC JP").AddOVar("cc","qoa://jointpdf_cc.qoa");


  dataproc.AddPSProc("ES JP","ES JP",Multiply);
  dataproc.GetProc("ES JP").AddIVar("f_e","qoa://espdf_f_e.qoa");
  dataproc.GetProc("ES JP").AddIVar("f_r","qoa://espdf_f_r.qoa");
  dataproc.GetProc("ES JP").AddIVar("f_c","qoa://espdf_f_c.qoa");
  dataproc.GetProc("ES JP").AddOVar("es","qoa://jointpdf_es.qoa");


  dataproc.AddPSProc("NC JP","NC JP",Multiply);
  dataproc.GetProc("NC JP").AddIVar("f_e","qoa://ncpdf_f_e.qoa");
  dataproc.GetProc("NC JP").AddIVar("f_r","qoa://ncpdf_f_r.qoa");
  dataproc.GetProc("NC JP").AddIVar("f_c","qoa://ncpdf_f_c.qoa");
  dataproc.GetProc("NC JP").AddOVar("nc","qoa://jointpdf_nc.qoa");


  dataproc.AddPSProc("BK JP","BK JP",Multiply);
  dataproc.GetProc("BK JP").AddIVar("f_e","qoa://bkpdf_f_e.qoa");
  dataproc.GetProc("BK JP").AddIVar("f_r","qoa://bkpdf_f_r.qoa");
  dataproc.GetProc("BK JP").AddIVar("f_c","qoa://bkpdf_f_c.qoa");
  dataproc.GetProc("BK JP").AddOVar("bk","qoa://jointpdf_bk.qoa");


  QProcDouble llsum;
  dataproc.AddPSProc("LLSum","LLSum",LLSum);
  dataproc.GetProc("LLSum").AddParam("ncc");
  dataproc.GetProc("LLSum").AddParam("nes");
  dataproc.GetProc("LLSum").AddParam("nnc");
  dataproc.GetProc("LLSum").AddParam("nbk");
  dataproc.GetProc("LLSum").AddIVar("cc","qoa://jointpdf_cc.qoa");
  dataproc.GetProc("LLSum").AddIVar("es","qoa://jointpdf_es.qoa");
  dataproc.GetProc("LLSum").AddIVar("nc","qoa://jointpdf_nc.qoa");
  dataproc.GetProc("LLSum").AddIVar("bk","qoa://jointpdf_bk.qoa");
  dataproc.GetProc("LLSum").AddOObj(&llsum);

  dataproc.Analyze();
  //dataproc.InitProcess();
  //dataproc.PrintAnalysisResults();

  QProcList plist;
  plist.AddQProc(&dataproc);
  plist.Analyze();
  plist.InitProcess();

  QSigExFitMinuit fitter;
  fitter.SetProcessor(&plist);
  fitter.SetProcOutput(&llsum);

  fitter.Param("ncc").Setup(2000,0.01,0,10000);
  fitter.Param("nes").Setup(200,0.01,0,1000);
  fitter.Param("nnc").Setup(500,0.01,0,1000);
  fitter.Param("nbk").Setup(124.2,0.00,124.2,124.2,kTRUE);

  fitter.SetFCN(ELLFunction);

  fitter.SetVerbose(0);
  fitter.InitFit();
  fitter.Fit();
  fitter.PrintParams();
  fitter.GetCovMatrix().Print();

  plist.TerminateProcess();

  f1.Write();
  f1.Close();
}

Bool_t PDFEval(QProcArgs &args)
{
  switch(args.GetNIVars()) {
    case 1:
      args.OVar(0)=((QDisTH*)args.IObj(0))->ProbDensity(args.IVar(0));
      break;
    case 2:
      args.OVar(0)=((QDisTH*)args.IObj(0))->ProbDensity(args.IVar(0),args.IVar(1));
      break;
    case 3:
      args.OVar(0)=((QDisTH*)args.IObj(0))->ProbDensity(args.IVar(0),args.IVar(1),args.IVar(2));
      break;
    default:
      fprintf(stderr,"FillHist: Error: Number of input variables (%i) is invalid\n",args.GetNIVars());
  }
  return kTRUE;
}

Bool_t Selector(QProcArgs &args)
{
  if(args.IVar(0)<0 || args.IVar(0)>42) return kFALSE;
  if(args.IVar(1)<0 || args.IVar(1)>26) return kFALSE;
  if(args.IVar(2)<0 || args.IVar(2)>40) return kFALSE;
  return kTRUE;
}

Bool_t Multiply(QProcArgs &args)
{
  static Int_t i;
  args.OVar(0)=args.IVar(0);

  for(i=1; i<args.GetNIVars(); i++) {
    args.OVar(0)*=args.IVar(i);
  }
  
  return kTRUE;
}

Bool_t LLSum(QProcArgs &args)
{
  static Int_t i;
  static Double_t P;
  P=0;

  for(i=0; i<args.GetNParams(); i++) {
    P+=args.Param(i)*args.IVar(i);
  }

  if(P>0) (*(QProcDouble*)args.OObj(0))+=log(P);

  return kTRUE;
}

void ELLFunction(Int_t& npar, Double_t*, Double_t &f, Double_t *par, Int_t)
{
  static Int_t i;
  QSigExFit::SetParams(par);
  QSigExFit::GetCurInstance().ExecProc();
  f=0;
  for(i=0; i<npar; i++) f+=par[i];
  f=2*(f-QSigExFit::GetCurInstance().GetProcOutput());
}
