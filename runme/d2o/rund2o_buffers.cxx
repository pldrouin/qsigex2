#include <typeinfo>

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

  QHN_D* cce=new QHN_D("signal_pdf.root","h1001");
  QHN_D* ccr=new QHN_D("signal_pdf.root","h1002");
  QHN_D* ccc=new QHN_D("signal_pdf.root","h1003");

  QHN_D* ese=new QHN_D("signal_pdf.root","h2001");
  QHN_D* esr=new QHN_D("signal_pdf.root","h2002");
  QHN_D* esc=new QHN_D("signal_pdf.root","h2003");

  QHN_D* nce=new QHN_D("signal_pdf.root","h3001");
  QHN_D* ncr=new QHN_D("signal_pdf.root","h3002");
  QHN_D* ncc=new QHN_D("signal_pdf.root","h3003");

  QHN_D* bke=new QHN_D("bck_pdf.root","h4001");
  QHN_D* bkr=new QHN_D("bck_pdf.root","h4002");
  QHN_D* bkc=new QHN_D("bck_pdf.root","h4003");

  cce->Normalize();
  ccr->Normalize();
  ccc->Normalize();

  ese->Normalize();
  esr->Normalize();
  esc->Normalize();

  nce->Normalize();
  ncr->Normalize();
  ncc->Normalize();

  bke->Normalize();
  bkr->Normalize();
  bkc->Normalize();

  gDirectory->Add(cce);
  gDirectory->Add(ccr);
  gDirectory->Add(ccc);

  gDirectory->Add(ese);
  gDirectory->Add(esr);
  gDirectory->Add(esc);

  gDirectory->Add(nce);
  gDirectory->Add(ncr);
  gDirectory->Add(ncc);

  gDirectory->Add(bke);
  gDirectory->Add(bkr);
  gDirectory->Add(bkc);

  QArrayProcessor dataproc("Data Processor","Data Processor");
  QOversizeArray::SetMemConstraints(200*1024*1024,160*1024*1024,170*1024*1024,1000*1024*1024);
  QProcBranchHandler::SaveOutputs(kFALSE);
  QProcQOAHandler::SaveOutputs(kFALSE);
  //QProcessor::SetDefVerbosity(QProcessor::kShowExec|QProcessor::kShowExec2);

  dataproc.AddProc("Selector","Selector",Selector,NULL,kTRUE);
  dataproc.GetProc("Selector").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("Selector").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("Selector").AddIVar("cthsun","tree://d2o_data.root:Tree");


  dataproc.AddPSProc("CCE PDF Eval","CCE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("CCE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("CCE PDF Eval").AddIObj(cce);
  dataproc.GetProc("CCE PDF Eval").AddOVar("ccpdf_f_e");

  dataproc.AddPSProc("CCR PDF Eval","CCR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("CCR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("CCR PDF Eval").AddIObj(ccr);
  dataproc.GetProc("CCR PDF Eval").AddOVar("ccpdf_f_r");

  dataproc.AddPSProc("CCC PDF Eval","CCC PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("CCC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("CCC PDF Eval").AddIObj(ccc);
  dataproc.GetProc("CCC PDF Eval").AddOVar("ccpdf_f_c");


  dataproc.AddPSProc("ESE PDF Eval","ESE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("ESE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("ESE PDF Eval").AddIObj(ese);
  dataproc.GetProc("ESE PDF Eval").AddOVar("espdf_f_e");

  dataproc.AddPSProc("ESR PDF Eval","ESR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("ESR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("ESR PDF Eval").AddIObj(esr);
  dataproc.GetProc("ESR PDF Eval").AddOVar("espdf_f_r");

  dataproc.AddPSProc("ESC PDF Eval","ESE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("ESC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("ESC PDF Eval").AddIObj(esc);
  dataproc.GetProc("ESC PDF Eval").AddOVar("espdf_f_c");


  dataproc.AddPSProc("NCE PDF Eval","NCE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("NCE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("NCE PDF Eval").AddIObj(nce);
  dataproc.GetProc("NCE PDF Eval").AddOVar("ncpdf_f_e");

  dataproc.AddPSProc("NCR PDF Eval","NCR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("NCR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("NCR PDF Eval").AddIObj(ncr);
  dataproc.GetProc("NCR PDF Eval").AddOVar("ncpdf_f_r");

  dataproc.AddPSProc("NCC PDF Eval","NCC PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("NCC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("NCC PDF Eval").AddIObj(ncc);
  dataproc.GetProc("NCC PDF Eval").AddOVar("ncpdf_f_c");


  dataproc.AddPSProc("BKE PDF Eval","BKE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("BKE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("BKE PDF Eval").AddIObj(bke);
  dataproc.GetProc("BKE PDF Eval").AddOVar("bkpdf_f_e");

  dataproc.AddPSProc("BKR PDF Eval","BKR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("BKR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("BKR PDF Eval").AddIObj(bkr);
  dataproc.GetProc("BKR PDF Eval").AddOVar("bkpdf_f_r");

  dataproc.AddPSProc("BKC PDF Eval","BKC PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("BKC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("BKC PDF Eval").AddIObj(bkc);
  dataproc.GetProc("BKC PDF Eval").AddOVar("bkpdf_f_c");


  dataproc.AddPSProc("CC JP","CC JP",Multiply);
  dataproc.GetProc("CC JP").AddIVar("ccpdf_f_e");
  dataproc.GetProc("CC JP").AddIVar("ccpdf_f_r");
  dataproc.GetProc("CC JP").AddIVar("ccpdf_f_c");
  dataproc.GetProc("CC JP").AddOVar("jointpdf_cc");


  dataproc.AddPSProc("ES JP","ES JP",Multiply);
  dataproc.GetProc("ES JP").AddIVar("espdf_f_e");
  dataproc.GetProc("ES JP").AddIVar("espdf_f_r");
  dataproc.GetProc("ES JP").AddIVar("espdf_f_c");
  dataproc.GetProc("ES JP").AddOVar("jointpdf_es");


  dataproc.AddPSProc("NC JP","NC JP",Multiply);
  dataproc.GetProc("NC JP").AddIVar("ncpdf_f_e");
  dataproc.GetProc("NC JP").AddIVar("ncpdf_f_r");
  dataproc.GetProc("NC JP").AddIVar("ncpdf_f_c");
  dataproc.GetProc("NC JP").AddOVar("jointpdf_nc");


  dataproc.AddPSProc("BK JP","BK JP",Multiply);
  dataproc.GetProc("BK JP").AddIVar("bkpdf_f_e");
  dataproc.GetProc("BK JP").AddIVar("bkpdf_f_r");
  dataproc.GetProc("BK JP").AddIVar("bkpdf_f_c");
  dataproc.GetProc("BK JP").AddOVar("jointpdf_bk");


  QProcDouble llsum;
  dataproc.AddPSProc("LLSum","LLSum",LLSum);
  dataproc.GetProc("LLSum").AddParam("ncc");
  dataproc.GetProc("LLSum").AddParam("nes");
  dataproc.GetProc("LLSum").AddParam("nnc");
  dataproc.GetProc("LLSum").AddParam("nbk");
  dataproc.GetProc("LLSum").AddIVar("jointpdf_cc");
  dataproc.GetProc("LLSum").AddIVar("jointpdf_es");
  dataproc.GetProc("LLSum").AddIVar("jointpdf_nc");
  dataproc.GetProc("LLSum").AddIVar("jointpdf_bk");
  dataproc.GetProc("LLSum").AddOObj(&llsum);

  dataproc.Analyze();
  dataproc.PrintProcesses(0,kTRUE);
  //dataproc.PrintAnalysisResults();
  dataproc.InitProcess();

  QSigExFitMinuit fitter;
  fitter.SetProcessor(&dataproc);
  fitter.AddProcOutput(&llsum);

  fitter.Param("ncc").Setup(2000,0.01,0,10000);
  fitter.Param("nes").Setup(200,0.01,0,1000);
  fitter.Param("nnc").Setup(500,0.01,0,1000);
  fitter.Param("nbk").Setup(124.2,0.00,124.2,124.2,kTRUE);

  fitter.SetFCN(ELLFunction);

  fitter.SetVerbose(0);
  fitter.InitFit();
  //fitter.PrintParams();
  fitter.Fit();
  fitter.PrintParams();
  fitter.GetCovMatrix().Print();
  fitter.GetCorMatrix().Print();

  dataproc.TerminateProcess();

  f1.Add(&fitter);

  f1.Write();
  f1.Close();
}

Bool_t PDFEval(QProcArgs &args)
{
  switch(args.GetNIVars()) {
    case 1:
      args.OVar(0)=((QHN_D*)args.IObj(0))->Eval(args.IVarF(0));
      break;
    case 2:
      args.OVar(0)=((QHN_D*)args.IObj(0))->Eval(args.IVarF(0),args.IVarF(1));
      break;
    case 3:
      args.OVar(0)=((QHN_D*)args.IObj(0))->Eval(args.IVarF(0),args.IVarF(1),args.IVarF(2));
      break;
    default:
      fprintf(stderr,"FillHist: Error: Number of input variables (%i) is invalid\n",args.GetNIVars());
  }
  return kTRUE;
}

Bool_t Selector(QProcArgs &args)
{
  if(args.IVarF(0)<0 || args.IVarF(0)>42) return kFALSE;
  if(args.IVarF(1)<0 || args.IVarF(1)>26) return kFALSE;
  if(args.IVarF(2)<0 || args.IVarF(2)>40) return kFALSE;
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

void ELLFunction(Int_t&, Double_t*, Double_t &f, Double_t *, Int_t)
{
  static Int_t i;
  static Double_t const* const* pars;
  pars=QSigExFit::GetCurInstance().GetProcessor()->GetParams();
  QSigExFit::GetCurInstance().ExecProc();
  f=0;
  for(i=0; i<QSigExFit::GetCurInstance().GetNParams(); i++) f+=*(pars[i]);
  f=2*(f-(QProcDouble&)QSigExFit::GetCurInstance().GetProcOutput(0));
}
