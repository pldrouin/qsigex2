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

  //Load histograms from existing ROOT file and convert them to QHN_D objects
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

  //Normalize the histograms using default normalization
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

  //Add the histograms to the output file
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

  //Create an array (tree) processor
  QArrayProcessor dataproc("Data Processor","Data Processor");
  //Request to save all output arrays at the end (this is only useful for debugging)
  QProcBranchHandler::SaveOutputs(kTRUE);
  QProcQOAHandler::SaveOutputs(kTRUE);
  //QProcessor::SetDefVerbosity(QProcessor::kShowExec|QProcessor::kShowExec2);

  //Add a selector process to apply the cuts (notice the last argument).
  dataproc.AddProc("Selector","Selector",Selector,NULL,kTRUE);
  //Add input branches to the selector process
  dataproc.GetProc("Selector").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("Selector").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("Selector").AddIVar("cthsun","tree://d2o_data.root:Tree");


  //Add a Post Selection process to evaluate the 1D PDF. The process will use the function PDFEval.
  //Only the selected events will be processed (notice the last argument).
  dataproc.AddPSProc("CCE PDF Eval","CCE PDF Eval",PDFEval,NULL,kTRUE);
  //Assign an input branch to the process
  dataproc.GetProc("CCE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  //Assign the PDF to the process
  dataproc.GetProc("CCE PDF Eval").AddIObj(cce);
  //Add an output branch to store the probability density values
  dataproc.GetProc("CCE PDF Eval").AddOVar("f_e","tree://ccpdf");

  //Idem for other PDF
  dataproc.AddPSProc("CCR PDF Eval","CCR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("CCR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("CCR PDF Eval").AddIObj(ccr);
  dataproc.GetProc("CCR PDF Eval").AddOVar("f_r","tree://ccpdf");

  //Idem for other PDF
  dataproc.AddPSProc("CCC PDF Eval","CCC PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("CCC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("CCC PDF Eval").AddIObj(ccc);
  dataproc.GetProc("CCC PDF Eval").AddOVar("f_c","tree://ccpdf");


  //Idem for other PDF
  dataproc.AddPSProc("ESE PDF Eval","ESE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("ESE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("ESE PDF Eval").AddIObj(ese);
  dataproc.GetProc("ESE PDF Eval").AddOVar("f_e","tree://espdf");

  //Idem for other PDF
  dataproc.AddPSProc("ESR PDF Eval","ESR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("ESR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("ESR PDF Eval").AddIObj(esr);
  dataproc.GetProc("ESR PDF Eval").AddOVar("f_r","tree://espdf");

  //Idem for other PDF
  dataproc.AddPSProc("ESC PDF Eval","ESE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("ESC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("ESC PDF Eval").AddIObj(esc);
  dataproc.GetProc("ESC PDF Eval").AddOVar("f_c","tree://espdf");


  //Idem for other PDF
  dataproc.AddPSProc("NCE PDF Eval","NCE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("NCE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("NCE PDF Eval").AddIObj(nce);
  dataproc.GetProc("NCE PDF Eval").AddOVar("f_e","tree://ncpdf");

  //Idem for other PDF
  dataproc.AddPSProc("NCR PDF Eval","NCR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("NCR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("NCR PDF Eval").AddIObj(ncr);
  dataproc.GetProc("NCR PDF Eval").AddOVar("f_r","tree://ncpdf");

  //Idem for other PDF
  dataproc.AddPSProc("NCC PDF Eval","NCC PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("NCC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("NCC PDF Eval").AddIObj(ncc);
  dataproc.GetProc("NCC PDF Eval").AddOVar("f_c","tree://ncpdf");


  //Idem for other PDF
  dataproc.AddPSProc("BKE PDF Eval","BKE PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("BKE PDF Eval").AddIVar("energy","tree://d2o_data.root:Tree");
  dataproc.GetProc("BKE PDF Eval").AddIObj(bke);
  dataproc.GetProc("BKE PDF Eval").AddOVar("f_e","tree://bkpdf");

  //Idem for other PDF
  dataproc.AddPSProc("BKR PDF Eval","BKR PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("BKR PDF Eval").AddIVar("radius","tree://d2o_data.root:Tree");
  dataproc.GetProc("BKR PDF Eval").AddIObj(bkr);
  dataproc.GetProc("BKR PDF Eval").AddOVar("f_r","tree://bkpdf");

  //Idem for other PDF
  dataproc.AddPSProc("BKC PDF Eval","BKC PDF Eval",PDFEval,NULL,kTRUE);
  dataproc.GetProc("BKC PDF Eval").AddIVar("cthsun","tree://d2o_data.root:Tree");
  dataproc.GetProc("BKC PDF Eval").AddIObj(bkc);
  dataproc.GetProc("BKC PDF Eval").AddOVar("f_c","tree://bkpdf");


  //Add a Post Selection process to compute the joint probabilities. The process will use the function Multiply
  dataproc.AddPSProc("CC JP","CC JP",Multiply);
  //Add the appropriate branches that contain the probability densities
  dataproc.GetProc("CC JP").AddIVar("f_e","tree://ccpdf");
  dataproc.GetProc("CC JP").AddIVar("f_r","tree://ccpdf");
  dataproc.GetProc("CC JP").AddIVar("f_c","tree://ccpdf");
  //Store the joint probabilities in a new output branch
  dataproc.GetProc("CC JP").AddOVar("cc","tree://jointpdf");


  //Idem for other signal
  dataproc.AddPSProc("ES JP","ES JP",Multiply);
  dataproc.GetProc("ES JP").AddIVar("f_e","tree://espdf");
  dataproc.GetProc("ES JP").AddIVar("f_r","tree://espdf");
  dataproc.GetProc("ES JP").AddIVar("f_c","tree://espdf");
  dataproc.GetProc("ES JP").AddOVar("es","tree://jointpdf");


  //Idem for other signal
  dataproc.AddPSProc("NC JP","NC JP",Multiply);
  dataproc.GetProc("NC JP").AddIVar("f_e","tree://ncpdf");
  dataproc.GetProc("NC JP").AddIVar("f_r","tree://ncpdf");
  dataproc.GetProc("NC JP").AddIVar("f_c","tree://ncpdf");
  dataproc.GetProc("NC JP").AddOVar("nc","tree://jointpdf");


  //Idem for other signal
  dataproc.AddPSProc("BK JP","BK JP",Multiply);
  dataproc.GetProc("BK JP").AddIVar("f_e","tree://bkpdf");
  dataproc.GetProc("BK JP").AddIVar("f_r","tree://bkpdf");
  dataproc.GetProc("BK JP").AddIVar("f_c","tree://bkpdf");
  dataproc.GetProc("BK JP").AddOVar("bk","tree://jointpdf");


  //Create a double value for a process (double value with time stamp)
  QProcDouble llsum;
  //Add a Post Selection process to compute the likelihood sum over the classes for the current event. The process will use the function LLSum
  dataproc.AddPSProc("LLSum","LLSum",LLSum);
  //Add the fit parameters to the process
  dataproc.GetProc("LLSum").AddParam("ncc");
  dataproc.GetProc("LLSum").AddParam("nes");
  dataproc.GetProc("LLSum").AddParam("nnc");
  dataproc.GetProc("LLSum").AddParam("nbk");
  //Add the joint probability branches to the process
  dataproc.GetProc("LLSum").AddIVar("cc","tree://jointpdf");
  dataproc.GetProc("LLSum").AddIVar("es","tree://jointpdf");
  dataproc.GetProc("LLSum").AddIVar("nc","tree://jointpdf");
  dataproc.GetProc("LLSum").AddIVar("bk","tree://jointpdf");
  //Add llsum as the output of the function
  dataproc.GetProc("LLSum").AddOObj(&llsum);

  //Analyze the processes
  dataproc.Analyze();
  //Print some info about the structure
  dataproc.PrintProcesses(0,kTRUE);
  //dataproc.PrintAnalysisResults();
  //Initialize the processes
  dataproc.InitProcess();

  //Create a QSigExFitMinuit instance
  QSigExFitMinuit fitter;
  //Attach the processor to the fitter
  fitter.SetProcessor(&dataproc);
  //Add llsum as an output to the minimizing function
  fitter.AddProcOutput(&llsum);

  //Set the fit parameters
  fitter.Param("ncc").Setup(2000,0.01,0,10000);
  fitter.Param("nes").Setup(200,0.01,0,1000);
  fitter.Param("nnc").Setup(500,0.01,0,1000);
  //the background is fixed
  fitter.Param("nbk").Setup(124.2,0.00,124.2,124.2,kTRUE);

  //Set the minimizing function
  fitter.SetFCN(ELLFunction);

  //Set Minuit verbosity
  fitter.SetVerbose(0);
  //Initialize the fit
  fitter.InitFit();
  //fitter.PrintParams();
  //Perform the fit
  fitter.Fit();
  //Print the results
  fitter.PrintParams();
  //Print the covariance matrix
  fitter.GetCovMatrix().Print();
  //Print the correlation matrix
  fitter.GetCorMatrix().Print();

  //Terminate the processes
  dataproc.TerminateProcess();

  //Add the fitter to the output file
  f1.Add(&fitter);

  f1.Write();
  f1.Close();
}

Bool_t PDFEval(QProcArgs &args)
{
  //Evaluate the probability density for the current event depending on the dimensionality of the PDF and store it as an output variable.
  //The input variables are float values and the output variables are double values (double is the default)
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
  //Select the current event based on its observable values
  if(args.IVarF(0)<0 || args.IVarF(0)>42) return kFALSE;
  if(args.IVarF(1)<0 || args.IVarF(1)>26) return kFALSE;
  if(args.IVarF(2)<0 || args.IVarF(2)>40) return kFALSE;
  return kTRUE;
}

Bool_t Multiply(QProcArgs &args)
{
  static Int_t i;
  //Multiply inputs together and store it in output
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

  //Compute the likelihood sum for the current event
  for(i=0; i<args.GetNParams(); i++) {
    P+=args.Param(i)*args.IVar(i);
  }

  //If the probability is non-zero, add the log to the output
  if(P>0) (*(QProcDouble*)args.OObj(0))+=log(P);

  return kTRUE;
}

void ELLFunction(Int_t&, Double_t*, Double_t &f, Double_t *, Int_t)
{
  static Int_t i;
  static Double_t const* const* pars;
  //Get a pointer to the array of parameters from the current processor
  pars=QSigExFit::GetCurInstance().GetProcessor()->GetParams();
  //Update the processor output depending on the updated inputs
  QSigExFit::GetCurInstance().ExecProc();
  //Compute the total number of events
  f=0;
  for(i=0; i<QSigExFit::GetCurInstance().GetNParams(); i++) f+=*(pars[i]);
  //Finish to compute the extended log-likelihood
  f=2*(f-(QProcDouble&)QSigExFit::GetCurInstance().GetProcOutput(0));
}
