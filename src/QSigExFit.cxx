// Author: Kathryn Miknaitis <mailto:gator@u.washington.edu>, Pierre-Luc Drouin <http://www.pldrouin.net>
// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Author: Juergen Wendland <mailto:juergen@phas.ubc.ca>
// Copyright Carleton University

#include "QSigExFit.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExFit                                                          //
//                                                                    //
// This class uses the "JointPDFsProbs" joint probability densities   //
// TTree stored in "Probs/JointPDFsProbs" to estimate the population  //
// parameters values. To achieve this, it first reads different       //
// configuration parameters of population parameters and some         //
// constants from a card file. Then, it uses a parameters fitter      //
// function chosen by the user to estimate the parameters and their   //
// error. The minimization routine is run twice once with the initial //
// parameters defined in the cardfile and the second time with the    //
// result from the first fit as initial conditions.                   //
// The card file information and the results are placed in            //
// subfolders of the "Fit" QSigExStruct.                              //
//                                                                    //
////////////////////////////////////////////////////////////////////////


ClassImp(QSigExFit)

void QSigExFit::FormatDir()
{
  //This protected function gives to the fMyDir TDirectory the structure that is
  //needed to store the information produced by this class. It creates
  //directories "Fit" and subdirectories "Fit/Setup" and "Fit/Numbers". 

  PRINTF2(this,"\tvoid QSigExFit::FormatDir()\n")
    try{
      if(!(fFitDir=(QSigExStruct*)fMyDir->Get(kFitIdx))){
	fFitDir=(QSigExStruct*)fMyDir->mkdir("Fit","Fit",kFitIdx);
      }
      if(!FindObjKey("Setup",fFitDir)){
	fFitDir->mkdir("Setup","Setup",kSetupIdx);
      }
      if(!FindObjKey("Numbers",fFitDir)){
	fFitDir->mkdir("Numbers","Numbers",kNumbersIdx);
      }
      if(!FindObjKey("Minuit",fFitDir)){
	fFitDir->mkdir("Minuit","Minuit",kMinuitIdx);
      }
    }catch(int e){
      cout << "Exception handled by QSigExFit::FormatDir\n";
      throw e;
    }
}

void QSigExFit::CleanDir()
{
  //This public member function reinitializes the part of the fMyDir directory
  //structure that belongs to QSigExFit. It removes "Fit" QSigExStruct and calls
  //FormatDir().

  PRINTF2(this,"\tvoid QSigExFit::CleanDir()\n")
    try{
      fMyDir->Delete(kFitIdx);
      fFitDir=NULL;
    }catch(int e){
      cout << "Exception handled by QSigExFit::CleanDir\n";
      throw e;
    }
}

void QSigExFit::ClearCardBuf()
{         
  //This member function clears the internal variables used to store the
  //configuration information of this class in card file format 

  fFluxCard.Clear();
  fVariabsCard.Clear();
  fMinimCard.Clear();
  fMinosCard.Clear();
}

void QSigExFit::LoadCardFile(const Char_t* cardfilename)
{
  //This public member function reads the card file with filename cardfilename
  //and saves the information needed by this class in its internal member
  //variables.
  //
  //Four cardfile keywords are read by this function: "flux", "variab",
  //"minimizer" and "minos". The first type of entry specifies different
  //configuration parameters for the population parameters. The second type
  //specifies constants that can be used by the parameters fitter function. The
  //third type specifies which Minuit minimizer has to be used, the "UP" value
  //on the minimization function used to compute the parameters errors and some
  //parameters needed by the minimizer. The fourth type is optional and
  //specifies the maximum number of function calls per parameter allowed in the
  //MINOs calculations. Refer to Minuit documentation for more details.
  //
  //Syntax of "flux" card file entries:
  //flux  [fgroup]  [active]  [startval]  [minval]  [maxval]  [stepval]
  //where [fgroup] is the flux group (popluation parameter) name, [active] is a
  //boolean value that indicates if the parameter value is unfixed, [startval]
  //is the starting parameter value, [minval] and [maxval] the range limits of
  //the parameter value and [stepval] is the step value used to fit the
  //parameter.
  //
  //Syntax of "variab" card file entries:
  //variab  [vname]  [vvalue]
  //where [vname] is the constant name and [vvalue] is the constant value
  //
  //Syntax of "minimizer" card file entry:
  //minimizer  [minimname]  [FCNError] [...] where [minimname] is the Minuit
  //minimizer name, [FCNError] is the minimization function "UP" value used to
  //compute the parameters erros, and [...] are the optional minimizer
  //parameters (see Minuit documentation for more details).
  //
  //Syntax of "minos" optional card file entry:
  //minos  [maxcalls] where [maxcalls] is the maximum number of function calls
  //per parameter allowed in the MINOs calculations. If this card file entry is
  //not used, the default Minuit value is used.

  PRINTF4(this,"\tvoid QSigExFit::LoadCardFile(const Char_t* cardfilename<",
      cardfilename,">)\n")
    try{
      if(cardfilename){
	fReader.SetFilename(cardfilename);
	fReader.SetKeyword("flux");
	fFluxCard=fReader.GetMany(0);

	fReader.SetKeyword("variab");      
	fVariabsCard=fReader.GetMany();

	fReader.SetKeyword("minimizer");
	fMinimCard=fReader.Get(0);

	fReader.SetKeyword("minos");
	fMinosCard=fReader.Get(0);
      }
    }catch(int e){
      cout << "Exception handled by QSigExFit::LoadCardFile\n";
      throw e;
    }
}

void QSigExFit::AddFlux(const Char_t* fgroup, Bool_t active, Float_t startval, Float_t minval, Float_t maxval, Float_t stepval)
{
  //This function adds a flux group in the internal member variables. The format of the arguments is described in QSigExFit::LoadCardFile.

  QList<TString> entry;
  entry.RedimList(7);
  entry[0]="flux";
  entry[1]=fgroup;
  entry[2]+=active;
  entry[3]+=startval;
  entry[4]+=minval;
  entry[5]+=maxval;
  entry[6]+=stepval;
  fFluxCard+=entry;
  entry.Clear();
}

void QSigExFit::AddVariab(const Char_t* vname, Float_t vvalue)
{
  //This function adds a constant in the internal member variables. The format of the arguments is described in QSigExFit::LoadCardFile.

  QList<TString> entry;
  entry.RedimList(3);
  entry[0]="variab";
  entry[1]=vname;
  entry[2]+=vvalue;
  fVariabsCard+=entry;
  entry.Clear();
}

void QSigExFit::SetMinimizer(const Char_t* minimname, const Char_t* fcnerror, const Char_t* param1, const Char_t* param2, const Char_t* param3, const Char_t* param4, const Char_t* param5, const Char_t* param6, const Char_t* param7, const Char_t* param8)
{
  //This functions sets the minimizer parameters. The format of the arguments is described in QSigExFit::LoadCardFile.

  fMinimCard.Clear();
  fMinimCard+="minimizer";
  fMinimCard+=minimname;
  fMinimCard+=fcnerror;
  if(param1) fMinimCard+=(TString)param1;
  if(param2) fMinimCard+=(TString)param2;
  if(param3) fMinimCard+=(TString)param3;
  if(param4) fMinimCard+=(TString)param4;
  if(param5) fMinimCard+=(TString)param5;
  if(param6) fMinimCard+=(TString)param6;
  if(param7) fMinimCard+=(TString)param7;
  if(param8) fMinimCard+=(TString)param8;
}

void QSigExFit::SetMinosMaxNCalls(Int_t maxcalls)
{
  //This functions sets the maximum number of calls in Minos.

  fMinosCard.RedimList(2);
  fMinosCard[0]="minos"; 
  fMinosCard[1]=maxcalls;
}

void QSigExFit::GetParametersParams()
{
  //This protected function reads the "flux" entries from the internal member
  //variables of QSigExFit and stores the information in
  //"Fit/Setup/Parameters/[fgroup]" directories as QNamedVar objects ([fgroup] is
  //the name of the entry flux group). 

  PRINTF2(this,"\tvoid QSigExFit::GetParmetersParams()\n")

    try{

      //set the number of columns to expect in the flux part of the card file
      const Int_t numfluxfields = 7;
      //set the column/index numbers for the inputs associated with the fluxes
      const Int_t fluxgroupindex = 1;
      const Int_t activeindex = 2;
      const Int_t startvalindex = 3;
      const Int_t minvalindex = 4;
      const Int_t maxvalindex = 5;
      const Int_t stepvalindex = 6;

      //Creates a directory to put the flux card parameters info
      QSigExStruct* fcarddir=(QSigExStruct*)(((QSigExStruct*)fFitDir->Get(kSetupIdx))->mkdir("Parameters","",kSParamsIdx));


      //If there are no fluxes defined, complain!
      if(!fFluxCard.Count()){
	cout << "QSigExFit::GetParmetersParams: Error in dat file: no flux parameters defined\n";
	throw 1014;
      }

      //some buffers to temporarily hold inputs
      QSigExStruct* dirbuf=NULL;
      TObject *nvbuf;

      //For each of the flux lines from the card file, fill the appropriate lists
      // with the information from that line:
      for(Int_t i=0;i<fFluxCard.Count();i++){

	//First check that we loaded the flux info correctly 
	CheckCardNFields(fFluxCard[i].Count(),numfluxfields,numfluxfields);

	dirbuf=(QSigExStruct*)(fcarddir->mkdir(fFluxCard[i][fluxgroupindex],"",i));

	nvbuf=new QNamedVar<Float_t>("StartVal",fFluxCard[i][startvalindex].Data());
	dirbuf->Add(nvbuf,kPStartValIdx);

	nvbuf=new QNamedVar<Float_t>("MinVal",fFluxCard[i][minvalindex].Data());
	dirbuf->Add(nvbuf,kPMinValIdx);

	nvbuf=new QNamedVar<Float_t>("MaxVal",fFluxCard[i][maxvalindex].Data());
	dirbuf->Add(nvbuf,kPMaxValIdx);

	nvbuf=new QNamedVar<Float_t>("StepVal",fFluxCard[i][stepvalindex].Data());
	dirbuf->Add(nvbuf,kPStepValIdx);

	nvbuf=new QNamedVar<UShort_t>("Active",fFluxCard[i][activeindex].Data());
	dirbuf->Add(nvbuf,kPActiveIdx);

	nvbuf=new QNamedVar<Int_t>("Index",i);
	dirbuf->Add(nvbuf,kPIndexIdx);
      }

    }catch(Int_t e){
      cout << "Exception handled by QSigExFit::GetParmetersParams\n";
      throw e;
    }
}

void QSigExFit::GetConstants()
{
  //This protected function reads the "variabs" entries from the internal member
  //variables of QSigExFit and stores the information in "Fit/Setup/Constants"
  //QSigExStruct as QNamedVar objects. 

  try{
    //set the number of columns to expect in the "variab" part of the card file
    const Int_t numvariabfields = 3;  
    //set the column/index numbers for the inputs associated with the "variabs"
    const Int_t variabnameindex = 1;
    const Int_t variabvalueindex = 2;

    //Creates a directory to put the flux card variables info
    QSigExStruct* fcarddir=(QSigExStruct*)(((QSigExStruct*)fFitDir->Get("Setup"))->mkdir("Constants","",kSConstantsIdx));

    TObject* nvbuf=NULL; 

    //for each entry of type "variab",
    for(Int_t i=0;i<fVariabsCard.Count();i++){

      //first check that we loaded in all of the lines correctly.
      CheckCardNFields(fVariabsCard[i].Count(),numvariabfields,numvariabfields);

      nvbuf=new QNamedVar<Double_t>(fVariabsCard[i][variabnameindex],fVariabsCard[i][variabvalueindex]);
      fcarddir->Add(nvbuf,i);
    }
  }catch(Int_t e){
    cout << "Exception handled by QSigExFit::GetConstants\n";
    throw e;
  }
}

void QSigExFit::GetMinimizerSettings()
{
  //This protected member function reads the "minimizer" entry from the internal
  //variables of QSigExFit and stores the information in "Fit/Setup/Minimizer"
  //QSigExStruct as QNamedVar objects.

  try{
    //Set the minimum number of fields needed by the card file entry
    const Int_t minfields=3;
    //Set the minimizer name field index
    const Int_t minimnameindex=1;
    //Set the FCN error index
    const Int_t errindex=2;

    //Create the "Fit/Setup/Minimizer" QSigExStruct
    QSigExStruct* fcarddir=(QSigExStruct*)(((QSigExStruct*)fFitDir->Get("Setup"))->mkdir("Minimizer","",kSMinimizerIdx));

    //Check the number of fields
    CheckCardNFields(fMinimCard.Count(),minfields);

    TObject* nvbuf; //QNamedVar buffer

    //Create a QNamedVar object named "Minimizer" and that holds the minimizer name
    //in its title 
    nvbuf=new QNamedVar<TString>("Minimizer",fMinimCard[minimnameindex].Data());
    //Add this QNamedVar object to the minimizer QSigExStruct
    fcarddir->Add(nvbuf,kMMinimizerIdx);

    //Create a QNamedVar object named "FCNError" and that holds the minimization
    //function "UP" vallue in its title
    nvbuf=new QNamedVar<Double_t>("FCNError",fMinimCard[errindex].Data());
    //Add this QNamedVar object to the minimizer QSigExStruct
    fcarddir->Add(nvbuf,kMFCNErrorIdx); 

    TString sbuf; //TString buffer

    //Loop over the remaining fields in the card file entry
    for(Int_t i=3;i<fMinimCard.Count();i++){
      //Build the QNamedVar object name
      sbuf="Arg";
      sbuf+=i-2;
      //Create a new QNamedVar that holds the minimizer argument value in its title
      nvbuf=new QNamedVar<TString>(sbuf,fMinimCard[i]);
      //Add this QNamedVar object to the minimizer QSigExStruct
      fcarddir->Add(nvbuf,kMOtherFieldsIdx+i-3);
    }
  }catch(int e){
    cout << "Exception handled by QSigExFit::GetMinimizerSettings\n";
    throw e;
  }
}

void QSigExFit::GetMinosSettings()
{
  //This protected member function reads the "minos" entry from the internal
  //variables of QSigExFit and stores the information in "Fit/Setup/MINOs"
  //QSigExStruct as QNamedVar objects.

  try{

    //Create a "Fit/Setup/MINOs" QSigExStruct
    QSigExStruct* fcarddir=(QSigExStruct*)(((QSigExStruct*)fFitDir->Get("Setup"))->mkdir("MINOs","",kSMINOsIdx));

    TObject* nvbuf; //QNamedVar buffer
    //If the number of fields in the card file entry is greater than 1
    if(fMinosCard.Count()>1){
      //Check if the number of fields is 2
      CheckCardNFields(fMinosCard.Count(),2,2);

      //Create a new QNamedVar object named "MaxCalls" and which title is field value
      nvbuf=new QNamedVar<Int_t>("MaxCalls",fMinosCard[1].Data());
      //Else if the number of fields in 0 (no card file entry) or 1
    }else{
      //Create a QNamedVar object named "MaxCalls" and which title is "-1" (Minuit
      //default value for the maximum number of MINOs calls per parameter)
      nvbuf=new QNamedVar<Int_t>("MaxCalls",-1);
    }
    //Aadd the QNamedVar object to the MINOs QSigExStruct
    fcarddir->Add(nvbuf,kMINOsMaxCallsIdx);

  }catch(int e){
    cout << "Exception handled by QSigExFit::GetMinosSettings\n";
    throw e;
  }
}

void QSigExFit::SetFCN(void (*fcn)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t))
{
  //This overloaded version of QSigExFit::SetFCN() function is called when fcn is
  //a compiled function (fcn pointer is passed in compiled code or by the CINT
  //interpreter). It sets the parameters fitter function.

  PRINTF4(this,"\tvoid QSigExFit::SetFCN(void (*fcn)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t)<",fcn,">)\n")

    fInterpretedFunc=NULL;
  fCompiledFunc=fcn;
}

void QSigExFit::SetFCN(void* fcn)
{
  //This overloaded version of QSigExFit::SetFCN() function is called when fcn
  //is an interpreted function by the CINT interpreter. It sets the parameters
  //fitter function.

  PRINTF4(this,"\tvoid QSigExFit::SetFCN(void* fcn<",fcn,">)\n")

    fCompiledFunc=NULL;
  fInterpretedFunc=fcn;
}

Int_t QSigExFit::Get()
{
  //This function estimates the population parameters values and their
  //error using the "Probs/JointPDFsProbs/JointPDFsProbs" joint
  //probability densities TTree, configuration parameters and
  //constants from the internal member variables (see
  //QSigExFit::LoadCardFile() for more details on configuration
  //parameters and fit constants). The parameters fitter function is
  //defined by calling QSigExFit::SetFCN(). The output of this
  //function is contained in a set of subfolders in "Fit" TDirectory:
  //"Fit/Setup/Parameters/[fgroup]", "Fit/Setup/Constants",
  //"Fit/Setup/Minimizer", "Fit/Setup/MINOs" and
  //"Fit/Numbers/[fgroup]", where [fgroup] are the flux groups
  //(parameters names). The subfolder "Fit/Setup" contains the fit
  //setup information and the subfolder "Fit/Numbers" contains the fit
  //results, the covariance matrix (TMatrixDSym object) of the
  //fitted, non-fixed  parameters and the minimum function value (QNamedVar
  //object with name "Fmin"). Each "Fit/Numbers/[fgroup]" subfolder contains
  //3 QNamedVar objects which name is explicit:
  //             -"FitValue"
  //             -"MinusFitError"
  //             -"PlusFitError"
  //
  //The Minuit status is stored in QNamedVar object "Fit/Minuit/Status".
  //
  //"Fit/Numbers" contains also a fourth QNamedVar object,
  //"ActParamIndex" that constains the index of the parameter in the
  //covariance matrix. The value is "-1" if the parameter is fixed.
  // 
  //This function returns 0

  PRINTF2(this,"\tInt_t QSigExFit::Get()\n")

    try{
      //Format the TDirectory structure
      FormatDir();

      //Create the QNamedVar objects for the config params of fit params
      GetParametersParams();
      //Create the QNamedVar objects for "variab" entries
      GetConstants();
      //Create the QNamedVar objects for "minimizer" entry
      GetMinimizerSettings();
      //Create the QNamedVar object for "minos" entry
      GetMinosSettings();

      //Do some checks
      CheckJointPDFsProbs();

      Int_t i; //iterator

      TDirectory* dirbuf;
      Bool_t *hasloadedjp=new Bool_t[fJProbsDirs.GetEntries()];

      TList params; //List of population params objects
      //Fill params with the list of objects in "Fit/Setup/Parameters" TDirectory 
      GetDirs(&params,(TDirectory*)((TDirectory*)fFitDir->Get("Setup"))->Get("Parameters"));

      //number of parameters being fit is numpar:
      Int_t numpar=params.GetEntries();

      TList variables; //List of "variab" QNamedVar objects
      //Fill variables with the list of objects in the "Fit/Setup/Constants" TDirectory
      GetObjs(&variables,(TDirectory*)((TDirectory*)fFitDir->Get("Setup"))->Get("Constants"));

      //create an array for parameter names:
      const Char_t** parname=new const Char_t*[numpar];     
      QList<TString> parnames;

      //create an array for start values:
      Float_t* vstart=new Float_t[numpar];                  

      //create arrays for min and max values;
      Float_t* vmin=new Float_t[numpar];
      Float_t* vmax=new Float_t[numpar];

      //create an array for step values:
      Float_t* step=new Float_t[numpar]; 
      Bool_t* act=new Bool_t[numpar];
      Double_t dbuf;
      TList param;

      //Loops over the parameters objects in "Fit/Setup/Parameters"
      for(i=0; i<params.GetEntries(); i++){
	dirbuf=(TDirectory*)params.At(i);
	//Get the list of objects in the TDirectory
	GetObjs(&param,dirbuf);
	//Set the parameter name
	parnames+=(TString)(dirbuf->GetName());
	parname[i]=parnames[i].Data();
	//Get the start value for the parameter
	printf("%p\t%p\n",dirbuf->Get("StartVal"),dirbuf->Get("MinVal"));
	vstart[i]=*((QNamedVar<Float_t>*)dirbuf->FindObject("StartVal"));
	//Get the min value for the parameter
	vmin[i]=*((QNamedVar<Float_t>*)dirbuf->FindObject("MinVal"));
	//Get the max value for the parameter
	vmax[i]=*((QNamedVar<Float_t>*)dirbuf->FindObject("MaxVal"));
	//Get the step value for the parameter
	step[i]=*((QNamedVar<Float_t>*)dirbuf->FindObject("StepVal"));
	//Get the active flag for the parameter
	act[i]=(UShort_t)*((QNamedVar<UShort_t>*)dirbuf->FindObject("Active"));
	//Clear the list of objects in the parameter TDirectory
	param.Clear();
	//Exit the loop if a match has been found
      }

      //Clear the list of parameters TDirectory without deleting its objects
      params.Clear("nodelete");

      //Create an instance of QSigExFitDataHolder to pass information to the fit function
      QSigExFitDataHolder dataholder(&parnames,&variables);

      for(i=0; i<fJProbsDirs.GetEntries(); i++){
	dirbuf=(TDirectory*)fJProbsDirs.At(i);
	//Get a pointer to "Probs/JointPDfsProbs" TDirectory
	dirbuf=(TDirectory*)((TDirectory*)dirbuf->Get("Probs"))->Get("JointPDFsProbs");
	//Is the joint probability densities TTree already loaded in memory?
	hasloadedjp[i]=!dynamic_cast<TTree*>(dirbuf->FindObject("JointPDFsProbs"));
	//Get a pointer to this TTree
	dataholder.AddJProbs(dynamic_cast<TTree*>(dirbuf->Get("JointPDFsProbs")),hasloadedjp[i]);
      }

      //Create a TMinuit instance 
      TMinuit minuit(numpar);

      //Get a pointer to "Fit/Setup/Minimizer" TDirectory
      TDirectory* minimdir=(TDirectory*)((TDirectory*)fFitDir->Get("Setup"))->Get("Minimizer");
      //Get a pointer to "Fit/Setup/MINOs" TDirectory
      TDirectory* minosdir=(TDirectory*)((TDirectory*)fFitDir->Get("Setup"))->Get("MINOs");

      //Set parameters to use for Minuit SET PRINT command:
      Double_t level=0;                       // -1 tells minuit we want minimal printout.
      Int_t ierflg;
      //Execute the minuit SET PRINT command:
      minuit.mnexcm( "SET PRINT", &level, 1, ierflg );  

      //Execute the minuit SET WIDTH command: (changes the width of std output)
      Double_t width=200;
      minuit.mnexcm( "SET WIDTH", &width, 1, ierflg );  

      //Initialize Minuit with the type of fit function we are using.  
      //See http://root.cern.ch/root/roottalk/roottalk97/0407.html for more details on the following lines
      if(fCompiledFunc){ //If the function is compiled
	//If the function pointer is passed from CINT, call TMinuit::SetFCN(void*)
	if(G__p2f2funcname((void*)fCompiledFunc)) minuit.SetFCN((void*)fCompiledFunc);
	//else if it's passed from compiled code, call TMiuit::SetFCN(void (*)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t))
	else minuit.SetFCN(fCompiledFunc);  //Sets name of fit function
      }
      else if(fInterpretedFunc){ //Else if the function is interpreted by CINT
	minuit.SetFCN(fInterpretedFunc);  //Sets name of fit function
      }else{ //Else if no function has been set, throw an error
	cout << "Error: A fit function has not been set\n";
	throw 1;
      }

      TString minimname; //Minuit minimizer name
      Double_t fcnerror; //Relative value of FCN function from its minimum value
      Double_t* minimargs=NULL; //Minimizer agruments
      Int_t nminimargs=0; //Number of minimizer arguments
      TString strbuf; //TString buffer
      QNamedVar<Double_t>* dnvbuf; //QNamedVar<Double_t> buffer
      TObject* nvbuf; //QNamedVar buffer

      TList minimdlist; //List of objects in Minimizer TDirectory
      //Fill minimdlist with the list of objects in "Fit/Setup/Minimizer"
      //TDirectory
      GetObjs(&minimdlist,minimdir);
      //Read the minimizer name
      minimname=*(dynamic_cast<QNamedVar<TString>*>(minimdir->Get("Minimizer")));
      //Read the error value
      fcnerror=*(dynamic_cast<QNamedVar<Double_t>*>(minimdir->Get("FCNError")));
      //Build the first argument name
      strbuf="Arg";
      strbuf+=nminimargs+1;
      //Loop over the arguments
      while((dnvbuf=dynamic_cast<QNamedVar<Double_t>*>(minimdir->Get(strbuf)))){
	//Resize the argument values array
	minimargs=(Double_t*)realloc(minimargs,(nminimargs+1)*sizeof(Double_t));
	//Read the current argument value
	minimargs[nminimargs]=*(dnvbuf);
	//Increment the number of arguments
	nminimargs++;
	//Build the next argument name
	strbuf="Arg";
	strbuf+=nminimargs+1; 
      }
      //Clear the list of objects in the minimizer TDirectory and delete its
      //objects
      minimdlist.Clear();

      Double_t minosmaxcalls; //Number of maximum calls per parameter
      TList minosdlist; //Number of objects in "Fit/Setup/MINOs" TDirectory
      //Fill minosdlist with de list of objects in MINOs TDirectory
      GetObjs(&minosdlist,minosdir);
      //Read the maximum number of calls (if negative, no maximum)
      minosmaxcalls=*(dynamic_cast<QNamedVar<Int_t>*>(minosdir->Get("MaxCalls")));
      //Clear the list of objects in MINOs TDirectory and delete its objects
      minimdlist.Clear();

      //set Minuit parameters to "undefined"
      minuit.mncler(); 

      //Execute the SET ERR command:
      minuit.mnexcm("SET ERR", &fcnerror, 1, ierflg );

      ierflg=0;

      //Loop through parameters, and initialize a variable in Minuit for each one.
      for (i=0; i<numpar; i++){

	//mnparm implements a parameter definition with a parameter number,
	//name, starting value, step size, min and max values, and an error flag.

	minuit.mnparm(i, parname[i], vstart[i], step[i], vmin[i], vmax[i], ierflg);
	//If the parameter is fix, tell to TMinuit
	if(!act[i]){
	  dbuf=i+1;
	  minuit.mnexcm("FIX",&dbuf,1,ierflg);     //fix the parameter with that index
	}

      }

      Double_t arglist; //dummy Float_t

      //**** Call the user defined minimizer ****
      const_cast<TMinuit&>(minuit).mnexcm(minimname, minimargs, nminimargs, ierflg ); 
      // set fit parameters to result from this run of the minimizer
      for (i=0; i<numpar; i++){
	Double_t dbuf1,dbuf2,dbuf3,dbuf4; 
	Int_t ibuf1;
	if(act[i]) {
	  const_cast<TMinuit&>(minuit).mnpout(i,strbuf,dbuf4,dbuf1,dbuf2,dbuf3,ibuf1);
	  minuit.mnparm(i, parname[i], dbuf4, step[i], vmin[i], vmax[i], ierflg);
	}
      }
      // rerun minimizer
      const_cast<TMinuit&>(minuit).mnexcm(minimname, minimargs, nminimargs, ierflg ); 

      //Clean up arrays we used:
      delete[] parname;
      delete[] vstart;
      delete[] vmin;
      delete[] vmax;
      delete[] step;

      ierflg = 0; 

      free(minimargs);

      //Print the covariance matrix
      cout << "\nParameter correlations calculated by MIGRAD:"<<"\n";
      const_cast<TMinuit&>(minuit).mnexcm("SHOW COR",&arglist,0,ierflg);

      //Calculate non-symmetric errors with MINOS:
      //Minuit calculates errors by finding the change in the parameter value 
      //required to change the function by fcnerror.
      //In the case of a Chi-2, 
      //     fcnerror =1.0 --> 1 sigma
      //     fcnerror =4.0 --> 2 sigma
      //     fcnerror =9.0 --> 3 sigma
      //When minosmaxcalls is positive, it sets the maximum number of function
      //calls per parameter to its values 
      const_cast<TMinuit&>(minuit).mnexcm("MINO",&minosmaxcalls,(Int_t)(minosmaxcalls>=0),ierflg); 

      //buffers to hold minuit results
      Double_t dbuf1,dbuf2,dbuf3,dbuf4;    

      //store the fit status in "Fit/Minuit" TDirectory
      TDirectory* statfitdir=(TDirectory*)(fFitDir->Get("Minuit"));

      nvbuf=new QNamedVar<TString>("Status",const_cast<TMinuit&>(minuit).fCstatu);
      statfitdir->Add(nvbuf);

      //Get a pointer to "Fit/Numbers" TDirectory
      TDirectory* numfitdir=(TDirectory*)(fFitDir->Get("Numbers"));

      nvbuf=new QNamedVar<Double_t>("Fmin",const_cast<TMinuit&>(minuit).fAmin);
      numfitdir->Add(nvbuf);

      Int_t ibuf1; //Int_t buffer

      Int_t numapar=0; //Number of floating parameters

      //Loop over the parameters (flux groups)
      for(Int_t i=0;i<numpar;i++){
	//Create a TDirectory to hold the fit information for the current
	//parameter
	dirbuf=numfitdir->mkdir(parnames[i]);

	//mnpout takes in the index of the parameter we're asking about, and returns
	//it's name, fitted value, estimate of parameter uncertainty, lower limit
	//on the parameter value, upper limit on the parameter value, and the
	//internal parameter number (if the parameter is variable).  See Minuit
	//documentation for details.  We aren't actually interested in any of this
	//except the fit value.

	const_cast<TMinuit&>(minuit).mnpout(i,strbuf,dbuf4,dbuf1,dbuf2,dbuf3,ibuf1);

	//Create a QNamedVar object named "FitValue" which title is the parameter
	//value
	nvbuf=new QNamedVar<Double_t>("FitValue",dbuf4);
	//Add the QNamedVar object to the parameter TDirectory
	dirbuf->Add(nvbuf);

	//mnerrs reports the errors calculated by MINOS.  It takes in the index of 
	//the parameter we're asking about, and returns the positive error, 
	//negative error (as a negative number), the parabolic parameter error 
	//and the global correlation coefficient for the parameter.  

	const_cast<TMinuit&>(minuit).mnerrs(i,dbuf4,dbuf3,dbuf1,dbuf2);
	//print out the global correlation coefficients for the variables:
	//cout <<"parameter: "<<strbuf<<"  MINOS global corr. coeff.: "<<dbuf2<<"\n";

	//Create a QNamedVar object named "PlusFitError" which title is the parameter
	//plus error value
	nvbuf=new QNamedVar<Double_t>("PlusFitError",dbuf4);
	//Add the QNamedVar object to the parameter TDirectory
	dirbuf->Add(nvbuf);

	//Create a QNamedVar object named "MinusFitError" which title is the
	//parameter minus error value
	nvbuf=new QNamedVar<Double_t>("MinusFitError",dbuf3);
	//Add the QNamedVar object to the parameter TDirectory
	dirbuf->Add(nvbuf);

	//Create a QNamedVar object named "ActParamIndex" which title is the
	//active parameter value (-1 if fixed)
	nvbuf=new QNamedVar<Int_t>("ActParamIndex",act[i] ? numapar++ : -1);
	//Add the QNamedVar object to the parameter TDirectory
	dirbuf->Add(nvbuf);

	// Finally copy the index of the parameter into the TDirectory so that we
	// know the indexing for later analysis
	nvbuf=new QNamedVar<Int_t>("ParamIndex",i);
	dirbuf->Add(nvbuf);

      }

      delete[] act;
      parnames.Clear();

      Double_t *covmat=new Double_t[numapar*numapar]; //covariance matrix array
      //Get the covariance matrix from TMinuit
      const_cast<TMinuit&>(minuit).mnemat(covmat,numapar);
      //Store the matrix in a TMatrixDSym object
      TMatrixDSym* covtmat=new TMatrixDSym(numapar,covmat);
      //Add the TMatrixDSym ocject ti the Fit directory
      numfitdir->Add(covtmat);
      //Delete the covariance matrix array
      delete[] covmat;


      minuit.mncler();

      //Clear the list of constants QNamedVar objects and delete its objects
      variables.Clear();

      return 0;

    }catch(Int_t e){
      cout << "Exception handled by QSigExFit::Get\n";
      throw e;
    }
}

void QSigExFit::CheckJointPDFsProbs() const
{
  //This protected function checks if a "Probs/JointPDFsProbs/JointPDFsProbs"
  //TTree exists

  PRINTF2(this,"\tvoid QSigExFit::CheckJointPDFsProbs()\n")

    TDirectory* probsdir;
  TDirectory* jprobsdir;

  if(!fJProbsDirs.GetEntries()){
    cout << "Error: The joint probabilities TTree list is empty\n";
    throw 1;
  }
  TIter iter(&fJProbsDirs);
  TDirectory* dirbuf;

  while((dirbuf=(TDirectory*)iter())){

    if(!(probsdir=dynamic_cast<TDirectory*>(dirbuf->Get("Probs"))) ||
	!(jprobsdir=dynamic_cast<TDirectory*>(probsdir->Get("JointPDFsProbs"))) ||
	!FindObjKey("JointPDFsProbs",jprobsdir)){
      cout << "Error: There's no JointPDFsProbs TDirectory in one of the elements of fJProbsDirs\n";
      throw 1;
    }
  }
}

#include "debugger.h"
