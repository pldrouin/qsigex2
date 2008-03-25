// Author: J.Wendland <mailto:juergen@physics.ubc.ca>


#include "QSigExFlux2Events.h"
#include "QSigExF2EDataHolder.h"
#include "QSigExDis.h"
#include "QSigExDisTF.h"

#include "TList.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QSigExFlux2Events)

void QSigExFlux2Events::FormatDir()
{
  //This protected function gives to the fMyDir TDirectory the structure that is
  //needed to store the information produced by this class. It creates
  //directories "Fit" and subdirectories "Fit/Setup" and "Fit/Numbers". 

  PRINTF2(this,"\tvoid QSigExFlux2Events::FormatDir()\n")
  try{
    if(!(fF2EDir=(TDirectory*)fMyDir->Get("Physics"))){
      fF2EDir=fMyDir->mkdir("Physics","Physics");
    }
    if(!FindObjKey("Flux2Events",fF2EDir)){
      fF2EDir=fF2EDir->mkdir("Flux2Events","Flux2Events");
    }
  }catch(int e){
    cout << "Exception handled by QSigExFlux2Events::FormatDir\n";
    throw e;
  }
}

void QSigExFlux2Events::CleanDir()
{
  //This public member function reinitializes the part of the fMyDir directory
  //structure that belongs to QSigExFlux2Events. It removes "Fit" TDirectory and calls
  //FormatDir().

  PRINTF2(this,"\tvoid QSigExFlux2Events::CleanDir()\n")
  try{
    if((fF2EDir=(TDirectory*)fMyDir->Get("Physics"))){
      DelObjsKeys("Flux2Events",fF2EDir);
    }
    fF2EDir=NULL;
  }catch(int e){
    cout << "Exception handled by QSigExFlux2Events::CleanDir\n";
    throw e;
  }
}

void QSigExFlux2Events::ClearCardBuf()
{         
  //This member function clears the internal variables used to store the
  //configuration information of this class in card file format 
            
  fFlux2EventsCard.Clear();
}

void QSigExFlux2Events::LoadCardFile(const Char_t* cardfilename)
{
  //This public member function reads the card file with filename cardfilename
  //and saves the information needed by this class in its internal member
  //variables.

  PRINTF4(this,"\tvoid QSigExFlux2Events::LoadCardFile(const Char_t* cardfilename<",
      cardfilename,">)\n")
  try{
    if(cardfilename){
      fReader.SetFilename(cardfilename);
      fReader.SetKeyword("flux2events");
      fFlux2EventsCard=fReader.GetMany(0);
    }
  }catch(int e){
    cout << "Exception handled by QSigExFlux2Events::LoadCardFile\n";
    throw e;
  }
}

void QSigExFlux2Events::GetFluxToEventMapping()
{
  //This protected function reads the "flux2events" entries from the internal member
  //variables of QSigExFlux2Events and stores the information in "Fit/Setup/Flux2Events"
  //TDirectory as TNamed objects. 

  try{
    //set the number of columns to expect in the "flux2events" part of the card file
    const Int_t minfields = 7;  
    const Int_t maxfields = 9;  
    //set the column/index numbers for the inputs associated with the "flux2events"
    const Int_t activeindex = 1;
    const Int_t classindex  = 2;
    const Int_t nameindex   = 3;
    const Int_t objectindex = 4;
    const Int_t fileindex   = 5;
    const Int_t xfluxindex  = 6;
    const Int_t yfluxindex  = 7;
    const Int_t zfluxindex  = 8;
    
    TDirectory* flux2eventsdir;

    TDirectory* inputspdir; //Inputs TDirectory pointer
    QList<TString> inputs;
    Int_t ninputs;       //Number of inputs

    TNamed* nbuf;

    //for each entry of type "flux2events",
   for(Int_t i=0;i<fFlux2EventsCard.Count();i++){
     const QList<TString> anEntry = fFlux2EventsCard[i];

     //first check that we loaded in all of the lines correctly.
     CheckCardNFields(anEntry.Count(),minfields,maxfields);
     QSigExDis* flux2evtsbuf=NULL;
     if(anEntry[classindex]=="TF1"){
//        cout << "<QSigExFlux2Events::GetFluxToEventMapping> "
// 	    << anEntry[nameindex].Data() 
// 	    << " " << anEntry[objectindex]
// 	    << " " << anEntry[fileindex].Data() << endl;
       flux2evtsbuf=new QSigExDisTF(anEntry[classindex], anEntry[fileindex], anEntry[objectindex]);
     }
     else {
       cout << "<QSigExFlux2Events::GetFluxToEventMapping> ERROR: only implemented for TF1 objects." << endl;
       throw 9014;
     }
     if(flux2evtsbuf){
       // set up directory for this mapping function
       // flux2evtsbuf->SetName(anEntry[objectindex]);
       flux2evtsbuf->SetName(anEntry[nameindex].Data());
       flux2eventsdir = fF2EDir->mkdir(anEntry[nameindex].Data());
       nbuf=new TNamed(ClassName(),"Class Owner Identifier");

       // add the function (the TF* object)
       flux2eventsdir->Add(nbuf);
       flux2eventsdir->Add(flux2evtsbuf);

       // add the input fluxes for this function
       inputs.Clear();
       for(Int_t j=xfluxindex; j<anEntry.Count(); j++){
// 	 cout << j << " " << anEntry.Count() << " " << anEntry[j] << endl;
	 inputs+=anEntry[j];
       }
       ninputs=inputs.Count();
       inputspdir=flux2eventsdir->mkdir("Inputs"); // create an "Inputs" directory
       for(Int_t j=0;j<ninputs;j++){ // Loop over the coordinates in card file 
	 //Create a new TNamed object to contain the Input
	 nbuf=new TNamed(((TString)"Input ")+(long int)(j),inputs[j]);
	 //Add the Input object to the Input TDirectory
	 inputspdir->Add(nbuf);
	 //Increment the number of inputs
       }
     }
   }
  }catch(Int_t e){
    cout << "Exception handled by QSigExFlux2Events::GetFluxToEventMapping\n";
    throw e;
  }
}

Int_t QSigExFlux2Events::Get()
{
  //This function estimates the population parameters values and their
  //error using the "Probs/JointPDFsProbs/JointPDFsProbs" joint
  //probability densities TTree, configuration parameters and
  //constants from the internal member variables (see
  //QSigExFlux2Events::LoadCardFile() for more details on configuration
  //parameters and fit constants). The parameters fitter function is
  //defined by calling QSigExFlux2Events::SetFCN(). The output of this
  //function is contained in a set of subfolders in "Fit" TDirectory:
  //"Fit/Setup/Parameters/[fgroup]", "Fit/Setup/Constants",
  //"Fit/Setup/Minimizer", "Fit/Setup/MINOs" and
  //"Fit/Numbers/[fgroup]", where [fgroup] are the flux groups
  //(parameters names). The subfolder "Fit/Setup" contains the fit
  //setup information and the subfolder "Fit/Numbers" contains the fit
  //results and the covariance matrix (TMatrixDSym object) of the
  //fitted, non-fixed  parameters.  Each "Fit/Numbers/[fgroup]"
  //subfolder contains 3 TNamed objects which name is explicit:
  //             -"FitValue"
  //             -"MinusFitError"
  //             -"PlusFitError"
  //
  //The values of the results are stored in the titles
  //(TNamed::GetTitle()) of the objects.
  //
  //"Fit/Numbers" contains also a fourth TNamed object,
  //"ActParamIndex" which title is the index of the parameter in the
  //covariance matrix. This title is "-1" if the parameter is fixed.
  // 
  //This function returns 0

  PRINTF2(this,"\tInt_t QSigExFlux2Events::Get()\n")

  try{
    //Format the TDirectory structure
    FormatDir();
   
    //Create mapping of fluxes to events
    GetFluxToEventMapping();

    TDirectory *dirbuf;
    
    //create an instance of the dataholder used to pass info to the fitter
    QSigExF2EDataHolder f2edataholder;

    // get a pointer to the flux2events mapping TDirectory
    fF2EList = new TList;
    if(fF2EDir){
       GetDirs(fF2EList, fF2EDir);
       f2edataholder.AddMapping(fF2EList);
    }
  }catch(Int_t e){
    cout << "Exception handled by QSigExFlux2Events::Get\n";
    throw e;
  }

  return 0;
}

#include "debugger.h"
