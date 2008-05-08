// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExPDFs.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExPDFs                                                           //
//                                                                      //
// This abstract base class uses a card file to create a set of         //
// marginal PDFs.  Binned/unbinned functions are loaded via derived     //
// class GetFunction member function (GetFunction is a pure virtual     //
// member function of QSigExPDFs) and returned to QSigExPDFs class.     //
// Then, cuts defined in the TDirectory structure are applied (or not,  //
// depending on the derived class) and the functions are normalized.    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QSigExPDFs)

void QSigExPDFs::FormatDir()
{
  //This protected member function gives to the fMyDir TDirectory the structure
  //that is needed to store the information produced by this class. It creates
  //directory PDFs into fMyDir.

  PRINTF2(this,"\tvoid QSigExPDFs::FormatDir()\n")
  try{
    //If 'PDFs' is neither in memory nor on Disk, create it in memory
    if(!(fPDFsDir=(TDirectory*)fMyDir->Get("PDFs"))){
      fPDFsDir=fMyDir->mkdir("PDFs","PDFs");
    }

  }catch(int e){
    cout << "Exception handled by QSigExPDFs::FormatDir\n";
    throw e;
  }
}

void QSigExPDFs::CleanDir()
{
  //This public member function reinitializes the part of the fMyDir directory
  //structure that belongs to QSigExPDFs. It removes the PDFs directories owned
  //by the class and calls FormatDir.

  PRINTF2(this,"\tvoid QSigExPDFs::CleanDir()\n")
  try{
    if((fPDFsDir=dynamic_cast<TDirectory*>(fMyDir->Get("PDFs")))){
      //Format fMyDir properly
      FormatDir();
      TList gdlist,sdlist,pdlist; //flux group, systematic groups and PDFs
      //directories lists 
      TDirectory *gdir,*sdir,*pdir;
      Int_t i,j,k; //iterators

      //Fill gdlist with the list of flux group directories in "PDFs" TDirectory
      GetDirs(&gdlist,fPDFsDir);
      //Loop over the flux group directories
      for(i=0;i<gdlist.GetSize();i++){
	//Get a flux group TDirectory pointer
	gdir=dynamic_cast<TDirectory*>(gdlist.At(i));
	//Fill sdlist with the list of systematic group directories in gdir
	GetDirs(&sdlist,gdir);
	//Loop over the systematic group directories
	for(j=0;j<sdlist.GetSize();j++){
	  //Get a systematic group TDirectory pointer
	  sdir=dynamic_cast<TDirectory*>(sdlist.At(j));
	  //Fill pdlist with the list of PDFs directories in sdir
	  GetDirs(&pdlist,sdir);
	  //Loop over the PDFs directories
	  for(k=0;k<pdlist.GetSize();k++){
	    //Get a PDF TDirectory pointer
	    pdir=dynamic_cast<TDirectory*>(pdlist.At(k));
	    //If there's an object/key in the PDF TDirectory which name is the name of this
	    if(FindObjKey(ClassName(),pdir)){
	      //Delete the PDF TDirectory (and syst. group if last PDF) (and flux
	      //group if last syst. group)
	      DelPDF(gdir->GetName(),sdir->GetName(),pdir->GetName());
	    }
	  }
	  //Clear the list of PDF TDirectory without deleting the objects
	  pdlist.Clear("nodelete");
	}
	//Clear the list of systematics TDirectory without deleting the objects
	sdlist.Clear("nodelete");
      }
      //Clear the list of flux groups TDirectory without deleting the objects
      gdlist.Clear("nodelete");
      GetDirs(&gdlist,fPDFsDir);
      if(!gdlist.GetSize()) DelObjsKeys("PDFs",fMyDir);
      gdlist.Clear("nodelete");
    }
    fPDFsDir=NULL;
  }catch(int e){
    cout << "Exception handled by QSigExPDFs::CleanDir\n";
    throw e;
  }
}

void QSigExPDFs::ClearCardBuf()
{         
  //This member function clears the internal variables used to store the
  //configuration information of this class in card file format 
            
  fPDFCard.Clear();
}

void QSigExPDFs::LoadCardFile(const Char_t* cardfilename)
{
  //This public member function reads the card file with filename cardfilename
  //and saves the information in QSigExPDFs internal member variables.
  //
  //For the sake of flexibility, the card file entry format is not entirely
  //defined by QSigExPDFs. Only the firt 5 fields, including the keyword, have a
  //fixed meaning. For documentation about the other fields, refer to the
  //GetFunction member function of the appropriate derived class.
  //
  //Syntax of card file entries: 
  //pdf  [flags]  [typeid]  [fgroup]  [sgroup]  [...]
  //where [typeid] is the identifier used by derived classes validate
  //the PDF type, where [fgroup] is the flux group of the PDF and [sgroup] its
  //systematic group.
  //
  //[flags] specifies different options related to PDF activation and normalization.
  //Flags can be combined using a bitwise "OR" operator. The following values are
  //implemented in QSigExPDFs:
  //
  //1: The PDF entry is active
  //2: The PDF should not be normalized
  //More significant bits are passed to the function QDis::Normalize. Please
  //refer to the documentation of the QDis derived classes.
  //
  //About the PDFs inputs:
  //Depending on the type of PDF, a pdf entry contains one ore more fields that
  //specify the inputs to the PDF. These fields can be of 2 types:
  //-They can be branch names of the data TTree
  //-They can be equivalence names
  //In both cases, for all QSigExPDFs derived classes that let QSigExPDFs
  //normalize the PDFs, it's IMPORTANT that all the cut expressions that contain
  //the PDFs input names don't contain ANY OTHER VARIABLE.

  PRINTF4(this,"\tvoid QSigExPDFs::LoadCardFile(const Char_t* cardfilename<",cardfilename,">)\n")
  try{

    //if cardfilename is not NULL
    if(cardfilename){
      //Set the QDatReader filename to cardfilename
      fReader.SetFilename(cardfilename);
      //Set the QDatReader keywork to "pdf"
      fReader.SetKeyword("pdf");   
      //Read all the pdf lines
      fPDFCard=fReader.GetMany(0);
    }
  }catch(int e){
    cout << "Exception handled by QSigExPDFs::LoadCardFile\n";
    throw e;
  }
}

Int_t QSigExPDFs::Get()
{
  //This function uses the PDF entries stored in the internal member variables
  //of this class to load, apply cuts, normalize and store a set of marginal
  //PDFs in a subfolder of "PDFs" TDirectory in the TDirectory structure. The
  //resulting PDFs are instances of a class derived from QDis. 
  //
  //Each PDF is located in a TDirectory having the same name than the PDF. This
  //TDirectory is stored in another TDirectory named according to the PDF
  //systematic group.  Finally, this systematic TDirectory is placed in another
  //TDirectory named according to the flux group of the PDF. 
  //
  //The function returns the number of PDFs that have been loaded 

  PRINTF2(this,"\tInt_t QSigExPDFs::Get()\n")

  //Set up the indices for the fields in this part of the card file:
  const Int_t minfields = 5;         //minimum expected number of fields

  const Int_t flagindex = 1;         //Active flag index
  const Int_t pdfgroupindex = 3;     //Flux group name
  const Int_t systnameindex = 4;     //Systematics group name

  try{
    FormatDir();

    //Check if the Cuts TDirectory exists
    CheckCuts();

    TDirectory* cutsdir=dynamic_cast<TDirectory*>(fMyDir->Get("Cuts"));

    //Loads the equivalence TDirectory in memory
    cutsdir->cd("Equivalences");
    TDirectory* eqdir=dynamic_cast<TDirectory*>(cutsdir->Get("Equivalences"));
    TList equiv;           //List of equivalences QNamedVar objects

    //Get the list of equivalences QNamedVar objects from the equivalence
    //TDirectory
    GetObjs(&equiv,eqdir);

    QList<TString> eqnames; //List of equivalences names
    QList<TString> eqformulas; //List of equivalences formulae
    TString strbuf; //TString buffers

    Int_t i,j; //Iterators

    //Loop over the equivalences
    for(j=0;j<equiv.GetSize();j++){
      
      //Add the equivalence name
      eqnames+=dynamic_cast<QNamedVar<TString>*>(equiv.At(j))->GetName();
      
      //Add the equivalence expression
      eqformulas+=dynamic_cast<QNamedVar<TString>*>(equiv.At(j))->GetValue();
    }
    //Clear the list of equivalences QNamedVar objects and delete these
    //objects from memory
    equiv.Clear();
    
    TList ccuts;         //TCut objects that will contain combined cuts
    TCut  allcuts;       //All the cuts

    //Change directory to "Cuts Expressions"
    cutsdir->cd("Cuts Expressions");

    TList cuts; //List of simple cuts

    //Get the list of cuts QNamedVar objects from the equivalence TDirectory
    GetObjs(&cuts,gDirectory);

    TCut* buf; //TCut buffer

    //Loop over the cuts
    for(i=0;i<cuts.GetSize();i++){
      //Read the cut expression
      buf=new TCut(dynamic_cast<QNamedVar<TString>*>(cuts.At(i))->GetValue());
      //Add it to allcuts
      allcuts+=(*buf);
      //Add the cut to the list of combined cuts
      ccuts.Add(buf);
    }
    //Clear the list of cuts QNamedVar objects and delete these objects from
    //memory
    cuts.Clear();

    TIter iter(&ccuts); //Combined cuts iterator

    Int_t actbuf;        //Buffer for the act flag
    TDirectory* fluxdir; //Flux TDirectory pointer
    TDirectory* systdir; //Systematics TDirectory Pointer
    TDirectory* pdfdir;  //PDF TDirectory pointer
    TDirectory* inputspdir; //Inputs TDirectory pointer
    QList<TString> inputs;
    Int_t ninputs;       //Number of inputs
    QDis* pdfbuf;        //QDis buffer
    Bool_t pdfneedscuts;

    TObject* nbuf;      //QNamedVar buffer

    Double_t buf1,buf2,buf3;   //Float_t buffers

    TString cutsexpr;    //A cuts TString buffer

    TCut pdfcuts;        //Cuts for a specific pdf
    const Char_t* labels[3]={"x","y","z"}; //variable names for QDis* objs
    QList<Int_t> eqfound; //Buffer for matching equivalences

    //Loop over the pdf lines in the card file
    for(i=0;i<fPDFCard.Count();i++){

      CheckCardNFields(fPDFCard[i].Count(),minfields);

      //Store the act flag in actbuf
      sscanf(fPDFCard[i][flagindex].Data(),"%i",&actbuf);

      //If the PDF is activated
      if(actbuf & kDisAct){
	//initialize fluxdir and systdir pointers
	fluxdir=NULL;
	systdir=NULL;
	
	//If the TDirectory of the current flux group doesn't exist, create
	//it
	if(!(fluxdir=(TDirectory*)fPDFsDir->Get(fPDFCard[i][pdfgroupindex]))){
	  fluxdir=fPDFsDir->mkdir(fPDFCard[i][pdfgroupindex],
				 fPDFCard[i][pdfgroupindex]);
	}

	//If the TDirectory of the current systematic group doesn't exist,
	//create it
	if(!(systdir=(TDirectory*)fluxdir->Get(fPDFCard[i][systnameindex]))){
	  systdir=fluxdir->mkdir(fPDFCard[i][systnameindex],
				 fPDFCard[i][systnameindex]);
	}
	
	inputs.Clear();
	//Load the function and its inputs. If the function needs cuts,
	//pdfneedscuts is set to kTRUE
	pdfbuf=GetFunction(fPDFCard[i],fluxdir,allcuts,&inputs,&pdfneedscuts);

	if(pdfbuf){
	  //Create a TDirectory for the pdf
	  pdfdir=systdir->mkdir(pdfbuf->GetName());

	  //Add class identifier in pdf directory;
	  nbuf=new QNamedVar<TString>(ClassName(),"Class Owner Identifier");
	  pdfdir->Add(nbuf);
	  
	  //Create an "Inputs" directory for the pdf
	  inputspdir=pdfdir->mkdir("Inputs");
	  
	  ninputs=inputs.Count();
	  
	  //Loop over the coordinates in card file
	  for(j=0;j<ninputs;j++){
	    //Create a new QNamedVar object to contain the Input
	    nbuf=new QNamedVar<TString>(((TString)"Input ")+(long int)(j),inputs[j]);
	    //Add the Input object to the Input TDirectory
	    inputspdir->Add(nbuf);
	    //Increment the number of inputs
	  }
	  
	  //Initialize the PDFs cuts to TRUE
	  pdfcuts="1";
	  
	  if(pdfneedscuts){
	    //Reset the iterator
	    iter.Reset();
	    
	    //Loop over the single cuts
	    while((buf=(TCut*)iter())){
	      //Copy the current cut into strbuf
	      strbuf=buf->GetTitle();
	      
	      //Loop over the number of inputs in the PDF
	      for(j=0;j<ninputs;j++){
		//If the current cut contains the input name, add it to the
		//list of cuts for this PDF
		if(QFormulaUtils::IndexVar(strbuf,((QNamedVar<TString>*)inputspdir->GetList()->
			At(j))->GetValue())!=-1) pdfcuts+=(*buf);
	      }
	    }
	  }

	  //Copy the cuts into cutsexpr
	  cutsexpr=pdfcuts;
	  
	  //Loop over the inputs
	  for(j=0;j<ninputs;j++){
	    //Replace the input variables names by x,y,z
	    QFormulaUtils::ReplaceVar(&cutsexpr,((QNamedVar<TString>*)inputspdir->GetList()->At(j))->GetValue(),labels[j]);
	    //For the inputs, replace the equivalences names  by their
	    //expression
	    if((eqfound=eqnames.Find(((QNamedVar<TString>*)inputspdir->GetList()->At(j))
				     ->GetValue())).Count())
	      *((QNamedVar<TString>*)inputspdir->GetList()->At(j))=eqformulas[eqfound[0]];
	  }
	  
	  //Normalize the PDF using the cuts and according to what indicated by the act flag
	  if(!(actbuf & kDisSkipNorm)){
	    cout << "\nNormalize pdf " << pdfbuf->GetName() << "\n";
	    pdfbuf->SetCutExpr(cutsexpr);
	    pdfbuf->SetNormFlags(actbuf>>2);
	    pdfbuf->Normalize(&buf1,&buf2,&buf3);
	    cout << "Full integral: " << buf1 << "\n";
	    if(buf3==0){
	      cout << "Normalization integral: " << buf2 << "\n";
	    }else{
	      cout << "Normalization integral: " << buf2 << " (error: -" << buf3/buf2*100 << " %)\n";
	    }
	  }else{
	    cout << "\nThe pdf " << pdfbuf->GetName() << " WILL NOT be normalized\n";
	  }
	  
	  //Add the PDF to pdfdir
	  pdfdir->Add(pdfbuf);
	}
      }
    }
    
    //Delete the combined cuts 
    ccuts.Delete();

    //Return the number of loaded PDFs
    return fPDFCard.Count();
  
  }catch(Int_t e){
    cout << "Exception handled by QSigExPDFs::Get\n";
    throw e;
    }
}

void QSigExPDFs::DelPDF(const Char_t* fgroup, const Char_t* sgroup, const Char_t* pdfname)
{
  //This function removes a PDF from the TDirectory structure and from the card file entries in the
  //internal member variables, given its flux group, its systematics groups and its name
  
  TDirectory *fgdir,*sgdir;
  TList listbuf; //TList buffer

  Int_t i;
  const Int_t pdfgroupindex = 3;     //Flux group name
  const Int_t systnameindex = 4;     //Systematics group name

  TString strpdfname=pdfname;

  for(i=0;i<fPDFCard.Count();i++){

    if(!fPDFCard[i][pdfgroupindex].CompareTo(fgroup) && !fPDFCard[i][systnameindex].CompareTo(sgroup) && !strpdfname.CompareTo(GetPDFName(i))){
      fPDFCard.Del(i);
      i--;
    }
  }

  //If there's no fPDFsDir TDirectory, return
  if(!(fPDFsDir=dynamic_cast<TDirectory*>(fMyDir->Get("PDFs")))) return;

  //If there's no flux group or systematics group TDirectory corresponding
  //to the arguments given, return
  if(!(fgdir=dynamic_cast<TDirectory*>(fPDFsDir->Get(fgroup)))) return;
  if(!(sgdir=dynamic_cast<TDirectory*>(fgdir->Get(sgroup)))) return;

  //Delete all the objects corresponding to pdfname in the systematics
  //group
  DelObjsKeys(pdfname,sgdir);
  //Load the list of remaining TDirectory objects in the systematics group
  //TDirectory
  GetDirs(&listbuf,sgdir);
  //If the list is empty, delete the systematics group
  if(!listbuf.GetSize()) DelObjsKeys(sgroup,fgdir);
  //Clear the list without deleting objects
  listbuf.Clear("nodelete");
  //Load the list of remaining TDirectory objects in the flux group
  //TDirectory
  GetDirs(&listbuf,fgdir);
  //If the list is empty, delete the flux group
  if(!listbuf.GetSize()) DelObjsKeys(fgroup,fPDFsDir);
  //Clear the list without deleting objects
  listbuf.Clear("nodelete");
}

void QSigExPDFs::AddPDF(Int_t flags, const Char_t* type, const Char_t* fgroup, const Char_t* sgroup, const Char_t* param1, const Char_t* param2, const Char_t* param3, const Char_t* param4, const Char_t* param5, const Char_t* param6, const Char_t* param7, const Char_t* param8)
{
  //This function adds a PDF entry in the internal member variables. The format of the arguments is descibed in QSigExFit::LoadCardFile.

  TString flagsstr;
  flagsstr+=flags;

  QList<TString> entry;
  entry+="pdf";
  entry+=flagsstr;
  entry+=type;
  entry+=fgroup;
  entry+=sgroup;
  if(param1) entry+=(TString)param1;
  if(param2) entry+=(TString)param2;
  if(param3) entry+=(TString)param3;
  if(param4) entry+=(TString)param4;
  if(param5) entry+=(TString)param5;
  if(param6) entry+=(TString)param6;
  if(param7) entry+=(TString)param7;
  if(param8) entry+=(TString)param8;
  fPDFCard+=entry;
  entry.Clear();
}

void QSigExPDFs::CheckCuts() const
{
  //Check if the TDirectories "Cuts Expressions" and "Equivalences" exist

  TDirectory* cutsdir=dynamic_cast<TDirectory*>(fMyDir->Get("Cuts"));

  if(!cutsdir || !(cutsdir->FindObject("Cuts Expressions") ||
		   cutsdir->FindKey("Cuts Expressions")) ||
     !(cutsdir->FindObject("Equivalences") ||
       cutsdir->FindKey("Equivalences"))) {
    cout <<"Problem with cuts:  cuts not input correctly in card file\n"
         <<"or ReadCuts() was not called before it was needed\n";
    throw 1;
  }
}

#include "debugger.h"





