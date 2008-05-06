// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExStdPDFs.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExStdPDFs                                                        //
//                                                                      //
// This class is a QSigExPDFs class that produces QDisTH and/or         //
// QDisTF marginal PDFs from TH* and/or TF* functions respective-       //
// ly. See QSigExPDFs documentation for more details.                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QSigExStdPDFs)

QDis* QSigExStdPDFs::GetFunction(const QList<TString>& pdfentry, TDirectory* fluxdir, const TCut& fgcuts, QList<TString>* inputs, Bool_t *pdfneedscuts)
{
  //This protected function uses the pdf card file formatted entry pdfentry to
  //create a function used in QSigExPDFs::Get() to produce a marginal PDF. The
  //input function can be a TH1F, TH1D, TF1, TH2F, TH2D or TF2 object.
  //GetFunction() returns a QDis pointer to an object of type QDisTH or QDisTF,
  //depending on the input function class. It fills inputs with the list of
  //coordinates in the PDF domain.  pdfneedscuts is set to kTRUE since the
  //function doesn't apply cuts on the output function.
  //
  //Syntax of a card file entry: pdf  [...]  [objname]  [filename]  [c1]  [c2]  [c3]
  //where [...] are the fields defined in QSigExPDFs::LoadCardFile, [objname]
  //the name of the object in the ROOT file [filename] and where [c1], [c2] and
  //[c3] are the coordinate names. [c2] is needed only by 2d and 3d functions
  //and [c3] only be 3d functions. The [typeid] field defined in QSigExPDFs must
  //be one of the ROOT class names that are recognized as input functions by
  //QSigExStdPDFs (TH* , TF*). If it's not the case, the pdf entry is ignored
  //and GetFunction() returns NULL.
  
  try{
      //Set up the indices for the fields in this part of the card file:
      const Int_t minfields = 7;         //minimum number of fields
      const Int_t pdftypeindex = 2;      //ROOT class name of the read function
      
      const Int_t objnameindex = 5 ;     //object name of the function
      const Int_t filenameindex = 6 ;    //ROOT filename
      const Int_t firstcoordindex = 7;   //First coordinate

      fluxdir=NULL;
      
      QDis* pdfbuf=NULL;
      
      //Create an instance of the appropriate PDF type
      if(pdfentry[pdftypeindex]=="TH1F" ||
	  pdfentry[pdftypeindex]=="TH1D"){
	//QDisTH1*
	CheckCardNFields(pdfentry.Count()-1,minfields,minfields);
	pdfbuf=new QDisTH(pdfentry[pdftypeindex],pdfentry[filenameindex],pdfentry[objnameindex]);
      } else if(pdfentry[pdftypeindex]=="TF1"){
	//QDisTF1
	CheckCardNFields(pdfentry.Count()-1,minfields,minfields);
	pdfbuf=new QDisTF(pdfentry[pdftypeindex],pdfentry[filenameindex],pdfentry[objnameindex]);
      } else if(pdfentry[pdftypeindex]=="TH2F" ||
	  pdfentry[pdftypeindex]=="TH2D"){
	//QDisTH2*
	CheckCardNFields(pdfentry.Count()-1,minfields,minfields+1);
	pdfbuf=new QDisTH(pdfentry[pdftypeindex],pdfentry[filenameindex],pdfentry[objnameindex]);
      } else if(pdfentry[pdftypeindex]=="TF2"){
	//QDisTF2
	CheckCardNFields(pdfentry.Count()-1,minfields,minfields+1);
	pdfbuf=new QDisTF(pdfentry[pdftypeindex],pdfentry[filenameindex],pdfentry[objnameindex]);
      } else if(pdfentry[pdftypeindex]=="TH3F" ||
	  pdfentry[pdftypeindex]=="TH3D"){
	//QDisTH2*
	CheckCardNFields(pdfentry.Count()-1,minfields,minfields+2);
	pdfbuf=new QDisTH(pdfentry[pdftypeindex],pdfentry[filenameindex],pdfentry[objnameindex]);
      } else if(pdfentry[pdftypeindex]=="TF3"){
	//QDisTF3
	CheckCardNFields(pdfentry.Count()-1,minfields,minfields+2);
	pdfbuf=new QDisTF(pdfentry[pdftypeindex],pdfentry[filenameindex],pdfentry[objnameindex]);
      }
      
      if(pdfbuf){
	pdfbuf->SetName(pdfentry[objnameindex]);
	inputs->Clear();
	for(Int_t i=firstcoordindex;i<pdfentry.Count();i++){
	  (*inputs)+=pdfentry[i]; 
	}
      }
      (*pdfneedscuts)=kTRUE;
      
      return pdfbuf;
  }catch(Int_t e){
    cout << "Exception handled by QSigExStdPDFs::GetFunction\n";
    throw e;
  }
}

const Char_t* QSigExStdPDFs::GetPDFName(Int_t i)
{
  const Int_t objnameindex = 5 ;

  try{
    return fPDFCard[i][objnameindex].Data();

  }catch(Int_t e){
    cout << "Exception handled by QSigExStdPDFs::GetPDFName\n";
    throw e;
  }
}

#include "debugger.h"





