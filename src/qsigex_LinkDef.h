#ifdef __CINT__
#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class QList<Bool_t>;
#pragma link C++ class QList<Int_t>;
#pragma link C++ class QList<Float_t>;
#pragma link C++ class QList<Double_t>;
#pragma link C++ class QList<TString>;
#pragma link C++ class QList<TObject*>;
#pragma link C++ class QList<QList<TString> >;
#pragma link C++ class QList<QList<Int_t> >;
#pragma link C++ class QList<QList<Double_t> >;

#pragma link C++ class QSigExProbs<Float_t>;
#pragma link C++ class QSigExProbs<Double_t>;

#pragma link C++ class QDatReader;
#pragma link C++ class QFormulaUtils;
#pragma link C++ class QProgress;
#pragma link C++ class QSigExCleanData;
#pragma link C++ class QSigExCuts;
#pragma link C++ class QSigExDirHandler;
#pragma link C++ class QSigExDis;
#pragma link C++ class QSigExDisTF;
#pragma link C++ class QSigExDisTH;
#pragma link C++ class QSigExFit;
#pragma link C++ class QSigExFitDataHolder;
#pragma link C++ class QSigExFlux2Events;
#pragma link C++ class QSigExF2EDataHolder;
#pragma link C++ class QSigExGCJointProbs;
#pragma link C++ class QSigExGaussCor;
#pragma link C++ class QSigExIO-;
#pragma link C++ class QSigExJointProbs;
#pragma link C++ class QSigExPDFs;
#pragma link C++ class QSigExStaticList;
#pragma link C++ class QSigExStdPDFs;
#pragma link C++ class QSigExTFOps;
#pragma link C++ class QSigExTHOps;
#pragma link C++ class QSigExTTreePDF;
#pragma link C++ class QTTreeUtils;
#pragma link C++ class QSigExChecks;
#pragma link C++ class QIdxHashList;
#pragma link C++ class QSigExStruct-;
#pragma link C++ class QNamedVar<TString>;
#pragma link C++ class QNamedVar<UShort_t>;
#pragma link C++ class QNamedVar<Int_t>;
#pragma link C++ class QNamedVar<Float_t>;
#pragma link C++ class QNamedVar<Double_t>;

#pragma link C++ function QExtendedLikelihood;
#pragma link C++ function QF2EExtendedLikelihood;
#pragma link C++ function QSigExGaussMapping;
#pragma link C++ function erfinv;
#endif
