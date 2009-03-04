{

  gSystem->Load("libTree.so");
  gSystem->Load("libHist.so");
  gSystem->Load("libRIO.so");
  gSystem->Load("libMatrix.so");
  gSystem->Load("libqsigex-1.so");
  gSystem->Load("libqsigex-2.so");
  gEnv->SetValue("Root.Html.Root","http://root.cern.ch/root/html");

  THtml html;
  html.SetProductName("QSigEx2");
  html.MakeAll(kFALSE,"Q*");

}
