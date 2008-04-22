{

  gSystem->Load("libTree.so");
  gSystem->Load("libHist.so");
  gSystem->Load("libRIO.so");
  gSystem->Load("libMatrix.so");
  gSystem->Load("libqsigex.so");
  gEnv->SetValue("Root.Html.Root","http://root.cern.ch/root/html");

  THtml html;
  html.MakeAll(kFALSE,"Q*");

}
