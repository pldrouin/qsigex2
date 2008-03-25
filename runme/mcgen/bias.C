{
Int_t ifile, nfile = 1;
TString name="results";
TString filename = name + ".root";
TString epsname_fit = name + "_fit.eps";
TString epsname_stat = name + "_stat.eps";
TString epsname_res = name + "_res.eps";
TString epsname_pull = name + "_pull.eps";
TString htit = " ";

  // fitted
  TH1D* hc1 = new TH1D("hc1",htit,120,230,390);
  TH1D* hc2 = new TH1D("hc2",htit,120,230,390);
  TH1D* hc3 = new TH1D("hc3",htit,120,230,390);
  // stat error
  TH1D* he1 = new TH1D("he1",htit,120,20,25);
  TH1D* he2 = new TH1D("he2",htit,120,20,25);
  TH1D* he3 = new TH1D("he3",htit,120,20,25);
  // residual
  TH1D* hr1 = new TH1D("hr1",htit,120,-120,120);
  TH1D* hr2 = new TH1D("hr2",htit,120,-120,120);
  TH1D* hr3 = new TH1D("hr3",htit,120,-120,120);
  // pull
  TH1D* hp1 = new TH1D("hp1",htit,120,-5,5);
  TH1D* hp2 = new TH1D("hp2",htit,120,-5,5);
  TH1D* hp3 = new TH1D("hp3",htit,120,-5,5);
  //
  TCanvas c("c","",0,0,600,600);
  c.Divide(1,3);
  cout << filename << endl;
  TFile f(filename,"READ");
  TTree *T = (TTree *)f.Get("btree");
  Double_t c1fit,c2fit,c3fit;
  Double_t c1err,c2err,c3err;
  T->SetBranchAddress("c1fit",&c1fit);
  T->SetBranchAddress("c2fit",&c2fit);
  T->SetBranchAddress("c3fit",&c3fit);
  T->SetBranchAddress("c1err",&c1err);
  T->SetBranchAddress("c2err",&c2err);
  T->SetBranchAddress("c3err",&c3err);
  int nevents = (int) T->GetEntries();
  for (int k=0; k<nevents; k++){
    T->GetEntry(k);
    //    cout << k << " " << c1fit << endl;
    hc1.Fill(c1fit);
    hc2.Fill(c2fit);
    hc3.Fill(c3fit);
    he1.Fill(c1err);
    he2.Fill(c2err);
    he3.Fill(c3err);
    hr1.Fill(c1fit-300);
    hr2.Fill(c2fit-300);
    hr3.Fill(c3fit-300);
    hp1.Fill((c1fit-300)/c1err);
    hp2.Fill((c2fit-300)/c2err);
    hp3.Fill((c3fit-300)/c3err);
  }
  //  cout << filename << endl;
  
  c.cd(1);
  hc1.Draw();
  hc1.GetXaxis()->SetTitle("Event Class 1");
  hc1.GetYaxis()->SetTitle("Number of Experiments");
  c.cd(2);
  hc2.Draw();
  hc2.GetXaxis()->SetTitle("Event Class 2");
  hc2.GetYaxis()->SetTitle("Number of Experiments");
  c.cd(3);
  hc3.Draw();
  hc3.GetXaxis()->SetTitle("Event Class 3");
  hc3.GetYaxis()->SetTitle("Number of Experiments");
  c.Print(epsname_fit);
  //
  c.cd(1);
  he1.Draw();
  he1.GetXaxis()->SetTitle("Stat error Class 1");
  he1.GetYaxis()->SetTitle("Number of Experiments");
  c.cd(2);
  he2.Draw();
  he2.GetXaxis()->SetTitle("Stat error Class 2");
  he2.GetYaxis()->SetTitle("Number of Experiments");
  c.cd(3);
  he3.Draw();
  he3.GetXaxis()->SetTitle("Stat error Class 3");
  he3.GetYaxis()->SetTitle("Number of Experiments");
  c.Print(epsname_stat);
  //
  c.cd(1);
  hr1.Draw();
  hr1.GetXaxis()->SetTitle("Fit-MC Class 1");
  hr1.GetYaxis()->SetTitle("Number of Experiments");
  c.cd(2);
  hr2.Draw();
  hr2.GetXaxis()->SetTitle("Fit-MC Class 2");
  hr2.GetYaxis()->SetTitle("Number of Experiments");
  c.cd(3);
  hr3.Draw();
  hr3.GetXaxis()->SetTitle("Fit-MC Class 3");
  hr3.GetYaxis()->SetTitle("Number of Experiments");
  c.Print(epsname_res);
  //
  c.cd(1);
  hp1.Draw();
  hp1.GetXaxis()->SetTitle("Pull Class 1");
  hp1.GetYaxis()->SetTitle("Number of Experiments");
  c.cd(2);
  hp2.Draw();
  hp2.GetXaxis()->SetTitle("Pull Class 2");
  hp2.GetYaxis()->SetTitle("Number of Experiments");
  c.cd(3);
  hp3.Draw();
  hp3.GetXaxis()->SetTitle("Pull Class 3");
  hp3.GetYaxis()->SetTitle("Number of Experiments");
  c.Print(epsname_pull);
  //
  c.Close();
  f.Close();
}


