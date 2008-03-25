#include "erfinv.h"

Double_t erfinv(Double_t p, Int_t& err)
{
  Double_t a,b,x,z,w,wi,sn,sd,f,z2,sigma;
  err=0;

  const Double_t a1=-.5751703, a2=-1.896513, a3=-.5496261E-1,
    b0=-.1137730, b1=-3.293474, b2=-2.374996, b3=-1.187515,
    c0=-.1146666, c1=-.1314774, c2=-.2368201, c3=.5073975E-1,
    d0=-44.27977, d1=21.98546, d2=-7.586103,
    e0=-.5668422E-1, e1=.3937021, e2=-.3166501, e3=.6208963E-1,
    f0=-6.266786, f1=4.666263, f2=-2.962883,
    g0=.1851159E-3, g1=-.2028152E-2, g2=-.1498384, g3=.1078639E-1,
    h0=.9952975E-1, h1=.5211733, h2=-.6888301E-1;

  x=p;
  sigma=TMath::Sign(1.0,x);

  if(x<=-1 || x>=1) x=sigma*(1.-1E-10);

  //  if(x>-1 && x<1){
    z=TMath::Abs(x);
    
    if(z>.85){
      a=1-z;
      b=z;
      //REDUCED ARGUMENT IS IN (.85,1.),
      //OBTAIN THE TRANSFORMED VARIABLE
      w=TMath::Sqrt(-TMath::Log(a+a*b));

      if(w>=4){
	
	//W GREATER THAN 4., APPROX. F BY A
	//RATIONAL FUNCTION IN 1./W
	wi=1./w;
	sn=((g3*wi+g2)*wi+g1)*wi;
	sd=((wi+h2)*wi+h1)*wi+h0;
	f=w+w*(g0+sn/sd);
	
      }else if(w>=2.5){
	//W BETWEEN 2.5 AND 4., APPROX. F
	//BY A RATIONAL FUNCTION IN W
	
	sn=((e3*w+e2)*w+e1)*w;
	sd=((w+f2)*w+f1)*w+f0;
	f=w+w*(e0+sn/sd);
	
      }else{
	//W BETWEEN 1.13222 AND 2.5, APPROX.
	//F BY A RATIONAL FUNCTION IN W
	
	sn=((c3*w+c2)*w+c1)*w;
        sd=((w+d2)*w+d1)*w+d0;
	f=w+w*(c0+sn/sd);
      }
      
    }else{
      //Z BETWEEN 0. AND .85, APPROX. F
      //BY A RATIONAL FUNCTION IN Z
      
      z2=z*z;
      f=z+z*(b0+a1*z2/(b1+z2+a2/(b2+z2+a3/(b3+z2))));
    }
    
    //FORM THE SOLUTION BY MULT. F BY
    //THE PROPER SIGN
    return sigma*f;
   
    //  }else{
    //    cout << "Wrong input value to erfinv function: " << x << "\n";
    //    err=1;
    //    return 1e99*sigma;
    //  }
}
