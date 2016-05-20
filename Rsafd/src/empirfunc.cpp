#include<stdio.h>
#include<math.h>
#include<errno.h>
#include <iostream>
#include <stdlib.h>
#include "Array.h"

using namespace std;
using std::string;

extern "C" {

void empirfunc(double *x,  double *breaks, int *nx, int *nbreaks, int *indexes) 
{
  int n2;
  int N;
  int curindX;

  N = *nbreaks;
  n2 = *nx;
  cout << "Total length of the array  "<<N<<endl; 
  
  curindX = 0; 
 
  for (int j = 0; j < N; j++) 
  {
	    if (breaks[j] > x[curindX]) 
		    while ((curindX < n2) && (breaks[j] > x[curindX]) ) 
			{
		        indexes[curindX] = j;
		        curindX++;
			}
        if (curindX >= n2) break;
  }
}
}


void empircop(int *n, int *cnn, int *value) 
{
  int N;

  N = *n;

  cout << "Total length of the array  "<<N<<endl; 

  value[0] = cnn[0];

  for (int j = 1; j < N; j++) 
    value[j] = value[j - 1] + cnn[j];

  for (int i = 1; i < N; i++)
    value[i*N] = value[i*N - N] + cnn[i*N];
  
  for (int i = 1; i < N; i++)  
	  for (int j = 1; j < N; j++)
        value[i*N+j] = value[i*N+j-1] + value[i*N - N +j]
          + cnn[i*N + j] - value[i*N + j - 1 - N];
}  

extern "C" {
	
void interpcopR(double *xsort, double *ysort, int *ymatind, int *sampsize,
			   double *XX , double *YY, int *xlength, double *value) 
{
   int Nsamp;
   int numpoints;
   int done;
   double x,y,f;
   int count;
    double f_k_l, f_k1_l, f_k1_l1, f_k_l1;
	int I,J;
     double c;
	 double N;
	 int Nmax;
   
    Nsamp = *sampsize;
    numpoints = *xlength;
    N = 1.0*(Nsamp - 2);
	c = 1.0/N;
   
	for (int i = 0; i < numpoints; i++) 
	{
	    x = XX[i];
		y = YY[i];
        count = 0; 
		/*
		determine value of the imperical copula 
		t the four corners:
		*/
        f_k_l = 0;
		f_k1_l = 0;
		f_k_l1 = 0;
		f_k1_l1 = 0;
	
		I = (long) floor(x*N);
        J = (long) floor(y*N); 

		Nmax = I + 1;
		if (Nmax < J + 1) Nmax = J + 1;
        
        /**
		* we have to determine that I and J are not x and 1
		*/
        done = 0; 
        
		if (I < 0 || I > Nsamp - 1 || J < 0 || J > Nsamp - 1) 
		{	
			fprintf(stderr,"Problem in I and J: I %d   J %d  ",I,J);
			//exit(1);
		}
      
		if (I == N+1 && J == N+1)
		{
		    f =  1.0;
		}	 
        else if (I == N+1) 
		{
			f_k_l = 0;
			f_k_l1 = 0;
			for (int j = 1; j < Nmax ; j++) 
			{
			   if (ysort[ymatind[j]] <= ysort[J]) 
			   {
			      f_k_l++; f_k_l1++;
			   }
			   else if (ysort[ymatind[j]] <= ysort[J + 1]) {
			       f_k_l1++;  
			   }			  
			}
            f = f_k_l * ((J + 1)*c - y) + f_k_l1 * (y - J*c);
            f /= c;
		}
        else if (J == N+1) 
		{
			f_k1_l = 0;
			f_k_l = 0;
			for (int j = 1; j < Nmax; j++) {
			   if (xsort[j] <= xsort[I]) {
			      f_k_l++; f_k1_l1++;  
			   }
			   else if (xsort[j] <= xsort[I + 1]) {
			       f_k1_l++;  
			   }
			}
            f = f_k_l * ((I + 1)*c - x) + f_k1_l * (x - I*c);
            f /= c;
		}
        else 
		{
			for (int j = 1; j < Nmax; j++) 
			{
			   if (xsort[j] <= xsort[I] && ysort[ymatind[j]] <= ysort[J]) 
			   {
				  f_k_l++; f_k1_l++; f_k_l1++; f_k1_l1++;
			   }
			   else if (xsort[j] <= xsort[I] && ysort[ymatind[j]] <= ysort[J+1]) 
			   {
					f_k_l1++; f_k1_l1++;
			   }
	    		else if (xsort[j] <= xsort[I+1] && ysort[ymatind[j]] <= ysort[J]) 
				{
					f_k1_l1++; f_k1_l++;
				}
	    		else if (xsort[j] <= xsort[I+1] && ysort[ymatind[j]] <= ysort[J+1]) 
				{
					f_k1_l1++; 
				}
			}
			  
			f = 0.0;
			f += f_k_l*(x - (I+1)*c)*(y - (J+1)*c);
			f += -f_k1_l*(x - I*c)*(y - (J+1)*c);
			f += f_k1_l1*(x - I*c)*(y - J*c);
			f += -f_k_l1*(x - (I+1)*c)*(y-J*c);
			f /= c*c; 
		}
        value[i] = (double) f/N; 
	}
}
}

void get2peaks (double *T0s, double *V0s, double *Xs, double *Ys,
				int *sampSize, double *delta, int *T0l, int *notOK)
{
	int j, jj;
	int num_one_peak;
	int current_year;
	double maxX, secMax;
	double q;
	int sampleSize, T0slength;

    sampleSize = *sampSize;
	T0slength = *T0l;
	q = *delta;
	j = 0;
    jj = 0;
	current_year =(int) floor(T0s[j]); 			  
	maxX = -9999.0;
	secMax = -9999.0; 			   			  
	num_one_peak = 0;  

	while(jj < sampleSize) 
	{ 			       				  
		 if (j < T0slength && T0s[j] >= current_year && T0s[j] < current_year + 1) 
		 { 				     
			   if (V0s[j] > maxX) 
			   {
					secMax = maxX; 				       
					 maxX = V0s[j];
				}
			    else if (V0s[j] > secMax) secMax = V0s[j];
					 j++; 			      
	      }
		 else 
		 {     
			 Xs[jj] = maxX;
			 Ys[jj] = secMax;
					//	   if (maxX < q) Xs[jj] = -999;
					//	   if (secMax < q) Ys[jj] = -999;
		      jj++;  						 
			  if (j < T0slength) current_year = (int)floor(T0s[j]);  				  
                      if (jj !=  current_year && j < T0slength && jj < sampleSize) 
                    	  *notOK = 1 ;
			  maxX = -999;
			  secMax = -999;
		   }
	  }			    			  	     	
}




/****************************************************
****************************************************
***   Functions used to invert derivatives of Phi  ****
***     for Archemidian copula     ********************
******************************************************
*/



double PhiPrimeBB1(double u, double delta, double theta) 
{   
	return(-(delta*theta* pow(u,(-1 - theta))*pow((-1 + pow(u,(-theta))),(-1 + delta))));
}

double PhiPrimePrimeBB1(double u, double delta, double theta) 
{
   double put2;
   double put;

   put2 = pow(u,-2 - 2*theta);
   put = pow(u,-theta);

  return((-1 + delta)*delta*theta*theta*put2*
    pow(-1 + put,-2 + delta) - 
     delta*(-1 - theta)*theta*pow(u,-2 - theta)*
      pow(-1 + put,-1 + delta));
}


double PhiPrimeBB2(double u, double delta, double theta) {
     double put;
	 put = pow(u,-theta - 1);

	return ( -(delta*exp(delta*(-1 + put*u))*theta*
     put));

}

double PhiPrimePrimeBB2(double u, double delta, double theta) {
   double put2;
   double put;

   put2 = pow(u,-2 - 2*theta);
   put = pow(u,-theta);

  return(
	  delta*delta*exp(delta*(-1 + put))*
    theta*theta*put2 - 
   delta*exp(delta*(-1 + put))*(-1 - theta)*
    theta*put/u/u );
}


double PhiPrimeBB3(double u, double delta, double theta) {
     double lu;
	 double put;

	 lu = log(u);
	 put = pow(-lu,theta);

	return ((delta*exp(delta*put)*theta*put)/(u*lu));

}

double PhiPrimePrimeBB3(double u, double delta, double theta) {
  
     double lu;
	 double put;

	 lu = log(u);
	 put = pow(-lu,theta);

  return((delta*exp(delta*put)*theta*
     (-1 + theta + delta*theta*put - 
       lu)*put/lu/lu)/u/u);
}



double PhiPrimeBB6(double u, double delta, double theta) {
     double ubar;
	 double put;

	 ubar = 1 - u;
	 put = pow(ubar,theta);

	return ( -((delta*theta*put/ubar*
       pow(-log(1 - put),-1 + delta))/
     (1 - put)));

}

double PhiPrimePrimeBB6(double u, double delta, double theta) {
  
     double ubar;
	 double put;

	 ubar = 1 - u;
	 put = pow(ubar,theta);


  return(  (delta*theta*put/ubar/ubar*
       pow(-log(1 - put),-2 + delta)*
     ((-1 + delta)*theta*put - 
       (-1 + theta + put)*
        log(1 - put)))/
            ((-1 + put) *(-1 + put)) );
}





double PhiPrimeBB7(double u, double delta, double theta) {
     double ubar;
	 double put;

	 ubar = 1 - u;
	 put = pow(ubar,theta);

	return ( -(delta*theta*
		 pow(1 - put,-1 - delta)*
     put/ubar) );

}

double PhiPrimePrimeBB7(double u, double delta, double theta) {
  
     double ubar;
	 double put;

	 ubar = 1 - u;
	 put = pow(ubar,theta);

  return( 
	  delta*theta*pow(1 - put,-2 - delta)*
   (-1 + theta + put + 
     delta*theta*put)*
      put/ubar/ubar	  
	  );
}


double PhiPrimeGumbel(double u, double delta) {
      double put;

 	 put = pow(-log(u),delta-1);

	return (  -delta*put/u);

}

double PhiPrimePrimeGumbel(double u, double delta) {
  
     double ubar;
	 double put;

	 ubar = -log(u);
	 put = pow(ubar,delta-2);

  return( delta*((-1+delta)*put+put*ubar)/u/u );
}



double  KCGumbel(double u, double delta) {
     ;
 	return (   u - u*log(u)/delta);

}

double PrimeKCGumbel(double u, double delta) {
  
     double ubar;
	 ubar = -(log(u)+1)/delta;

  return( ubar +1);
}



/*
* This function computed the value of inverse pf phi'
* It solves equations like
*   Phi'(result) = inX
*   lengthX is the length of arra inX
*   delta and theta are copula parameters
*  family is the code for copula family: I == family BBI
*  eX and eF are precision in X and function values
*  maxit is maximum of iterations and converged is array
* that shows for which x solution converged 
* ( > 0 means converged)
*/
extern "C" {
void inverseDerivPhiBB(double *inX, int *lengthX, 
						  double *delta, double *theta, 
						  double *result, int *family, double* eX, 
						   double *eF,
						  int *maxit, int *converged) {

   int N;
   double errorX, errorF;
   double valF = 100;
   double curU;
   double nextU;
   double d;
   double t;
   int nX;
   double x;

   nX = *lengthX;

   N = 0;
   errorX = 1.0;
   errorF = 1.0;
   curU = 0.5;
   d = *delta;
   t = *theta;
  /* printf("  %f   %f \n", d,t); */
   
   for (int i = 0; i < nX; i++) {     
	 x = inX[i];     
	 while ((errorX > *eX) && (N < *maxit) && (errorF > *eF)) {
       N++;
       nextU = -999;
       valF = 100;
	   switch(*family) {
	     case 1:
			    valF = PhiPrimeBB1(curU,d,t) - x;
	            nextU = curU - (valF)/PhiPrimePrimeBB1(curU,d,t);
	            break;
		 case 2:
			    valF = PhiPrimeBB2(curU,d,t) - x;
	            nextU = curU - (valF)/PhiPrimePrimeBB2(curU,d,t);
	            break;
         case 3:
                valF = PhiPrimeBB3(curU,d,t) - x;
			    nextU = curU - (valF)/PhiPrimePrimeBB3(curU,d,t);
	            break;
         case 6:			    
			    valF = PhiPrimeBB6(curU,d,t) - x;
			    nextU = curU - (valF)/PhiPrimePrimeBB6(curU,d,t);
	            break;
		 case 7:			    
			    valF = PhiPrimeBB7(curU,d,t) - x;
			    nextU = curU - (valF)/PhiPrimePrimeBB7(curU,d,t);
	            break;
		 case 8: valF = PhiPrimeGumbel(curU,d) - x;
                 nextU = curU - (valF)/PhiPrimePrimeGumbel(curU,d);
				 break;        
		 case 9: valF = KCGumbel(curU,d) - x;
                 nextU = curU - (valF)/PrimeKCGumbel(curU,d);
				 break;
		 default: 
			    N = *maxit + 10;
			    nextU = curU;
	   }

       if (nextU < 0) nextU = curU/2.0;
       else if (nextU > 1) nextU = (1 - curU)/2.0 + curU;
	   errorX = fabs(curU - nextU);
       errorF = fabs(valF);
     
	   curU = nextU; 
	 }
  
      if (N >= *maxit) converged[i] = 0;
	  else if (errorX < *eX && errorF < *eF) 
		   converged[i]  = 1;
      else if (errorX < *eX)
		   converged[i] = 2;
	  else converged[i] = 3;
	  
	//  printf(" %f  %f  in %d iterations \n", x, curU, N);
     N = 0;
	 result[i] = curU;
	 errorX = 1.0; 
     errorF = 1.0;
   }
}
}

/***************************************************************/

void CondB6Prob (double u, double v, double w, double a, double b,
				 double *value, double *grad) 
{
        double put, pvt, pwt, Tterm,  
			 ptalpha,  sumterm;

		double val, deriv;

		put = pow(-log(u),b);
		pvt = pow(-log(v),b);
		pwt = pow(-log(w),a);
		Tterm = pow((put+pvt), 1/b);
        ptalpha = pow(Tterm,a);
        sumterm = pow(ptalpha+pwt,1/a);
    
        val = (exp(-sumterm + Tterm)*pow(ptalpha + pwt,-2 + 1/a)*
           pow(Tterm,-2 + a)*
     ((-1 + b)*(ptalpha + pwt)*Tterm - 
       (-pwt + a*pwt - ptalpha*sumterm)*Tterm))/
          (-1 + b + Tterm);

         deriv =  (exp(-sumterm + Tterm)*pow(ptalpha + pwt,-3 + 1/a)*
        pow(Tterm,-2 + a)*
       ((-1 + b)*(ptalpha + pwt)*(-1 + a + sumterm)*Tterm - 
          (pwt + pow(a,2)*(-ptalpha + pwt) - 
          ptalpha*pow(ptalpha + pwt,2/a) + 
          2*ptalpha*sumterm - pwt*sumterm + 
          a*(ptalpha - 2*pwt - 2*ptalpha*sumterm + 
             pwt*sumterm))*Tterm)*pow(-log(w),-1 + a))/
                ((-1 + b + Tterm)*w);

       *value = val;
	   *grad = deriv;
}


/*
* X assumed to be sorted
 */

void getRandomW_B6(double *inQ, int *lengthQ,
					    double *inU, double *inV,
						  double *delta1, double *delta2, 
						  double *result, double* eX, 
						   double *eF,
						  int *maxit, int *converged) 
{
   int N;
   double errorX, errorF;
   double curW;
   double nextW;
    int nX,i;
   double x,nextValue, nextGrad;
   double u, v, a,b;
   double valF = 100;

   nX = *lengthQ;
   a = *delta1;
   b = *delta2;

   N = 0;
   errorX = 1.0;
   errorF = 1.0;
   curW = 0.5;
   
   for (i = 0; i < nX; i++) 
   {   
	 x = inQ[i];
     u = inU[i];
	 v = inV[i];
      curW = 0.5;
	 
	  while ((errorX > *eX) && (N < *maxit) && (errorF > *eF)) {
       N++;
       nextValue = -999;
	   nextGrad = -999;
       valF = 100;
       CondB6Prob (u,v,curW,a,b,&nextValue,&nextGrad);
        
	   valF =  nextValue - x;
	   nextW = curW - (valF)/nextGrad;
	           	
       if (nextW < 0) nextW = curW/2.0;
       else if (nextW > 1) nextW = (1 - curW)/2.0 + curW;
	   
	   errorX = fabs(curW - nextW);
       errorF = fabs(valF);     

	   curW = nextW;
	 }
  
      if (N >= *maxit) converged[i] = 0;
	  else if (errorX < *eX && errorF < *eF) 
		   converged[i]  = 1;
      else if (errorX < *eX)
		   converged[i] = 2;
	  else converged[i] = 3;
     N = 0;
	 result[i] = curW;
	 errorX = 1.0; 
     errorF = 1.0;
   }
}


/**************************************************************/
extern "C" {
void jointDensity(double *xData, double *yData, int *sampsize,
			      double *xPlot , double *yPlot, int *xlength, int *ylength,
			      double *value, double *xwin, double *ywin) 
{
   int j,k;
   int Nsamp;
     
   int xn, yn;
   double x,y;
   double xw, yw;
 	 double N;
 
    Nsamp = *sampsize;
    xn = *xlength;
	yn = *xlength;

    xw = *xwin;
	yw = *ywin;

    N = 1.0*(Nsamp - 2);
 
	for (int i = 0; i < Nsamp; i++) {
         x = xData[i];
		 y = yData[i];
         j = 0; k = 0;
		 while(x > xPlot[j] - xw && j < xn) j++;
	//	 if (j == xn) continue;
         while(y > yPlot[k] - yw && k < yn) k++;
   //		 if (k == yn) continue;
		 k--;
		 j--;
         value[k*xn + j] = value[k*xn + j] + 1; 
	}
}
}

extern "C" {

void rho_EC(double *xsort, double *ysort, int *ymatind, int *sampsize,
			     double *rho) {
   int n;
   n = *sampsize;
   Array<int,2> Cn(n,n);
    double sum;

    for(int i = 0; i < n; i++)
		 for (int j = 0; j < n; j++)
		 {
		    Cn(i,j) = 0;
         }
    for(int i = 0; i < n; i++)
			 Cn(i,ymatind[i]) = 1;
    
	Cn(0,0) = Cn(0,0);
    for (int j = 1; j < n; j++) 
		Cn(0,j) = Cn(0,j-1) + Cn(0,j);
    for (int i = 1; i < n; i++) 
		Cn(i,0) = Cn(i-1,0) + Cn(i,0);

    for(int i = 1; i < n; i++)
		 for (int j = 1; j < n; j++)
            Cn(i,j) = Cn(i,j-1) + Cn(i-1,j) + Cn(i,j)- Cn(i-1,j-1);

   sum = 0.0;
   for(int i = 0; i < n; i++)
		 for (int j = 0; j < n; j++)
              sum += double(Cn(i,j))/double(n) - double( (i + 1)* (j+1)) /double (n * n);

	*rho = sum * 12.0 /double(n*n - 1.0); 
    cout << *rho << endl;
}
}

extern "C" {

void tau_EC(double *xsort, double *ysort, int *ymatind, int *sampsize, double *tau)
{
   int j;
   int n;
    
   double sum;     
    n = *sampsize; 
    sum = 0.0;

    for (int i = 1; i < n; i++)
    {	     
		j = ymatind[i];	
		for (int p = 0; p < i - 1; p++) 
		{
		   if (ymatind[p] < j ) sum++;
		   else sum--;
		}
	}
		
	*tau = sum*2.0/double(n - 1.0)/double(n);
	cout << *tau << endl;
}
}

int main() {
    double xsort[10];
    double ysort[10] ;
    int matind[10];
	int Nsamp;
	double XX[10];
	double YY[10];
	int n ;
	double value[10];
    int i;

    xsort[0] = 0;  
	xsort[1] = 0.00624006055295467;  
	xsort[2] = 0.193711566273123;  
	xsort[3] = 0.359216987155378;  
	xsort[4] = 0.435177967883646;  
	xsort[5] = 0.554811547044665;  
	xsort[6] = 0.67206845479086;  
	xsort[7] = 0.744113290216774;  
	xsort[8] = 0.774564654566348;  
	xsort[9] = 0.795486922375858;  
	xsort[0] = 0.981292782351375;  
	xsort[1] = 1;  

	ysort[0] = 0;  
	ysort[1] = 0.00624006055295467;  
	ysort[2] = 0.193711566273123;  
	ysort[3] = 0.359216987155378;  
	ysort[4] = 0.435177967883646;  
	ysort[5] = 0.554811547044665;  
	ysort[6] = 0.67206845479086;  
	ysort[7] = 0.744113290216774;  
	ysort[8] = 0.774564654566348;  
	ysort[9] = 0.795486922375858;  
	ysort[0] = 0.981292782351375;  
	ysort[1] = 1;  

	matind[0] = 0;  
	matind[1] = 8;  
	matind[2] = 7;  
	matind[3] = 1;  
	matind[4] = 4;  
	matind[5] = 10;  
	matind[6] = 5;  
	matind[7] = 3;  
	matind[8] = 6;  
	matind[9] = 9;  
	matind[0] = 2;  
	matind[1] = 11;  

	XX[0] = 0.1;  
	XX[1] = 0.6;  
	XX[2] = 0.8;  

	YY[0] = 0.2;  
	YY[1] = 0.5;  
	YY[2] = 0.7;  

	Nsamp = 12; 
	n = 3;
	for (i = 0; i < n; i++)
		value[i] = -1.0;

	return 0;
}
