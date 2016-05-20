/* C-PROGRAM FOR KERNEL REGRESSION				*/
/* prepared to be called by an Splus wrapper                    */
/* Parameters:                                                  */
/* x,y,xpred,ypred 1-D vectors of data                  	*/
/* n = number of observations					*/
/* p = number of explanatory variables  			*/
/* kernel = integer between 1 and 7 giving the kernel type      */
/*  1 = uniform                                                 */
/*  2 = cosine           					*/
/*  3 = triangular                                              */
/*  4 = Epanechnikov                 				*/
/*  5 = quartic                 				*/
/*  6 = Triweight                                               */
/*  7 = Gaussian         					*/
/*  The kernels are functions of the distance for kernel=1,2,3  */
/*  otherwise they are functions of the square distance         */
/*  They are not normalized to integrate to 1                   */
/* b = bandwidth of the kernel estimation			*/
#include<stdio.h>
#include<math.h>
#include<errno.h>
#include <iostream>
#include <stdlib.h>
#include "Array.h"

using namespace std;
using std::string;

#define PI 3.14159265

double kernel1 (double u)
{
  double w;
  if ( u > 0 && u < 1)  w=.5; else w=0.0;
  return(w);
}

double kernel2 (double u)
{
  double w;
  if ( u > 0 && u < 1) w= cos(PI/2.0 * u); else w=0.0;
  return(w);
}

double kernel3 (double u)
{
  double w;
  if ( u > 0 && u < 1) w= 1-fabs(u); else w=0.0;
  return(w);
}

double kernel4 (double u)
{
  double w;
  if ( u < 1) w= 1-u; else w=0.0;
  return(w);
}

double kernel5 (double u)
{
  double w;
  if ( u < 1) w= (1-u)*(1-u); else w=0.0;
  return(w);
}

double kernel6 (double u)
{
  double w;
  if ( u < 1) w= (1-u)*(1-u)*(1-u); else w=0.0;
  return(w);
}

double kernel7 (double u)
{
  return(exp(-0.5 * u));
}

extern "C" {
void kreg(double *x, double *y, double *xpred, double *ypred, int *pn, 
int *pp, int *pnpred, int *pkernel, double *pb)
{
  	double num, den;
  	double distance,weight;
  	double (*kernelproc)(double);

  	int n = *pn;
  	int npred = *pnpred;
  	int p = *pp;
  	int kernel = *pkernel;
  	double b = *pb;
    cout << "Bandwidth = " << b << endl;

    Array<double,2> X(n,p);
	Array<double,2> XPRED(npred,p);

   	for(int j = 0; j < p; j++)   
    	for(int  i = 0; i < n; i++)
      		X(i,j) = x[j * n + i];
  	for(int j = 0; j < p; j++)   
    	for(int i = 0; i < npred; i++)
      		XPRED(i,j) = xpred[j * npred + i];

  	switch (kernel)
  	{
    	case 1 :  kernelproc = kernel1; break;
    	case 2 :  kernelproc = kernel2; break;
    	case 3 :  kernelproc = kernel3; break;
    	case 4 :  kernelproc = kernel4; break;
    	case 5 :  kernelproc = kernel5; break;
    	case 6 :  kernelproc = kernel6; break;
    	case 7 :  kernelproc = kernel7; break;
    	default:  printf(" Error in the choice of kernel"); return;
  	}
	for (int k=0; k<npred; k++)
  	{
		num = 0.0;
       	den = 0.0;
       	for (int i=0; i<n; i++)
       	{
          	distance = 0.0;
          	for (int j=0; j<p; j++)
              	distance += (X(i,j)-XPRED(k,j))*(X(i,j)-XPRED(k,j));
	  		if(kernel <= 3)
	        	distance = sqrt(distance);
          	weight = kernelproc(distance/b);
          	num += weight * y[i];
          	den += weight;
       	}
    	if(den == 0)
          	error("Division by zero in the kernel computation. \n");
    	ypred[k] = num/den;
  	}
}

}




