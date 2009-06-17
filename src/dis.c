#include <R.h>
#include <Rmath.h>

double F77_SUB(dis)( double *x )
   { return pnorm( *x, 0, 1, 1, 0 ); }
double F77_SUB(dislog)( double *x )
   { return pnorm( *x, 0, 1, 1, 1 ); }
double F77_SUB(den)( double *x )
   { return dnorm( *x, 0, 1, 0 ); }
