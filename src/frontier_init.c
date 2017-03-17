#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(front41)( int *imArg, int *ipcArg, int *iceptArg,
   int *nnArg, int *ntArg, int *nobArg, int *nbArg, int *nmuArg, int *netaArg,
   int *iprintArg, int *indicArg, double *tolArg, double *tol2Arg, double *bignumArg,
   double *step1Arg, int *igrid2Arg, double *gridnoArg, int *maxitArg, double *bmuArg,
   int *mrestartArg, double *frestartArg, int *nrestartArg,
   int *nStartVal, double *startVal, int *nRowData, int *nColData, double *dataTable,
   int *nParamTotal, double *ob, double *ga, double *gb,
   double *startLogl, double *y, double *h, double *fmleLogl,
   int *nIter, int *icodeArg, int *nfunctArg );

static const R_FortranMethodDef FortranEntries[] = {
    {"front41", (DL_FUNC) &F77_NAME(front41), 38},
    {NULL, NULL, 0}
};

void R_init_frontier(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
