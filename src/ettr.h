#ifndef _eTTR_H_
#define _eTTR_H_

#include <Rinternals.h>

/* declare functions called via .Call() */
SEXP adjRatios(SEXP, SEXP, SEXP);
SEXP aroon_max(SEXP, SEXP);
SEXP ema(SEXP, SEXP, SEXP, SEXP);
SEXP evwma(SEXP, SEXP, SEXP);
SEXP sar(SEXP, SEXP, SEXP);
SEXP ettr_rollPercentRank(SEXP, SEXP, SEXP, SEXP);
SEXP ettr_zigzag(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP WilderSum(SEXP, SEXP);
SEXP wma(SEXP, SEXP, SEXP);
SEXP zlema(SEXP, SEXP, SEXP);
SEXP runsum(SEXP, SEXP);
SEXP runmin(SEXP, SEXP);
SEXP runmax(SEXP, SEXP);
SEXP runmedian(SEXP, SEXP, SEXP, SEXP);
SEXP runmad(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP runcov(SEXP, SEXP, SEXP, SEXP, SEXP);

/* declare xts imports */
extern SEXP (*xts_na_check)(SEXP, SEXP);
#endif
