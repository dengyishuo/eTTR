/*
 *  eTTR: Enhanced Technical Trading Rules
 *
 *  Copyright (C) 2007-2013  Joshua M. Ulrich
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <R.h>
#include <Rinternals.h>

SEXP WilderSum (SEXP x, SEXP n) {

    /* Initialize loop and PROTECT counters */
    int i, P=0;

    /* assure that 'x' is double */
    if(TYPEOF(x) != REALSXP) {
      PROTECT(x = coerceVector(x, REALSXP)); P++;
    }

    /* Pointers to function arguments */
    double *d_x = REAL(x);
    int i_n = asInteger(n);

    /* Input object length */
    int nr = nrows(x);

    /* Initialize result R object */
    SEXP result;
    PROTECT(result = allocVector(REALSXP,nr)); P++;
    double *d_result = REAL(result);

    /* Find first non-NA input value */
    int beg = i_n - 1;
    double sum = 0;
    for(i = 0; i < beg; i++) {
        /* Account for leading NAs in input */
        if(ISNA(d_x[i])) {
            d_result[i] = NA_REAL;
            beg++;
            d_result[beg] = 0;
            continue;
        }
        /* Set leading NAs in output */
        if(i < beg) {
            d_result[i] = NA_REAL;
        }
        /* Calculate raw sum to start */
        sum += d_x[i];
    }

    d_result[beg] = d_x[i] + sum * (i_n-1)/i_n;

    /* Loop over non-NA input values */
    for(i = beg+1; i < nr; i++) {
        d_result[i] = d_x[i] + d_result[i-1] * (i_n-1)/i_n;
    }

    /* UNPROTECT R objects and return result */
    UNPROTECT(P);
    return(result);
}
