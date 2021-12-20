/* ichimoku - Functions Utilising R's C API --------------------------------- */

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

/* rolling mean over a window */
SEXP _ichimoku_meanOver(const SEXP x, const SEXP window) {

  const R_xlen_t n = XLENGTH(x);
  const int w = INTEGER_ELT(window, 0), w1 = w - 1;
  SEXP vec = PROTECT(Rf_allocVector(REALSXP, n));
  const double *px = REAL(x);
  double *pvec = REAL(vec);
  long double sum = 0;

  for (R_xlen_t i = 0; i < n; i++) {
    sum += px[i];
    if (i >= w1) {
      pvec[i] = sum / w;
      sum -= px[i - w1];
    } else {
      pvec[i] = NA_REAL;
    }
  }

  UNPROTECT(1);
  return vec;

}

/* look - inspect informational attributes */
SEXP _ichimoku_look(SEXP x) {

  x = PROTECT(Rf_shallow_duplicate(x));
  SEXP ax = PROTECT(ATTRIB(x));

  for (ax = ATTRIB(x); ax != R_NilValue; ax = CDR(ax)) {
    const SEXP tag = TAG(ax);
    if (tag == R_NamesSymbol || tag == R_RowNamesSymbol ||
        tag == R_DimSymbol || tag == R_DimNamesSymbol ||
        tag == R_ClassSymbol || !strcmp(CHAR(PRINTNAME(tag)), "index"))
      Rf_setAttrib(x, tag, R_NilValue);
  }

  UNPROTECT(2);
  return ATTRIB(x);

}

/* class object as POSIXct (in-place) */
SEXP _ichimoku_psxct(SEXP x) {

  SEXP posix = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(posix, 0, Rf_mkChar("POSIXct"));
  SET_STRING_ELT(posix, 1, Rf_mkChar("POSIXt"));
  Rf_setAttrib(x, R_ClassSymbol, posix);

  UNPROTECT(1);
  return x;

}

/* ichimoku to data.frame / tibble converter */
SEXP _ichimoku_tbl(const SEXP x, const SEXP type) {

  R_xlen_t xlen, xwid;
  const SEXP dims = PROTECT(Rf_getAttrib(x, R_DimSymbol));
  switch (TYPEOF(dims)) {
  case INTSXP:
    xlen = INTEGER(dims)[0];
    xwid = INTEGER(dims)[1];
    break;
  case REALSXP:
    xlen = REAL(dims)[0];
    xwid = REAL(dims)[1];
    break;
  }
  UNPROTECT(1);

  SEXP tbl = PROTECT(Rf_allocVector(VECSXP, xwid + 1));

  SEXP index = PROTECT(Rf_shallow_duplicate(Rf_getAttrib(x, Rf_install("index"))));
  index = _ichimoku_psxct(index);
  SET_VECTOR_ELT(tbl, 0, index);
  UNPROTECT(1);

  void *srcptr = NULL, *dstptr = NULL;
  srcptr = REAL(x);
  const char *src = srcptr;
  size_t vecsize = xlen * sizeof(double);
  for (R_xlen_t j = 1; j <= xwid; j++) {
    SEXP vec = PROTECT(Rf_allocVector(REALSXP, xlen));
    SET_VECTOR_ELT(tbl, j, vec);
    dstptr = REAL(vec);
    char *dst = dstptr;
    memcpy(dst, src, vecsize);
    src += vecsize;
    UNPROTECT(1);
  }

  const SEXP dn2 = PROTECT(VECTOR_ELT(Rf_getAttrib(x, R_DimNamesSymbol), 1));
  R_xlen_t dlen = XLENGTH(dn2);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, dlen + 1));
  SET_STRING_ELT(names, 0, Rf_mkChar("index"));
  for (R_xlen_t i = 0; i < dlen; i++) {
    SET_STRING_ELT(names, i + 1, STRING_ELT(dn2, i));
  }
  Rf_setAttrib(tbl, R_NamesSymbol, names);
  UNPROTECT(2);

  const int typ = INTEGER_ELT(type, 0);
  SEXP klass = PROTECT(Rf_allocVector(STRSXP, typ));
  switch (typ) {
  case 1:
    SET_STRING_ELT(klass, 0, Rf_mkChar("data.frame"));
    break;
  case 3:
    SET_STRING_ELT(klass, 0, Rf_mkChar("tbl_df"));
    SET_STRING_ELT(klass, 1, Rf_mkChar("tbl"));
    SET_STRING_ELT(klass, 2, Rf_mkChar("data.frame"));
    break;
  case 4:
    SET_STRING_ELT(klass, 0, Rf_mkChar("ichimoku_tbl"));
    SET_STRING_ELT(klass, 1, Rf_mkChar("tbl_df"));
    SET_STRING_ELT(klass, 2, Rf_mkChar("tbl"));
    SET_STRING_ELT(klass, 3, Rf_mkChar("data.frame"));
    break;
  }
  Rf_setAttrib(tbl, R_ClassSymbol, klass);
  UNPROTECT(1);

  SEXP rownames;
  if (xlen <= INT_MAX) {
    rownames = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(rownames)[0] = NA_INTEGER;
    INTEGER(rownames)[1] = -(int)xlen;
  } else {
    rownames = PROTECT(Rf_allocVector(REALSXP, 2));
    REAL(rownames)[0] = NA_REAL;
    REAL(rownames)[1] = -(double)xlen;
  }
  Rf_setAttrib(tbl, R_RowNamesSymbol, rownames);
  UNPROTECT(1);

  UNPROTECT(1);
  return tbl;

}

