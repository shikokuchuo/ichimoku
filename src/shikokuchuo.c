// ichimoku - Functions Utilising R's C API ------------------------------------

#define R_NO_REMAP
#include <string.h>
#include <R.h>
#include <Rinternals.h>

SEXP _ichimoku_meanOver(SEXP x, SEXP window) {

  const R_xlen_t n = Rf_xlength(x), w = Rf_asInteger(window), w1 = w - 1;
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

SEXP _ichimoku_look(SEXP x) {

  x = PROTECT(Rf_shallow_duplicate(x));
  SEXP ax = PROTECT(ATTRIB(x));

  while(ax != R_NilValue) {
    const SEXP tag = TAG(ax);
    if (tag == R_NamesSymbol || tag == R_RowNamesSymbol ||
        tag == R_DimSymbol || tag == R_DimNamesSymbol ||
        tag == R_ClassSymbol || !strcmp(CHAR(PRINTNAME(tag)), "index")) {
      Rf_setAttrib(x, tag, R_NilValue);
    }
    ax = CDR(ax);
  }

  ax = ATTRIB(x);
  UNPROTECT(2);
  return ax;

}

SEXP _ichimoku_psxct(SEXP x) {

  x = PROTECT(Rf_shallow_duplicate(x));
  SEXP posix = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(posix, 0, Rf_mkChar("POSIXct"));
  SET_STRING_ELT(posix, 1, Rf_mkChar("POSIXt"));
  Rf_setAttrib(x, R_ClassSymbol, posix);

  UNPROTECT(2);
  return x;

}

static void *c_get_data(SEXP x, size_t *widthptr) {

  void *ptr = NULL;
  ptr = REAL(x);

  if (widthptr) {
    *widthptr = sizeof(double);
  }
  return ptr;

}

SEXP _ichimoku_tbl(SEXP x, SEXP type) {

  SEXP dims = PROTECT(Rf_getAttrib(x, R_DimSymbol));
  R_xlen_t xlen, xwid;
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

  SEXP out = PROTECT(Rf_allocVector(VECSXP, xwid + 1));

  SEXP index = PROTECT(_ichimoku_psxct(Rf_getAttrib(x, Rf_install("index"))));
  SET_VECTOR_ELT(out, 0, index);
  UNPROTECT(1);

  size_t eltsize;
  const char *src = c_get_data(x, &eltsize);
  size_t colsize = xlen * eltsize;
  for (R_xlen_t j = 1; j <= xwid; j++) {
    SEXP col = PROTECT(Rf_allocVector(REALSXP, xlen));
    SET_VECTOR_ELT(out, j, col);
    char *dst = c_get_data(col, NULL);
    memcpy(dst, src, colsize);
    src += colsize;
    UNPROTECT(1);
  }

  SEXP dn2 = PROTECT(VECTOR_ELT(Rf_getAttrib(x, R_DimNamesSymbol), 1));
  R_xlen_t dlen = Rf_xlength(dn2);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, dlen + 1));
  SET_STRING_ELT(names, 0, Rf_mkChar("index"));
  for (R_xlen_t i = 0; i < dlen; i++) {
    SET_STRING_ELT(names, i + 1, STRING_ELT(dn2, i));
  }
  Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(2);

  const int typ = Rf_asInteger(type);
  SEXP cls = PROTECT(Rf_allocVector(STRSXP, typ));
  switch (typ) {
  case 1:
    SET_STRING_ELT(cls, 0, Rf_mkChar("data.frame"));
    break;
  case 3:
    SET_STRING_ELT(cls, 0, Rf_mkChar("tbl_df"));
    SET_STRING_ELT(cls, 1, Rf_mkChar("tbl"));
    SET_STRING_ELT(cls, 2, Rf_mkChar("data.frame"));
    break;
  case 4:
    SET_STRING_ELT(cls, 0, Rf_mkChar("ichimoku_tbl"));
    SET_STRING_ELT(cls, 1, Rf_mkChar("tbl_df"));
    SET_STRING_ELT(cls, 2, Rf_mkChar("tbl"));
    SET_STRING_ELT(cls, 3, Rf_mkChar("data.frame"));
    break;
  }
  Rf_setAttrib(out, R_ClassSymbol, cls);
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
  Rf_setAttrib(out, R_RowNamesSymbol, rownames);
  UNPROTECT(1);

  UNPROTECT(1);
  return out;

}

