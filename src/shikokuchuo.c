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
SEXP _ichimoku_look(const SEXP x) {

  SEXP ax, y = PROTECT(Rf_ScalarInteger(0));

  for (ax = ATTRIB(x); ax != R_NilValue; ax = CDR(ax)) {
    if (TAG(ax) != R_NamesSymbol && TAG(ax) != R_RowNamesSymbol &&
        TAG(ax) != R_DimSymbol && TAG(ax) != R_DimNamesSymbol &&
        TAG(ax) != R_ClassSymbol && strcmp(CHAR(PRINTNAME(TAG(ax))), "index"))
      Rf_setAttrib(y, TAG(ax), CAR(ax));
  }

  UNPROTECT(1);
  return ATTRIB(y);

}

/* class object as POSIXct (in-place) */
SEXP _ichimoku_psxct(SEXP x) {

  SEXP posix = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(posix, 0, Rf_mkChar("POSIXct"));
  SET_STRING_ELT(posix, 1, Rf_mkChar("POSIXt"));
  Rf_classgets(x, posix);

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
  Rf_namesgets(tbl, names);
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
  Rf_classgets(tbl, klass);
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

/* internal function used by ichimoku() */
SEXP _ichimoku_create(SEXP kumo, SEXP xtsindex, const SEXP periods,
                      const SEXP periodicity, const SEXP ticker, const SEXP x) {

  SEXP tzone = PROTECT(Rf_ScalarString(Rf_mkChar("")));
  Rf_setAttrib(xtsindex, Rf_install("tzone"), tzone);
  SEXP tclass = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(tclass, 0, Rf_mkChar("POSIXct"));
  SET_STRING_ELT(tclass, 1, Rf_mkChar("POSIXt"));
  Rf_setAttrib(xtsindex, Rf_install("tclass"), tclass);
  Rf_setAttrib(kumo, Rf_install("index"), xtsindex);
  UNPROTECT(2);

  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(klass, 0, Rf_mkChar("ichimoku"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("xts"));
  SET_STRING_ELT(klass, 2, Rf_mkChar("zoo"));
  Rf_classgets(kumo, klass);
  UNPROTECT(1);

  Rf_setAttrib(kumo, Rf_install("periods"), periods);
  Rf_setAttrib(kumo, Rf_install("periodicity"), periodicity);
  Rf_setAttrib(kumo, Rf_install("ticker"), ticker);

  if (x != R_NilValue) {
    SEXP ax;
    for (ax = ATTRIB(x); ax != R_NilValue; ax = CDR(ax)) {
      if (TAG(ax) != R_NamesSymbol && TAG(ax) != R_RowNamesSymbol &&
          TAG(ax) != R_DimSymbol && TAG(ax) != R_DimNamesSymbol &&
          TAG(ax) != R_ClassSymbol && strcmp(CHAR(PRINTNAME(TAG(ax))), "index") &&
          strcmp(CHAR(PRINTNAME(TAG(ax))), "periods") &&
          strcmp(CHAR(PRINTNAME(TAG(ax))), "periodicity") &&
          strcmp(CHAR(PRINTNAME(TAG(ax))), "ticker"))
        Rf_setAttrib(kumo, TAG(ax), CAR(ax));
    }
  }

  return kumo;

}

/* special ichimoku to data.frame converter for plots */
SEXP _ichimoku_df(const SEXP x) {

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

  SEXP df = PROTECT(Rf_allocVector(VECSXP, xwid + 2));

  SEXP index = PROTECT(Rf_shallow_duplicate(Rf_getAttrib(x, Rf_install("index"))));
  index = _ichimoku_psxct(index);
  SET_VECTOR_ELT(df, 0, index);
  UNPROTECT(1);

  void *srcptr = NULL, *dstptr = NULL;
  srcptr = REAL(x);
  const char *src = srcptr;
  size_t vecsize = xlen * sizeof(double);
  for (R_xlen_t j = 1; j <= xwid; j++) {
    SEXP vec = PROTECT(Rf_allocVector(REALSXP, xlen));
    SET_VECTOR_ELT(df, j, vec);
    dstptr = REAL(vec);
    char *dst = dstptr;
    memcpy(dst, src, vecsize);
    src += vecsize;
    UNPROTECT(1);
  }

  SEXP idchar = PROTECT(Rf_coerceVector(VECTOR_ELT(df, 5), STRSXP));
  SET_VECTOR_ELT(df, 5, idchar);
  UNPROTECT(1);

  const SEXP dn2 = PROTECT(VECTOR_ELT(Rf_getAttrib(x, R_DimNamesSymbol), 1));
  R_xlen_t dlen = XLENGTH(dn2);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, dlen + 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("index"));
  for (R_xlen_t i = 0; i < dlen; i++) {
    SET_STRING_ELT(names, i + 1, STRING_ELT(dn2, i));
  }
  SET_STRING_ELT(names, dlen + 1, Rf_mkChar("idx"));
  Rf_namesgets(df, names);
  UNPROTECT(2);

  SEXP klass = PROTECT(Rf_ScalarString(Rf_mkChar("data.frame")));
  Rf_classgets(df, klass);
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
  Rf_setAttrib(df, R_RowNamesSymbol, rownames);
  UNPROTECT(1);

  const SEXP idx = PROTECT(Rf_getAttrib(df, R_RowNamesSymbol));
  SET_VECTOR_ELT(df, xwid + 1, idx);

  UNPROTECT(2);
  return df;

}

