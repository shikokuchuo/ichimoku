// This file is modified from the original with the following license.

/*
 Based on http://opensource.org/licenses/MIT
 Copyright (c) 2015, Andrew Uhl
 Permission is hereby granted, free of charge, to any person obtaining
 a copy of this software and associated documentation files (the
 "Software"), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <deque>
#include <utility>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// types of calculations
enum CalcType {MIN, MAX};

// stores function arguments for non-data
struct Args {
  int window;
  CalcType ctype;
};

// calculates rolling window for {minimum, maximum}
NumericVector roll_minmax(const NumericVector& x, Args a) {

  int n  = x.length();
  NumericVector rollx(n);

  std::deque< std::pair<long double, int> > deck;
  for (int i = 0; i < x.size(); ++i) {
    if(a.ctype == MIN) {
      while (!deck.empty() && deck.back().first >= x[i])
        deck.pop_back();
    } else {
      while (!deck.empty() && deck.back().first <= x[i])
        deck.pop_back();
    }
    deck.push_back(std::make_pair(x[i], i));

    while(deck.front().second <= i - a.window)
      deck.pop_front();

    long double min = deck.front().first;
    if (i < a.window - 1) {
      rollx[i] = NA_REAL;
    } else {
      rollx[i] = min;
    }
  }
  return rollx;
}

//' Maximum over a rolling window
//'
//' Calculate the maximum over a rolling window
//'
//' @param x a vector.
//' @param window size of the rolling window.
//' @return A vector of the same length as 'x' with elements 1 to
//'     (length(window) - 1) containing NAs.
//' @examples
//' max <- maxOver(sample_ohlc_data$Close, 9L)
//' @export
// [[Rcpp::export]]
NumericVector maxOver(const SEXP& x, int window) {
  Args a; a.window = window; a.ctype = MAX;
  return roll_minmax(x, a);
}

//' Minimum over a rolling window
//'
//' Calculate the minimum over a rolling window
//'
//' @param x a vector.
//' @param window size of the rolling window.
//' @return A vector of the same length as 'x' with elements 1 to
//'     (length(window) - 1) containing NAs.
//' @examples
//' min <- minOver(sample_ohlc_data$Close, 9L)
//' @export
// [[Rcpp::export]]
NumericVector minOver(const SEXP& x, int window) {
  Args a; a.window = window; a.ctype = MIN;
  return roll_minmax(x, a);
}
