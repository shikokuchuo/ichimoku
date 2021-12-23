// ichimoku - Window Functions - Adapted from code with the following licence: -

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
#include "cpp11/doubles.hpp"

[[cpp11::register]]
cpp11::doubles maxOver(const cpp11::doubles& x, const int window) {

  const int n = x.size(), w1 = window - 1;
  cpp11::writable::doubles vec(n);
  std::deque<std::pair<int, long double> > q;

  for (int i = 0; i < n; ++i) {
    while (!q.empty() && q.back().second <= x[i]) {
      q.pop_back();
    }
    q.push_back(std::make_pair(i, x[i]));
    while(q.front().first <= i - window) {
      q.pop_front();
    }
    long double max = q.front().second;
    if (i >= w1) {
      vec[i] = max;
    } else {
      vec[i] = NA_REAL;
    }
  }

  return vec;

}

[[cpp11::register]]
cpp11::doubles minOver(const cpp11::doubles& x, const int window) {

  const int n = x.size(), w1 = window - 1;
  cpp11::writable::doubles vec(n);
  std::deque<std::pair<int, long double> > q;

  for (int i = 0; i < n; ++i) {
    while (!q.empty() && q.back().second >= x[i]) {
      q.pop_back();
    }
    q.push_back(std::make_pair(i, x[i]));
    while(q.front().first <= i - window) {
      q.pop_front();
    }
    long double min = q.front().second;
    if (i >= w1) {
      vec[i] = min;
    } else {
      vec[i] = NA_REAL;
    }
  }

  return vec;

}

