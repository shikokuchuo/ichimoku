// ichimoku - Window Functions: modified from code with the following license:

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
#include <cpp11.hpp>

[[cpp11::register]]
cpp11::doubles maxOver(const cpp11::doubles& x, int window) {

  int n = x.size(), w1 = window - 1;
  cpp11::writable::doubles vec(n);

  std::deque<std::pair<long double, int>> deck;
  for (int i = 0; i < n; ++i) {
      while (!deck.empty() && deck.back().first <= x[i])
        deck.pop_back();
    deck.push_back(std::make_pair(x[i], i));

    while(deck.front().second <= i - window)
      deck.pop_front();

    long double min = deck.front().first;
    if (i >= w1) {
      vec[i] = min;
    } else {
      vec[i] = NA_REAL;
    }
  }
  return vec;
}

[[cpp11::register]]
cpp11::doubles minOver(const cpp11::doubles& x, int window) {

  int n = x.size(), w1 = window - 1;
  cpp11::writable::doubles vec(n);

  std::deque<std::pair<long double, int>> deck;
  for (int i = 0; i < n; ++i) {
      while (!deck.empty() && deck.back().first >= x[i])
        deck.pop_back();
    deck.push_back(std::make_pair(x[i], i));

    while(deck.front().second <= i - window)
      deck.pop_front();

    long double min = deck.front().first;
    if (i >= w1) {
      vec[i] = min;
    } else {
      vec[i] = NA_REAL;
    }
  }
  return vec;
}

[[cpp11::register]]
cpp11::doubles meanOver(const cpp11::doubles& x, int window) {

  int n = x.size(), w1 = window - 1;
  cpp11::writable::doubles vec(n);
  long double sum = 0;
  for (int i = 0; i < n; ++i) {
    sum += x[i];
    if (i >= w1) {
      vec[i] = sum / window;
      sum -= x[i - w1];
    } else {
      vec[i] = NA_REAL;
    }
  }
  return vec;
}

