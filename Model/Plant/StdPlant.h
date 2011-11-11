#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include <boost/bind.hpp>
#include "SimplePart.h"

#ifndef WIN32
int isnan(double x) { return x != x; }
int isinf(double x) { return !isnan(x) && isnan(x - x); }
#else
#define isnan _isnan
#define isinf _isinf
#endif