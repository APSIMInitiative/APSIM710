#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include <fstream>
#include <functional>
#include "SimplePart.h"

#ifdef __GNUC__
#define _ISOC99_SOURCE
#else
#define isnan _isnan
#define isinf _isinf
#endif

using std::placeholders::_1;
using std::placeholders::_2;
using std::placeholders::_3;

// Debugging trace function
void XTrace(const char* lpszFormat, ...);

#ifdef _DEBUG
#define Debug XTrace
#else
#define Debug(s, ...) (void)0
#endif