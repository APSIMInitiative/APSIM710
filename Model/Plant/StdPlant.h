#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include <fstream>
#include <boost/bind.hpp>
#include "SimplePart.h"

#ifdef __GNUC__
#define _ISOC99_SOURCE
#else
#define isnan _isnan
#define isinf _isinf
#endif



// Debugging trace function
void XTrace(const char* lpszFormat, ...);
#ifdef _DEBUG
#define Debug XTrace
#else
#define Debug  
#endif