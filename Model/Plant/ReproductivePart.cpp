#include "StdPlant.h"
#include "ReproductivePart.h"
using namespace std;

ReproductivePart::ReproductivePart(ScienceAPI& api, plantInterface *p, const string &name)
//=======================================================================================
     : SimplePart(api, p, name)
     {
     }

