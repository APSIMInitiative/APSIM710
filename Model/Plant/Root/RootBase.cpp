#include "StdPlant.h"

#include "RootBase.h"
#include "RootGrowthOption1.h"
#include "RootGrowthOption2.h"
#include "NoRoot.h"
#include "MultiRoot.h"

using namespace std;

RootBase::RootBase(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : SimplePart(scienceAPI,p,name)
   {
   }
