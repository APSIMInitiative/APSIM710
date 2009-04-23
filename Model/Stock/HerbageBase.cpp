#include "HerbageBase.h"

using namespace std;

std::string ftoa(double Float, char *fmtwidth)
   {
   return (ftoa(Float, ".2"));
   }


// ------------------------------------------------------------------
// default constructor
// ------------------------------------------------------------------
HerbageBase::HerbageBase(void)
   {
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
HerbageBase::HerbageBase(protocol::Component *s)
   {
      system = s;
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
HerbageBase::~HerbageBase(void)
   {
   }


