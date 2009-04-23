#include <general\pch.h>
#include <boost/function.hpp>
#include "ConverterBase.h"
#include "Constants.h"


#pragma package(smart_init)
using namespace std;


#define singleArrayTypeDDML \
   "<type  array=\"T\" kind=\"single\"/>"


// ------------------------------------------------------------------
// default constructor
// ------------------------------------------------------------------
ConverterBase::ConverterBase(void)
   {
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ConverterBase::ConverterBase(protocol::Component *s)
   {
      system = s;
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
ConverterBase::~ConverterBase(void)
   {
   }
