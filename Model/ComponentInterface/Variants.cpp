#pragma hdrstop

#include <stdexcept>
#include "Variants.h"
#include <general/platform.h>
using namespace protocol;

// ------------------------------------------------------------------
//  Short description:
//     Return the number of Variant objects in specified variants.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned EXPORT STDCALL variants_size(const Variants& variants)
   {
   return variants.size();
   }
