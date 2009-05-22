#pragma once
#include "ComponentType.h"

public ref class PaddockType : public ComponentType
   {
   // --------------------------------------------------------------------
   // Encapsulates an APSIM paddock in a simulation.
   // --------------------------------------------------------------------

   public:
      PaddockType(String^ Nam, ModelFramework::ApsimComponent^ component)
         : ComponentType(Nam, component) { }

      
   };