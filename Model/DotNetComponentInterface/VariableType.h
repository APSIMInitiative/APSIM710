#pragma once
#include "NamedItem.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace System::Collections::Specialized;
using namespace System::Runtime::InteropServices;
using namespace CSGeneral;

namespace ModelFramework {
   ref class ApsimComponent;  // forward
   }
public ref class VariableType : NamedItem
   {
   /// --------------------------------------------------------------------------
   /// This class encapsulates an APSIM variable.
   /// --------------------------------------------------------------------------
   private:
      ModelFramework::ApsimComponent^ Component;
      String^ ComponentName;
   public:
      VariableType(ModelFramework::ApsimComponent^ component, String^ Componentname)
         {
         Component = component;
         ComponentName = Componentname;
         }
      property VariableType^ default[String^]
         {
         VariableType^ get(String^ Nam)
            {
            Name = Nam;
            return this;
            }
         }
      // getters.
      double ToDouble();
      array<double>^ ToDoubleArray();
      
      // setters.
      void Set(double Value);
      void Set(array<double>^ Values);
   };        
