#pragma once

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
      int ToInt32();
      float ToSingle();
      double ToDouble();
      String^ ToString() new;

      array<int>^ ToInt32Array();      
      array<float>^ ToSingleArray();      
      array<double>^ ToDoubleArray();
      array<String^>^ ToStringArray();      
      
      // setters.
      void Set(int Value);
      void Set(float Value);
      void Set(double Value);
      void Set(String^ Value);
     
      void Set(array<int>^ Values);
      void Set(array<float>^ Values);
      void Set(array<double>^ Values);
      void Set(array<String^>^ Values);
      
   };        
