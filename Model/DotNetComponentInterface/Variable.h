#pragma once

using namespace System;
using namespace System::Collections::Generic;
using namespace System::Collections::Specialized;
using namespace System::Runtime::InteropServices;
using namespace CSGeneral;

namespace ModelFramework {
   ref class ApsimComponent;  // forward
   
public ref class Variable
   {
   /// --------------------------------------------------------------------------
   /// This class encapsulates an APSIM variable.
   /// --------------------------------------------------------------------------
   private:
      ModelFramework::ApsimComponent^ Component;
      String^ _Name;
   public:
      Variable(ModelFramework::ApsimComponent^ component, String^ VariableName)
         {
         Component = component;
         _Name = VariableName;
         }

      bool Exists();
      property String^ Name {String^ get() {return _Name;}}

      // getters.
      bool ToBoolean();
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
   }