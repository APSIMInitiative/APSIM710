#pragma once
#include "VariableType.h"
using namespace System;
using namespace System::Collections::Generic;
using namespace System::Collections::Specialized;
using namespace System::Runtime::InteropServices;
using namespace CSGeneral;

[DllImport("ApsimShared.dll", EntryPoint = "getChildren", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void getChildren(String^ ComponentName, System::Text::StringBuilder^ Children);

[DllImport("ApsimShared.dll", EntryPoint = "findVariable", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
bool findVariable(String^ OwnerComponentName, String^ VariableName);

[DllImport("ApsimShared.dll", EntryPoint = "isPaddock", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
bool isPaddock(String^ ComponentName);  

[DllImport("ApsimShared.dll", EntryPoint = "getComponentType", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void getComponentType(String^ ComponentName, System::Text::StringBuilder^ ComponentType); 

namespace ModelFramework {
   ref class ApsimComponent;  // forward
   }
public ref class ComponentType : public TypedItem
   {
   // --------------------------------------------------------------------
   // Returns the singleton instance of a reflection class that is
   // capable of returning metadata about the structure of the simulation.
   // --------------------------------------------------------------------
   
   protected:
      static System::Text::StringBuilder^ Data = gcnew System::Text::StringBuilder(10000);
      String^ Name;
      String^ TypeName;
      ModelFramework::ApsimComponent^ ParentComponent;

   public:
      virtual bool IsOfType(String^ TypeNameToMatch) override
         {
         return TypeName->ToLower() == TypeNameToMatch->ToLower();         
         } 
      bool IsCrop()
         {
         // Turn the fully qualified name (e.g. .MasterPM.Paddock1.wheat) into a
         // crop name (e.g. wheat)
         String^ CropName = Name;
         int PosLastPeriod = CropName->LastIndexOf('.');
         if (PosLastPeriod != -1)
            CropName = CropName->Substring(PosLastPeriod+1);
         return ApsimFile::Configuration::Instance->IsCrop(CropName);
         }               
   
   
      ComponentType(String^ Nam, ModelFramework::ApsimComponent^ component)
         {
         // --------------------------------------------------------------------
         // Constructor
         // --------------------------------------------------------------------
         
         Name = Nam;
         ParentComponent = component;
         getComponentType(Name, Data);
         TypeName = Data->ToString();
         }

         
      property TypedList<ComponentType^>^ Component
        {
         // --------------------------------------------------------------------
         // Return a list of all sibling components to caller.
         // --------------------------------------------------------------------
         TypedList<ComponentType^>^ get()
            {
            getChildren(Name, Data);
            StringCollection^ ChildNames = CSGeneral::StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");

            TypedList<ComponentType^>^ Children = gcnew TypedList<ComponentType^>();
            for each (String^ ChildName in ChildNames)
               {
               ComponentType^ C = gcnew ComponentType(ChildName->Replace("\"",""), ParentComponent);
               Children->Add(C);
               }
            return Children;
            }        
         
         }
         
      property VariableType^ Variable
         {
         VariableType^ get()
            {
            return gcnew VariableType(ParentComponent, Name);
            }
         }
         
      void Publish(String^ EventName, ApsimType^ Data)
         {
         ParentComponent->Publish(Name + "." + EventName, Data);
         }
         
         
         

      
   };

