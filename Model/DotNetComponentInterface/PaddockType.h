#pragma once
#include "ComponentType.h"
using namespace System;
using namespace System::Runtime::InteropServices;

[DllImport("ApsimShared.dll", EntryPoint = "getChildren", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void getChildren(String^ ComponentName, System::Text::StringBuilder^ Children);

[DllImport("ApsimShared.dll", EntryPoint = "isPaddock", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
bool isPaddock(String^ ComponentName);  

public ref class PaddockType : public ComponentType
   {
   // --------------------------------------------------------------------
   // Encapsulates an APSIM paddock in a simulation.
   // --------------------------------------------------------------------

   public:
      PaddockType(String^ Nam, ModelFramework::ApsimComponent^ component)
         : ComponentType(Nam, component) { }

      
      property TypedMultiList<PaddockType^>^ SubPaddocks
         {
         // --------------------------------------------------------------------
         // Return a list of all child components to caller.
         // --------------------------------------------------------------------
         TypedMultiList<PaddockType^>^ get()
            {
            getChildren(Name, Data);
            StringCollection^ ChildNames = CSGeneral::StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");

            TypedMultiList<PaddockType^>^ Children = gcnew TypedMultiList<PaddockType^>();
            for each (String^ ChildName in ChildNames)
               {
               PaddockType^ C = gcnew PaddockType(ChildName->Replace("\"",""), ParentComponent);
               if (C->IsOfType("ProtocolManager"))
                  Children->Add(C);
               }

            return Children;
            }
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
   };
   
public ref class MyPaddock
   {
   private: 
      static PaddockType^ _Singleton = nullptr;
   public:
    property PaddockType^ default[String^]
      {
      PaddockType^ get(String^ ComponentName)
         {
         if (_Singleton == nullptr)
            _Singleton = gcnew PaddockType(ComponentName, nullptr);
         return _Singleton;
         }
      }

   
   };   