#pragma once
#include "ComponentType.h"
#include "SoilWaterType.h"
#include "SoilNitrogenType.h"
#include "CropType.h"
#include "FertiliserType.h"
#include "IrrigationType.h"
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
      PaddockType(Instance^ In)
         : ComponentType(In->ParentComponent()->GetName()->Substring(0, In->ParentComponent()->GetName()->LastIndexOf('.')), 
                         In->ParentComponent())
         {
         if (Types::Instance->TypeNames->Length == 0)
            PlugIns::LoadAll();
         }
         
      PaddockType(String^ Nam, ModelFramework::ApsimComponent^ component)
         : ComponentType(Nam, component) 
         {
         if (Types::Instance->TypeNames->Length == 0)
            PlugIns::LoadAll();
         }


      property String^ Name
         {
         String^ get()
            {
            return ComponentType::Name;   
            }
         }
      
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
         
      ComponentType^ Component(String^ TypeToFind)
        {
         // --------------------------------------------------------------------
         // Return a list of all sibling components to caller.
         // --------------------------------------------------------------------
         getChildren(Name, Data);
         StringCollection^ ChildNames = CSGeneral::StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");

         for each (String^ ChildName in ChildNames)
            {
            ComponentType^ C = gcnew ComponentType(ChildName->Replace("\"",""), ParentComponent);
            if (C->IsOfType(TypeToFind))
               return C;
            }
         return nullptr;
         }

      ComponentType^ ComponentByName(String^ NameToFind) 
         {
         // --------------------------------------------------------------------
         // Return a list of all sibling components to caller.
         // --------------------------------------------------------------------
         getChildren(Name, Data);
         StringCollection^ ChildNames = CSGeneral::StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");

         for each (String^ ChildName in ChildNames)
            {
            int PosDelimiter = ChildName->LastIndexOf('.');
            if (PosDelimiter != -1)
               {
               String^ Child = ChildName->Substring(PosDelimiter+1)->Replace("\"","");
               if (Child->ToLower() == NameToFind->ToLower())
                  return gcnew ComponentType(ChildName->Replace("\"",""), ParentComponent);
               }
            }
         return nullptr;
         }
            
      property SoilWaterType^ SoilWater
         {
         SoilWaterType^ get()
            {
            ComponentType^ SoilToReturn;
            SoilToReturn = Component("soilwat");
            if (SoilToReturn == nullptr)
               SoilToReturn = Component("swim2");

            if (SoilToReturn != nullptr)
               return gcnew SoilWaterType(SoilToReturn);
            return nullptr;            
            }
         }
      property SoilNitrogenType^ SoilNitrogen
         {
         SoilNitrogenType^ get()
            {
            ComponentType^ SoilToReturn;
            SoilToReturn = Component("soiln");
            if (SoilToReturn != nullptr)
               return gcnew SoilNitrogenType(SoilToReturn);
            return nullptr;            
            }
         }         
      property List<CropType^>^ Crops
         {
         // --------------------------------------------------------------------
         // Return a list of all child crops to caller.
         // --------------------------------------------------------------------
         List<CropType^>^ get()
            {
            getChildren(Name, Data);
            StringCollection^ ChildNames = CSGeneral::StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");

            List<CropType^>^ Children = gcnew List<CropType^>();
            for each (String^ ChildName in ChildNames)
               {
               ComponentType^ ChildComponent = gcnew ComponentType(ChildName->Replace("\"",""), ParentComponent);
               if (ChildComponent->IsCrop())
                  Children->Add(gcnew CropType(ChildComponent));
               }

            return Children;
            }
         }         
      property FertiliserType^ Fertiliser
         {
         FertiliserType^ get()
            {
            ComponentType^ C = Component("fertiliser");
            if (C != nullptr)
               return gcnew FertiliserType(C);
            else
               return nullptr;            
            }
         }   
      property IrrigationType^ Irrigation
         {
         IrrigationType^ get()
            {
            ComponentType^ C = Component("irrigation");
            if (C != nullptr)
               return gcnew IrrigationType(C);
            else
               return nullptr;            
            }
         }                
   };
   