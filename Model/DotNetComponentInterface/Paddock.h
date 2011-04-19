#pragma once
#include "Component.h"
using namespace System;
using namespace System::Runtime::InteropServices;

[DllImport("ApsimShared.dll", EntryPoint = "getChildren", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void getChildren(String^ ComponentName, System::Text::StringBuilder^ Children);

[DllImport("ApsimShared.dll", EntryPoint = "isPaddock", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
bool isPaddock(String^ ComponentName);  

namespace ModelFramework {

/// <summary>
/// Paddock proxy class
/// </summary>
public ref class Paddock : public Component
   {

   public:
      Paddock(String^ _FullName, ModelFramework::ApsimComponent^ _Comp)
         : Component(_FullName, _Comp) 
         { 
         if (Types::Instance->TypeNames->Length == 0)
            PlugIns::LoadAll();
         }

      /// <summary>
      /// Returns a list of sub paddocks.
      /// </summary>
      property List<Paddock^>^ SubPaddocks
         {
         List<Paddock^>^ get()
            {
            getChildren(FullName, Data);
            StringCollection^ ChildNames = CSGeneral::StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");

            List<Paddock^>^ Children = gcnew List<Paddock^>();
            for each (String^ ChildName in ChildNames)
               {
               String^ ChildNameNoQuotes = ChildName->Replace("\"", "");
               if (ComponentType(ChildNameNoQuotes) == "ProtocolManager")
                  Children->Add(gcnew Paddock(ChildNameNoQuotes, Comp));
               }

            return Children;
            }
         }
         
      /// <summary>
      /// Returns a list of crops.
      /// </summary>
      property List<Component^>^ Crops
         {
         List<Component^>^ get()
            {
            getChildren(FullName, Data);
            StringCollection^ ChildNames = StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");
            List<Component^>^ Crops = gcnew List<Component^>();
            for each (String^ ChildName in ChildNames)
               {
               String^ ChildNameNoQuotes = ChildName->Replace("\"", "");
               String^ CompType = ComponentType(ChildNameNoQuotes);
               if (CompType == "Plant" || CompType == "Plant2" || Types::Instance->IsCrop(CompType))
                  Crops->Add(gcnew Component(ChildNameNoQuotes, Comp));
               }
            return Crops;
            }
         }

      /// <summary>
      /// Return the first sibling that has the specified TypeName. TypeName is the
      /// name of the DLL e.g. Plant or SoilWat
      /// </summary>
      Component^ ComponentByType(String^ TypeName)
         {
         getChildren(FullName, Data);
         StringCollection^ ChildNames = StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");

         for each (String^ ChildName in ChildNames)
            {
            String^ ChildNameNoQuotes = ChildName->Replace("\"", "");
            if (ComponentType(ChildNameNoQuotes)->ToLower() == TypeName->ToLower())
               return CreateComponent(ChildNameNoQuotes);
            }
         return nullptr;
         }
         

      /// <summary>
      /// Return the first sibling that has the specified Name. Name is the
      /// instance name as shown in the APSIM User Interface e.g. Wheat
      /// </summary>
      Component^ ComponentByName(String^ Name)
         {
         getChildren(FullName, Data);
         StringCollection^ ChildNames = StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");

         Name = ComponentName(Name);

         for each (String^ ChildName in ChildNames)
            {
            String^ ChildNameNoQuotes = ChildName->Replace("\"", "");
            if (ComponentName(ChildNameNoQuotes)->ToLower() == Name->ToLower())
               return CreateComponent(ChildNameNoQuotes);
            }
         return nullptr;
         }

      /// <summary>
      /// Publish a notification event i.e. one that doesn't have any data 
      /// associated with it. This event is broadcast to all components within scope.
      /// </summary>
      virtual void Publish(String^ EventName) override;

      /// <summary>
      /// Publish an event that has associated data. This event is broadcast to all components within scope.
      /// </summary>
      virtual void Publish(String^ EventName, ApsimType^ Data) override;


      /// <summary>
      ///  Create a component of the specified name
      /// </summary>
      Component^ CreateComponent(String^ ComponentName);

    
   }; // class
} // namespace
