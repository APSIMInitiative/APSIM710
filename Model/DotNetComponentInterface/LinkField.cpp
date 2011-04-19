#pragma once
#include "LinkField.h"
#include "ApsimComponent.h"

using namespace System::Collections::Specialized;


[DllImport("ApsimShared.dll", EntryPoint = "getChildren", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void getChildren(String^ ComponentName, System::Text::StringBuilder^ Children);

[DllImport("ApsimShared.dll", EntryPoint = "getComponentType", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void getComponentType(String^ ComponentName, System::Text::StringBuilder^ ComponentType); 

void LinkField::Resolve()
   {
   if (Field->GetValue(In) == nullptr)
      {
      // Work out our name and the name of our containing paddock.
      String^ OurName = Comp->GetName();

      // Load in the probe info assembly.
      Assembly^ ProbeInfo = Types::GetProbeInfoAssembly();
      
      bool TypeIsInDotNetComponentInterface = false;
      String^ FQN = nullptr;
      if (LinkAttr->_Path == nullptr)
         {
         String^ TypeToFind = Field->FieldType->Name;

         // If the type to find is "Paddock" then don't go looking for children.
         if (TypeToFind == "Paddock")
            {
            String^ PaddockName = OurName->Substring(0, OurName->LastIndexOf('.'));
            FQN = PaddockName;
            TypeIsInDotNetComponentInterface = true;
            }
         else if (TypeToFind == "Component")
            {
            FQN = OurName;
            TypeIsInDotNetComponentInterface = true;
            }

         else
            {
            FQN = FindComponentByType(TypeToFind, OurName);
            if (FQN == "")
               {
               if (LinkAttr->_IsOptional == IsOptional::Yes)
                  return;
               throw gcnew Exception("Cannot find [Link] for type: " + TypeToFind);
               }
            }
         }
      else
         {
         // The path must be a name.
         FQN = FindComponentByName(LinkAttr->_Path, OurName);
         }

      // Now go create a proxy object
      String^ ProxyTypeString = Field->FieldType->Name;
      Type^ ProxyType;
      if (TypeIsInDotNetComponentInterface)
         ProxyType = Assembly::GetExecutingAssembly()->GetType("ModelFramework." + ProxyTypeString);
      else
         ProxyType = Types::GetProbeInfoAssembly()->GetType("ModelFramework." + ProxyTypeString);

      if (ProxyType == nullptr)
         throw gcnew Exception("Cannot find proxy reference: " + ProxyTypeString);
      array<Object^>^ Parameters = gcnew array<Object^>(2);
      Parameters[0] = FQN;
      Parameters[1] = Comp;
      Object^ ReferencedObject = Activator::CreateInstance(ProxyType, Parameters);

      // Set the value of the [Link] field to the newly created proxy object.
      Field->SetValue(In, ReferencedObject);
      }
   }


// Go find a component IN SCOPE that matches the specified type.
// IN SCOPE means a component that is a sibling or in the paddock
// above.
String^ LinkField::FindComponentByType(String^ TypeToFind, String^ OurName)
   {
   String^ PaddockName = OurName->Substring(0, OurName->LastIndexOf('.'));

   // Get a list of all paddock children.
   getChildren(PaddockName, Data);
   StringCollection^ ChildNames = CSGeneral::StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");

   // Go through all children and find a component that has the specified type.
   for each (String^ ChildName in ChildNames)
      {
      String^ ChildNameNoQuotes = ChildName->Replace("\"","");
      getComponentType(ChildNameNoQuotes, Data);
      String^ ChildType = Data->ToString();

      if (ChildType->ToLower() == TypeToFind->ToLower())
         return ChildNameNoQuotes;
      }

   // If we get this far then we need to go to our parent and search it's children.
   int PosLastPeriod = OurName->LastIndexOf('.');
   if (PosLastPeriod == -1)
      return "";
   String^ ParentName = OurName->Substring(0, PosLastPeriod);
   if (ParentName == ".MasterPM")
      return "";
   return FindComponentByType(TypeToFind, ParentName);
   }

// Go find a component with the specified name. NameToFind can be either
// a relative or absolute address.
String^ LinkField::FindComponentByName(String^ NameToFind, String^ OurName)
   {
   String^ Delimiter = ".";

   if (NameToFind->Contains(".MasterPM."))
      return NameToFind;   // absolute reference.
   else
      {
      // relative reference.

      String^ ParentName = "";
      int PosLastPeriod = OurName->LastIndexOf('.');
      if (PosLastPeriod == -1)
         throw gcnew Exception("Invalid component name found: " + OurName);
      ParentName = OurName->Substring(0, PosLastPeriod);
      return ParentName + "." + NameToFind;
      }
   }
