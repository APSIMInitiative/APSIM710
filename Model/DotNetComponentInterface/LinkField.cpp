#pragma once
#include "LinkField.h"
#include "ApsimComponent.h"

using namespace System::Collections::Specialized;
using namespace System::IO;


[DllImport("ApsimShared.dll", EntryPoint = "getChildren", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void getChildren(String^ ComponentName, System::Text::StringBuilder^ Children);

[DllImport("ApsimShared.dll", EntryPoint = "getComponentType", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void getComponentType(String^ ComponentName, System::Text::StringBuilder^ ComponentType); 

/// <summary>
/// Resolve this [Link]
/// </summary>
void LinkField::Resolve()
   {
   if (Field->GetValue(In) == nullptr)
      {
      Object^ ReferencedObject = nullptr;

      // Load in the probe info assembly.
      Assembly^ ProbeInfo = Types::GetProbeInfoAssembly();

      String^ TypeToFind = Field->FieldType->Name;
      String^ NameToFind = Field->Name;

      if (IsAPSIMType(TypeToFind))
         {
         if (LinkAttr->_Path == nullptr)
            NameToFind = nullptr; // default is not to use name.
         else
            {
            NameToFind = LinkAttr->_Path;
            }
         if (NameToFind != nullptr && NameToFind->Contains("."))
            ReferencedObject = GetSpecificApsimComponent(NameToFind);
         else
            ReferencedObject = FindApsimComponent(NameToFind, TypeToFind);
         }
      else
         {
         // Link is an Instance link - use name and type to find the object.
         ReferencedObject = FindInstanceObject(NameToFind, TypeToFind);
         }

      // Set the value of the [Link] field to the newly created object.
      if (ReferencedObject == nullptr)
         {
         if (LinkAttr->_IsOptional != IsOptional::Yes)
            throw gcnew Exception("Cannot find [Link] for type: " + TypeToFind + " " + NameToFind + " in object: " + In->Name);
         }
      else
         Field->SetValue(In, ReferencedObject);
      }
   }

/// <summary>
/// Search for an Instance object in scope that has the specified name and type.
/// Returns null if not found.
/// </summary>
Object^ LinkField::FindInstanceObject(String^ NameToFind, String^ TypeToFind)
   {
   // Check our children first.
   for each (NamedItem^ Child in In->Children)
      {
      if (NameToFind == Child->Name && Utility::IsOfType(Child->GetType(), TypeToFind))
         return Child;
      }

   // Check our siblings, our parent's siblings and our parent's, parent's siblings etc.
   Instance^ Parent = In->Parent;
   while (Parent != nullptr)
      {
      if (Parent->Name == TypeToFind)
         return Parent;
      for each (NamedItem^ Sibling in Parent->Children)
         {
         if (NameToFind == Sibling->Name && Utility::IsOfType(Sibling->GetType(), TypeToFind))
            return Sibling;
         }
      Parent = Parent->Parent;
      }
   return nullptr;
   }


/// <summary>
/// Returns true if the specified typename is an APSIM type.
/// </summary>
bool LinkField::IsAPSIMType(String^ TypeToFind)
   {
   Type^ ProxyType;
   if (TypeToFind == "Paddock" || TypeToFind == "Component")
      return true;
   else
      return Types::GetProbeInfoAssembly()->GetType("ModelFramework." + TypeToFind) != nullptr;
   }

/// <summary>
/// Create a DotNetProxy class to represent an Apsim component.
/// </summary>
Object^ LinkField::CreateDotNetProxy(String^ TypeToFind, String^ FQN)
   {
   Type^ ProxyType;
   if (TypeToFind == "Paddock" || TypeToFind == "Component")
      ProxyType = Assembly::GetExecutingAssembly()->GetType("ModelFramework." + TypeToFind);
   else
      ProxyType = Types::GetProbeInfoAssembly()->GetType("ModelFramework." + TypeToFind);

   if (ProxyType == nullptr)
      throw gcnew Exception("Cannot find proxy reference: " + TypeToFind);
   array<Object^>^ Parameters = gcnew array<Object^>(2);
   Parameters[0] = FQN;
   Parameters[1] = Comp;
   return Activator::CreateInstance(ProxyType, Parameters);
   }

/// <summary>
/// Go find an Apsim component IN SCOPE that matches the specified name and type.
/// IN SCOPE means a component that is a sibling or in the paddock
/// above.
/// </summary>
Object^ LinkField::FindApsimComponent(String^ NameToFind, String^ TypeToFind)
   {
   String^ OurName = Comp->GetName();
   String^ PaddockName = OurName->Substring(0, OurName->LastIndexOf('.'));

   if (TypeToFind == "Paddock")
      return CreateDotNetProxy(TypeToFind, PaddockName);
   if (TypeToFind == "Component" && NameToFind == nullptr)
      return CreateDotNetProxy(TypeToFind, OurName);

   // The TypeToFind passed in is a DotNetProxy type name. We need to convert this to a DLL name
   // e.g. TypeToFind = Outputfile, DLLName = Report
   String^ DLLFileName = TypeToFind;
   List<String^>^ DLLs = Types::Instance->Dlls(TypeToFind);
   if (DLLs->Count > 0)
      DLLFileName = Path::GetFileNameWithoutExtension(DLLs[0]);

   while (PaddockName != "")
      {
      // Get a list of all paddock children.
      getChildren(PaddockName, Data);
      StringCollection^ SiblingNames = CSGeneral::StringManip::SplitStringHonouringQuotes(Data->ToString(), ",");

      // Go through all siblings and find a component that has the specified type.
      for each (String^ SiblingName in SiblingNames)
         {
         String^ SiblingNameNoQuotes = SiblingName->Replace("\"","");
         getComponentType(SiblingNameNoQuotes, Data);
         String^ SiblingType = Data->ToString();

         if (SiblingType->ToLower() == DLLFileName->ToLower())
            {
            String^ SiblingShortName = SiblingNameNoQuotes->Substring(SiblingNameNoQuotes->LastIndexOf('.')+1);
            if (NameToFind == nullptr || NameToFind == SiblingShortName)
               return CreateDotNetProxy(TypeToFind, SiblingNameNoQuotes);
            }
         }

      // Go to parent paddock.
      PaddockName = PaddockName->Substring(0, PaddockName->LastIndexOf('.'));
      }

   // If we get this far then we didn't find the APSIM component.
   return nullptr;
   }

/// <summary>
/// Go find a component with the specified name. NameToFind can be either
/// a relative or absolute address.
/// </summary>
Object^ LinkField::GetSpecificApsimComponent(String^ NameToFind)
   {
   String^ Delimiter = ".";

   if (NameToFind->Contains(".MasterPM."))
      return CreateDotNetProxy(NameToFind, NameToFind);   // absolute reference.
   else
      {
      // relative reference.
      String^ OurName = Comp->GetName();
      String^ ParentName = "";
      int PosLastPeriod = OurName->LastIndexOf('.');
      if (PosLastPeriod == -1)
         throw gcnew Exception("Invalid component name found: " + OurName);
      ParentName = OurName->Substring(0, PosLastPeriod);
      return CreateDotNetProxy(NameToFind, ParentName + "." + NameToFind);
      }
   }

