using namespace System;
using namespace System::Collections::Generic;
using namespace System::Text;
using namespace System::Reflection;

#include "Instance.h"
#include "FactoryProperty.h"

Instance::Instance()
   {
   Children = gcnew NamedList<NamedItem^>();
   Parent = nullptr;
   }

bool Instance::Override(Type^ aType, String^ targetName)
{
   // Now look for parameter overrides and apply them as needed
   for (int i = 0; i != Children->Count; i++)
   {
      if (Children[i]->GetType()->Equals(aType) &&
          Children[i]->Name == targetName)
      {
        safe_cast<DerivedInstance^>(Children[i])->ApplyOverrides();
        return true;
      }
   }
   return false;
}

void DerivedInstance::ApplyOverrides()
   {
      for each (XmlNode^ Instruction in xml->ChildNodes)
      {
         if (Instruction->Name == "Override")
         {
			 Instance^ target;
			 String^ ReferencedNodeName = XmlHelper::Attribute(Instruction, "name");
			 if (ReferencedNodeName == "")
				 target = Root;
			 else
                target = Root->Find(ReferencedNodeName->Replace(".", "/"));
             for each (XmlNode^ Child in Instruction->ChildNodes)
             {
               if (Child->Name == "Memo")
               {
               // Ignore memo fields.
               }
               else if (!Child->HasChildNodes && Child->InnerText == "")
                  throw gcnew Exception("Cannot have a blank value for property: " + Child->Name);
               else if (Child->HasChildNodes)
               {
			      bool found = false;
				  // First look for a suitable field
				  FieldInfo^ field = target->GetType()->GetField(Child->Name, BindingFlags::Instance | BindingFlags::Public | BindingFlags::NonPublic);
				  if (field)
				  {
                     array<Object^>^ Attributes = field->GetCustomAttributes(false);
                     for each (Object^ Attr in Attributes)
                     {
                       if (dynamic_cast<Param^>(Attr) != nullptr)
					   {
				          found = true;
                          ReflectedField^ FieldToSet = gcnew ReflectedField(field, target);
	                      FieldToSet->Set(Child);
						  break;
					   }
					 }
				  } 
				  if (!found)
				  // Couldn't find a field; maybe it's a property
				  {
                     PropertyInfo^ property = target->GetType()->GetProperty(Child->Name, BindingFlags::Instance | BindingFlags::Public | BindingFlags::NonPublic);
					 if (property)
					 {
                        array<Object^>^ Attributes = property->GetCustomAttributes(false);
                        for each (Object^ Attr in Attributes)
                        {
                           if (dynamic_cast<Param^>(Attr) != nullptr)
					       {
						      found = true;
                              ReflectedProperty^ PropertyToSet = gcnew ReflectedProperty(property, target);
  					          PropertyToSet->Set(Child);
						 	  break;
						   }
						}
					 }
				  }
				  if (!found)
                     throw gcnew Exception("Could not find an overrideable parameter: " + Child->Name);
               }
            }
         }
      }
   }

