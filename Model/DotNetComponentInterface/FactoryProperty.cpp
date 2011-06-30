using namespace System;
using namespace System::Collections::Generic;
using namespace System::Text;
using namespace System::Reflection;
using namespace System::Xml;

#include "FactoryProperty.h"
#include "MessageData.h"

template <class T>
ref class WrapBuiltIn : ApsimType
   {
   // --------------------------------------------------------------------
   // This class wraps a FactoryProperty and a built in type (e.g. Single, 
   // Double etc). It then makes it look like an ApsimType with pack,
   // unpack methods etc.
   // --------------------------------------------------------------------
   private:
      FactoryProperty^ Property;
   public:
      WrapBuiltIn(FactoryProperty^ Property)
         {
         this->Property = Property;
         }
		virtual void pack(char* messageData)
			{
			T Data = (T) Property->Get;
			::pack(messageData, (T) Data);
			}
		virtual void unpack(char* messageData)
			{
			T Data = (T) Property->Get;
			::unpackWithConverter(messageData, Data);
			Property->Set(Data);
			}
		virtual unsigned memorySize()
			{
			T Data = (T) Property->Get;
			return ::memorySize((T) Data);
			}
      virtual String^ DDML()
         {
			T Data;
         return ::DDML(Data);
         }
	};
	
ref class WrapApsimType : ApsimType
   {
   // --------------------------------------------------------------------
   // This class wraps a FactoryProperty and a built in type (e.g. Single, 
   // Double etc). It then makes it look like an ApsimType with pack,
   // unpack methods etc.
   // --------------------------------------------------------------------
   private:
      FactoryProperty^ Property;
   public:
      WrapApsimType(FactoryProperty^ Property)
         {
         this->Property = Property;
         }
		virtual void pack(char* messageData)
			{
			ApsimType^ Data = (ApsimType^) Property->Get;
			Data->pack(messageData);
			}
		virtual void unpack(char* messageData)
			{
			ApsimType^ Data = (ApsimType^) Property->Get;
			Data->unpack(messageData);
			Property->Set(Data);
			}
		virtual unsigned memorySize()
			{
			ApsimType^ Data = (ApsimType^) Property->Get;
			return Data->memorySize();
			}
      virtual String^ DDML()
         {
			ApsimType^ Data = dynamic_cast<ApsimType^> (Property->Get);
			if (Data == nullptr)
			   return "";
         return Data->DDML();
         }
	};   
   
FactoryProperty::FactoryProperty(ReflectedType^ Property, XmlNode^ Parent)
   {
   // --------------------------------------------------------------------
   // Constructor
   // --------------------------------------------------------------------
   IsParam = false;
   IsInput = false;
   IsOutput = false;
   HaveSet = false;
   ReadOnly = Property->ReadOnly;
   TypeName = Property->Typ->Name;
   Units = "";
   Description = "";
   this->Property = Property;
   this->Name = Property->Name;
   
   FQN = CalcParentName(Parent) + this->Name;   
   this->OutputName = FQN;
   Data = GetFieldWrapper();
   sDDML = Data->DDML();
   regIndex = -1;
   
   for each (Object^ Attr in Property->MetaData)
      {
      Param^ P  = dynamic_cast<Param^> (Attr);
      Output^ O = dynamic_cast<Output^> (Attr);
      Input^ I  = dynamic_cast<Input^> (Attr);
      ::Units^ U  = dynamic_cast<::Units^> (Attr);
      ::Description^ D  = dynamic_cast<::Description^> (Attr);
      if (P != nullptr)
         {
         IsParam = true;
		 OptionalParam = P->Optional;
		 ParamMinVal = P->MinVal;
		 ParamMaxVal = P->MaxVal;
         if (P->Name != "")
            {
            Name = P->Name;
            FQN = CalcParentName(Parent) + this->Name;
            }
         }
      else if (I != nullptr)
         {
         IsInput = true;
         OptionalInput = I->Optional;
         }
      else if (O != nullptr)
         {
         IsOutput = true;
         if (O->Name != "")
            OutputName = O->Name;
         }
      else if (U != nullptr)
         Units = U->ToString();
      else if (D != nullptr)
         Description = D->ToString();
      }

   }
ApsimType^ FactoryProperty::GetFieldWrapper()
   {	
	// ----------------------------------------------
	// Creates a field wrapper for the given property.
	// ----------------------------------------------
   
   if (TypeName == "Single")
      return gcnew WrapBuiltIn<Single>(this);
   else if (TypeName == "Double")
      return gcnew WrapBuiltIn<Double>(this);
   else if (TypeName == "Int32")
      return gcnew WrapBuiltIn<Int32>(this);
   else if (TypeName == "String")
      return gcnew WrapBuiltIn<String^>(this);
   else if (TypeName == "Single[]")
      return gcnew WrapBuiltIn<array<Single>^>(this);
   else if (TypeName == "Double[]")
      return gcnew WrapBuiltIn<array<Double>^>(this);
   else if (TypeName == "Int32[]")
      return gcnew WrapBuiltIn<array<Int32>^>(this);
   else if (TypeName == "String[]")
      return gcnew WrapBuiltIn<array<String^>^>(this);
   else if (TypeName == "DateTime")
      return gcnew WrapBuiltIn<DateTime>(this);
	else
	   return gcnew WrapApsimType(this);
   }	
   
String^ CalcParentName(XmlNode^ Node)
   {
   // ----------------------------------------------
   // Calculate a parent name
   // ----------------------------------------------

   String^ ParentName = "";
   if (Node == nullptr || Node->ParentNode == nullptr || Node->ParentNode->ParentNode == nullptr)
      return "";
   
   ParentName = CalcParentName(Node->ParentNode);
   return ParentName + XmlHelper::Name(Node);
   }   
   
   
String^ FactoryProperty::GetDescription()
   {	
	// ----------------------------------------------
	// Creates a field wrapper for the given property.
	// ----------------------------------------------
	String^ Desc;
	if (IsOutput)
	   {
      Desc = "   <property name=\"" + OutputName + "\" access=\"";
      if (ReadOnly)
         Desc += "read";
      else
         Desc += "both";
      Desc += "\"  init=\"F\">\r\n";
      if (DDML() != "")
         {
         XmlDocument^ Doc = gcnew XmlDocument();
         Doc->LoadXml(DDML());
         if (Units != "")
            XmlHelper::SetAttribute(Doc->DocumentElement, "unit", Units);
         if (Description != "")
            XmlHelper::SetAttribute(Doc->DocumentElement, "description", Description);
         Desc += "      " + Doc->DocumentElement->OuterXml + "\r\n";
         }
      Desc += "   </property>\r\n";
      }
   else if (IsInput)
	   {
      Desc = "   <driver name=\"" + OutputName + "\">\r\n";
      if (DDML() != "")
         {
         XmlDocument^ Doc = gcnew XmlDocument();
         Doc->LoadXml(DDML());
         if (Units != "")
            XmlHelper::SetAttribute(Doc->DocumentElement, "unit", Units);
         if (Description != "")
            XmlHelper::SetAttribute(Doc->DocumentElement, "description", Description);
         Desc += "      " + Doc->DocumentElement->OuterXml + "\r\n";
         }
      Desc += "   </driver>\r\n";
      }
   
   return Desc;
   }
   