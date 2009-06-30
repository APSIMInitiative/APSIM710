#pragma once

#using <CSGeneral.dll>
#include "Instance.h"
#include "ApsimAttributes.cpp"
#include "..\DataTypes\DOTNETDataTypes.h"
using namespace CSGeneral;
using namespace System::Reflection;
using namespace System::Xml;

public ref class ReflectedType abstract : NamedItem
   {
   // --------------------------------------------------------------------
   // This class and the next two encapsulate the differences between
   // a FieldInfo and a PropertyInfo. Why did Microsoft make these two
   // types different? .e.g the SetObject method have a different number
   // of parameters.
   // --------------------------------------------------------------------
   public:
      property Type^ Typ { virtual Type^ get() abstract;}
      property bool ReadOnly { virtual bool get() abstract;}
      property Object^ Get { virtual Object^ get() abstract;}
      property array<Object^>^ MetaData { virtual array<Object^>^ get() abstract;}
      virtual void SetObject(Object^ Value) abstract;
      void Set(String^ Value)
         {
         if (Typ->Name == "Double")
            SetObject(Convert::ToDouble(Value));
         else if (Typ->Name == "Int32")
            SetObject(Convert::ToInt32(Value));
         else if (Typ->Name == "Single")
            SetObject(Convert::ToSingle(Value));
         else if (Typ->Name == "String")
            SetObject(Value);
         else if (Typ->Name == "Double[]")
            {
            String^ Delimiters = " ";
            array<String^>^ StringValues = Value->Split(Delimiters->ToCharArray(), StringSplitOptions::RemoveEmptyEntries);
            array<double>^ Values = MathUtility::StringsToDoubles(StringValues);
            SetObject(Values);
            }
         else if (Typ->Name == "String[]")
            {
            String^ Delimiters = " ";
            array<String^>^ StringValues = Value->Split(Delimiters->ToCharArray(), StringSplitOptions::RemoveEmptyEntries);
            SetObject(StringValues);
            }            
         else
            throw gcnew Exception("Cannot set value of property: " + Name +
                                  ". Cannot convert: " + Value->ToString() + " to: " + Typ->Name);
         }
   };
public ref class ReflectedField : ReflectedType
   {
   private:
      FieldInfo^ Member;
      Object^ Obj;
   public:
      ReflectedField(FieldInfo^ Field, Object^ Obj)                             { Member = Field; this->Obj = Obj; }
      property String^ Name { virtual String^ get()override                     { return Member->Name; } }
      property Type^   Typ  { virtual Type^ get()override                       { return Member->FieldType; } }
      property bool ReadOnly { virtual bool get() override                      { return true; } }
      property Object^ Get  { virtual Object^ get()override                     { return Member->GetValue(Obj); } }
      property array<Object^>^ MetaData { virtual array<Object^>^ get()override { return Member->GetCustomAttributes(false); } }
      virtual void SetObject(Object^ Value)override                             { Member->SetValue(Obj, Value); }
   };
public ref class ReflectedProperty : ReflectedType
   {
   private:
      PropertyInfo^ Member;
      Object^ Obj;
   public:
      ReflectedProperty(PropertyInfo^ Field, Object^ Obj)                       { Member = Field; this->Obj = Obj; }
      property String^ Name { virtual String^ get()override                     { return Member->Name; } }
      property Type^   Typ  { virtual Type^ get()override                       { return Member->PropertyType; } }
      property bool ReadOnly { virtual bool get() override                      { return !Member->CanWrite; } }
      property Object^ Get  { virtual Object^ get()override                     { return Member->GetValue(Obj, nullptr); } }
      property array<Object^>^ MetaData { virtual array<Object^>^ get()override { return Member->GetCustomAttributes(false); } }
      virtual void SetObject(Object^ Value)override                             { Member->SetValue(Obj, Value, nullptr); }
   };

public ref class FactoryProperty : Instance, ApsimType
   {
   // --------------------------------------------------------------------
   // An class for representing all registerable properties
   // i.e. those properties visible to APSIM.
   // --------------------------------------------------------------------
   private:
      List<ReflectedField^>^ ChildFields;
      ReflectedType^ Property;
      ApsimType^ Data;
      ApsimType^ GetFieldWrapper();

      
   public:
      bool IsParam;
      bool IsInput;
      bool IsOutput;
      bool ReadOnly;
      bool HaveSet;
      String^ TypeName;
      String^ Units;
      String^ Description;
      String^ FQN;
      String^ OutputName;

      FactoryProperty(ReflectedType^ Property, XmlNode^ Parent);
      property bool HasAsValue {bool get() {return HaveSet;}}
      
      property Object^ Get { Object^ get() {return Property->Get;}}
      void Set(Object^ Value)
         {
         HaveSet = true;
         Property->SetObject(Value);
         }
      void Set(String^ Value)
         {
         HaveSet = true;
         Property->Set(Value);
         }
      void SetFromXML(XmlNode^ Node)
         {
         // make sure we have a value.
         if (Property->Get == nullptr)
            Property->SetObject(Activator::CreateInstance(Property->Typ));

         // See if this field has a ReadFromXML method. If so then call it.
         MethodInfo^ ReadFromXML = Property->Typ->GetMethod("ReadFromXML");
         if (ReadFromXML != nullptr)
            {
            array<Object^>^ Params = gcnew array<Object^>(1);
            Params[0] =  Node;
            ReadFromXML->Invoke(Property->Get, Params);
            }
         else
            {
            if (ChildFields == nullptr)
               {
               ChildFields = gcnew List<ReflectedField^>();

               for each (XmlNode^ Child in Node->ChildNodes)
                  {
                  FieldInfo^ Field = Property->Typ->GetField(Child->Name, BindingFlags::Instance | BindingFlags::Public | BindingFlags::NonPublic);
                  if (Field == nullptr)
                     throw gcnew Exception("Cannot find field: " + Child->Name + " in type: " + Property->Typ->Name);
                  ChildFields->Add(gcnew ReflectedField(Field, Property->Get));
                  }
               }
            for each (XmlNode^ Child in Node->ChildNodes)
               {
               bool Found = false;
               for each (ReflectedField^ Field in ChildFields)
                  {
                  if (Field->Name == Child->Name)
                     {
                     Field->Set(Child->InnerText);
                     Found = true;
                     }
                  }
               if (!Found)
                  throw gcnew Exception("Cannot find field: " + Child->Name + " in type:" + Property->Typ->Name);
               }
            }
         HaveSet = true;
         }

   
   
      virtual void pack(char* messageData)
         {
         Data->pack(messageData);
         }
      virtual void unpack(char* messageData)
         {
         Data->unpack(messageData);
         }
      virtual unsigned memorySize()
         {
         return Data->memorySize();
         }         
      virtual String^ DDML()
         {
         return Data->DDML();
         }
      String^ GetDescription();
  
   };

String^ CalcParentName(XmlNode^ Node);