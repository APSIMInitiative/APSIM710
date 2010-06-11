#pragma once
using namespace System;

#include "..\DataTypes\DOTNETDataTypes.h"
public ref class EvntHandler abstract : ApsimType
   {
   // --------------------------------------------------------------------
   // A class for representing all registerable events
   // i.e. APSIM events that the model has subscribed to.
   // --------------------------------------------------------------------
   private:
      String^ Name;

   public:
      EvntHandler(String^ EventName) {Name = EventName;}
      property String^ EventName { String^ get() {return Name;}}
      property Type^ Typ { virtual Type^ get() abstract;}
      virtual void Invoke(Object^ Parameter) abstract;
      virtual void pack(char* messageData)
         {
         throw gcnew Exception("Cannot call pack on an event handler");
         }         
      virtual void unpack(char* messageData) = 0;
      virtual unsigned memorySize() = 0;
      virtual String^ DDML() = 0;
      String^ GetDescription()
         {	
	      // ----------------------------------------------
	      // Creates a field wrapper for the given property.
	      // ----------------------------------------------
	      String^ Desc;
         Desc = "   <event name=\"" + EventName + "\" kind=\"subscribed\">\r\n";
         if (DDML() != "")
            Desc += "      " + DDML() + "\r\n";

         Desc += "   </event>\r\n";
         return Desc;
         }


   };
   
   
public ref class FactoryEventHandler : EvntHandler
   {
   // --------------------------------------------------------------------
   // A class for representing all registerable events
   // i.e. APSIM events that the model has subscribed to.
   // --------------------------------------------------------------------
   
   private:
      Object^ Obj;
      MethodInfo^ Method;
      array<Object^>^ Parameters;
      ApsimType^ Data;
   public:
      FactoryEventHandler(MethodInfo^ Method, Object^ Instance)
         : EvntHandler(Method->Name->Substring(2))
         {
         this->Method = Method;
         this->Obj = Instance;
         Parameters = gcnew array<Object^>(1);
         if (Typ == nullptr)
            Data = gcnew NullType();
         else
            Data = (ApsimType^) Activator::CreateInstance(Typ);
         }
      property Type^ Typ 
         {
         virtual Type^ get() override
            {
            if (Method->GetParameters()->Length == 0)
               return nullptr;
            else
               return Method->GetParameters()[0]->ParameterType;
            }
         }
      virtual void Invoke(Object^ Parameter) override
         {
         bool IsNullParameter = (dynamic_cast<NullType^>(Parameter)) != nullptr;
         if (Parameter == nullptr || IsNullParameter)
            Method->Invoke(Obj, nullptr);
         else
            {
            Parameters[0] = Parameter;
            Method->Invoke(Obj, Parameters);
            }
         }
      virtual void unpack(char* messageData) override
         {
         Data->unpack(messageData);
         Invoke(Data);
         }
      virtual unsigned memorySize() override
         {
         return Data->memorySize();
         }         
      virtual String^ DDML() override
         {
         if (Typ == nullptr)
            return "<type/>";
         else
            return Data->DDML();
         }

   };   
