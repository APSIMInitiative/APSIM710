#pragma once
using namespace System;

#include "..\DataTypes\DOTNETDataTypes.h"
public ref class FactoryEventHandler : ApsimType
   {
   // --------------------------------------------------------------------
   // An class for representing all registerable events
   // i.e. APSIM events that the model has subscribed to.
   // --------------------------------------------------------------------
   
   private:
      Object^ Obj;
      MethodInfo^ Method;
      array<Object^>^ Parameters;
      ApsimType^ Data;
   public:
      FactoryEventHandler(MethodInfo^ Method, Object^ Instance)
         {
         this->Method = Method;
         this->Obj = Instance;
         Parameters = gcnew array<Object^>(1);
         if (Typ == nullptr)
            Data = gcnew NullType();
         else
            Data = (ApsimType^) Activator::CreateInstance(Typ);
         }
      property String^ EventName { String^ get() {return Method->Name->Substring(2);}}
      property Type^ Typ 
         {
         Type^ get() 
            {
            if (Method->GetParameters()->Length == 0)
               return nullptr;
            else
               return Method->GetParameters()[0]->ParameterType;
            }
         }
      void Invoke(Object^ Parameter)
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
      virtual void pack(char* messageData)
         {
         throw gcnew Exception("Cannot call pack on an event handler");
         }         
      virtual void unpack(char* messageData)
         {
         Data->unpack(messageData);
         Invoke(Data);
         }
      virtual unsigned memorySize()
         {
         return Data->memorySize();
         }         
      virtual String^ DDML()
         {
         if (Typ == nullptr)
            return "<type/>";
         else
            return Data->DDML();
         }
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