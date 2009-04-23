#pragma once
using namespace System;
using namespace System::Reflection;

#include "..\DataTypes\DOTNETDataTypes.h"


public ref class FactoryEvent : ApsimType
   {
   // --------------------------------------------------------------------
   // An class for representing all registerable events that will be
   // published to other APSIM components.
   // --------------------------------------------------------------------
   
   private:
      Object^ Obj;
      EventInfo^ Event;
   public:
      ApsimType^ Data;
      int ID;
      delegate void FactoryEventFired(FactoryEvent^ Event);
      event FactoryEventFired^ OnFired;

      FactoryEvent(EventInfo^ Event, Object^ Instance);
      property String^ EventName { String^ get() {return Event->Name;}}
      
      void NullHandler();
      void Handler(ApsimType^ Data);
      property Type^ Typ 
         {
         Type^ get() 
            {
            array<ParameterInfo^>^ Params = Event->EventHandlerType->GetMethod("Invoke")->GetParameters();
            if (Params->Length == 0)
               return nullptr;
            else
               return Params[0]->ParameterType;
            }
         }
      virtual void pack(char* messageData)
         {
         Data->pack(messageData);         
         }
      virtual void unpack(char* messageData)
         {
         throw gcnew Exception("Cannot call unpack on an event.");
         }
      virtual unsigned memorySize()
         {
         return Data->memorySize();
         }         
      virtual String^ DDML()
         {
         if (Data != nullptr)
            return Data->DDML();
         return "";
         }
      String^ GetDescription();

   };