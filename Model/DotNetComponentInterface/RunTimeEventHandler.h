#pragma once

using namespace System;
using namespace CSGeneral;
using namespace System::Reflection;

#include "FactoryEventHandler.h"  
namespace ModelFramework
   {
public ref class RuntimeEventHandler : EvntHandler
   {
   // --------------------------------------------------------------------
   // A class for representing an event handler that has been subscribed
   // to at runtime. It is a wrapper around a method in an object.
   // --------------------------------------------------------------------
   public:
      delegate void NullFunction(void);
   
   private:
      NullFunction^ F;
   public:

      RuntimeEventHandler(String^ EventName, NullFunction^ F)
         : EvntHandler(EventName)
         {
         this->F = F;
         }
      property Type^ Typ 
         {
         virtual Type^ get() override
            {
            return nullptr;
            }
         }
      virtual void Invoke(Object^ Parameter) override
         {
         F->Invoke();
         }
      virtual void unpack(char* messageData) override
         {
         Invoke(nullptr);
         }
      virtual unsigned memorySize() override
         {
         return 0;
         }         
      virtual String^ DDML() override
         {
         return "<type/>";
         }

   };    
   }