#pragma once

#include "..\DataTypes\DOTNETDataTypes.h"
#include "ApsimComponent.h"
#include "MessageData.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace System::Collections::Specialized;
using namespace System::Runtime::InteropServices;
using namespace CSGeneral;


public ref class GenericType : ApsimType
   {
   private:
      List<ApsimType^>^ Fields;
      String^ TypeDDML;

   public:
      GenericType()
         {
         Fields = gcnew List<ApsimType^>();
         }

      void Add(ApsimType^ Field) { Fields->Add(Field); }

      virtual String^ DDML() {return TypeDDML;}

      void SetDDML(String^ value) {TypeDDML = value;}

      virtual unsigned memorySize()
         {
         int Size = 0;
         for each (ApsimType^ Field in Fields)
            Size += Field->memorySize();
         return Size;
         }

      virtual void pack(char* MessageData)
         {
         for each (ApsimType^ Field in Fields)
            Field->pack(MessageData);
         }

      virtual void unpack(char* MessageData)
         {
         for each (ApsimType^ Field in Fields)
            Field->unpack(MessageData);
         }

   };