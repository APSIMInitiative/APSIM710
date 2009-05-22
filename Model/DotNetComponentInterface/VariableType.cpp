#pragma once
#include "VariableType.h"
#include "MessageData.h"
#include "ApsimComponent.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace System::Text;
using namespace System::Runtime::InteropServices;
using namespace ModelFramework;

[DllImport("ComponentInterface2.dll", EntryPoint = "CIGet", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void CIGet(int ComponentInterface, String^ Name, String^ Units, bool Optional, void* Handler,
		     int InstanceNumber, int RegistrationIndex, String^ ddml);

template <class T>
ref class WrapBuiltInVariable : ApsimType
   {
   // --------------------------------------------------------------------
   // This class wraps a FactoryProperty and a built in type (e.g. Single, 
   // Double etc). It then makes it look like an ApsimType with pack,
   // unpack methods etc.
   // --------------------------------------------------------------------
   public:
      T Value;
      WrapBuiltInVariable()
         {
         }
		virtual void pack(char* messageData)
			{
			::pack(messageData, (T) Value);
			}
		virtual void unpack(char* messageData)
			{
			::unpackWithConverter(messageData, Value);
			}
		virtual unsigned memorySize()
			{
			return ::memorySize((T) Value);
			}
      virtual String^ DDML()
         {
         return ::DDML(Value);
         }
	};

double VariableType::ToDouble()
   {
   WrapBuiltInVariable<double>^ Data = gcnew WrapBuiltInVariable<double>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }	
array<double>^ VariableType::ToDoubleArray()
   {
   WrapBuiltInVariable<array<double>^>^ Data = gcnew WrapBuiltInVariable<array<double>^>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }
	
	
void VariableType::Set(double Value)
   {
   WrapBuiltInVariable<double>^ Data = gcnew WrapBuiltInVariable<double>();
   Data->Value = Value;
   Component->Set(ComponentName + "." + Name, Data);
   }
void VariableType::Set(array<double>^ Values)
   {
   WrapBuiltInVariable<array<double>^>^ Data = gcnew WrapBuiltInVariable<array<double>^>();
   Data->Value = Values;
   Component->Set(ComponentName + "." + Name, Data);
   }
	