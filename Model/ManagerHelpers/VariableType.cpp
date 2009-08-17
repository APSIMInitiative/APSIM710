#pragma once
#include "VariableType.h"
#include <DOTNETComponentInterface\MessageData.h>
using namespace System;
using namespace System::Collections::Generic;
using namespace System::Text;
using namespace System::Runtime::InteropServices;
using namespace ModelFramework;

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

int VariableType::ToBoolean()
   {
   WrapBuiltInVariable<bool>^ Data = gcnew WrapBuiltInVariable<bool>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }	
int VariableType::ToInt32()
   {
   WrapBuiltInVariable<int>^ Data = gcnew WrapBuiltInVariable<int>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }	
float VariableType::ToSingle()
   {
   WrapBuiltInVariable<float>^ Data = gcnew WrapBuiltInVariable<float>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }	   
double VariableType::ToDouble()
   {
   WrapBuiltInVariable<double>^ Data = gcnew WrapBuiltInVariable<double>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }	
String^ VariableType::ToString()
   {
   WrapBuiltInVariable<String^>^ Data = gcnew WrapBuiltInVariable<String^>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }	 
array<int>^ VariableType::ToInt32Array()
   {
   WrapBuiltInVariable<array<int>^>^ Data = gcnew WrapBuiltInVariable<array<int>^>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }	
array<float>^ VariableType::ToSingleArray()
   {
   WrapBuiltInVariable<array<float>^>^ Data = gcnew WrapBuiltInVariable<array<float>^>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }	     
array<double>^ VariableType::ToDoubleArray()
   {
   WrapBuiltInVariable<array<double>^>^ Data = gcnew WrapBuiltInVariable<array<double>^>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }
array<String^>^ VariableType::ToStringArray()
   {
   WrapBuiltInVariable<array<String^>^>^ Data = gcnew WrapBuiltInVariable<array<String^>^>();
   Component->Get(ComponentName + "." + Name, Data);
   return Data->Value;
   }
	
void VariableType::Set(int Value)
   {
   WrapBuiltInVariable<int>^ Data = gcnew WrapBuiltInVariable<int>();
   Data->Value = Value;
   Component->Set(ComponentName + "." + Name, Data);
   }
void VariableType::Set(float Value)
   {
   WrapBuiltInVariable<float>^ Data = gcnew WrapBuiltInVariable<float>();
   Data->Value = Value;
   Component->Set(ComponentName + "." + Name, Data);
   }
void VariableType::Set(double Value)
   {
   WrapBuiltInVariable<double>^ Data = gcnew WrapBuiltInVariable<double>();
   Data->Value = Value;
   Component->Set(ComponentName + "." + Name, Data);
   }
void VariableType::Set(String^ Value)
   {
   WrapBuiltInVariable<String^>^ Data = gcnew WrapBuiltInVariable<String^>();
   Data->Value = Value;
   Component->Set(ComponentName + "." + Name, Data);
   }
   
void VariableType::Set(array<int>^ Values)
   {
   WrapBuiltInVariable<array<int>^>^ Data = gcnew WrapBuiltInVariable<array<int>^>();
   Data->Value = Values;
   Component->Set(ComponentName + "." + Name, Data);
   }
void VariableType::Set(array<float>^ Values)
   {
   WrapBuiltInVariable<array<float>^>^ Data = gcnew WrapBuiltInVariable<array<float>^>();
   Data->Value = Values;
   Component->Set(ComponentName + "." + Name, Data);
   }
void VariableType::Set(array<double>^ Values)
   {
   WrapBuiltInVariable<array<double>^>^ Data = gcnew WrapBuiltInVariable<array<double>^>();
   Data->Value = Values;
   Component->Set(ComponentName + "." + Name, Data);
   }
void VariableType::Set(array<String^>^ Values)
   {
   WrapBuiltInVariable<array<String^>^>^ Data = gcnew WrapBuiltInVariable<array<String^>^>();
   Data->Value = Values;
   Component->Set(ComponentName + "." + Name, Data);
   }
	