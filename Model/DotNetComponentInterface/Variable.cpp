#pragma once
#include "Variable.h"
#include "MessageData.h"
#include "..\DataTypes\DOTNETDataTypes.h"
#include "ApsimComponent.h"
using namespace System;
using namespace System::Collections::Generic;
using namespace System::Text;
using namespace System::Runtime::InteropServices;
using namespace ModelFramework;


bool Variable::Exists()
   {
   WrapBuiltInVariable<String^>^ Data = gcnew WrapBuiltInVariable<String^>();
   return Component->Get(Name, Data, false);
   }
bool Variable::ToBoolean()
   {
   WrapBuiltInVariable<bool>^ Data = gcnew WrapBuiltInVariable<bool>();
   Component->Get(Name, Data, false);
   return Data->Value;
   }	
int Variable::ToInt32()
   {
   WrapBuiltInVariable<int>^ Data = gcnew WrapBuiltInVariable<int>();
   Component->Get(Name, Data, false);
   return Data->Value;
   }	
float Variable::ToSingle()
   {
   WrapBuiltInVariable<float>^ Data = gcnew WrapBuiltInVariable<float>();
   Component->Get(Name, Data, false);
   return Data->Value;
   }	   
double Variable::ToDouble()
   {
   WrapBuiltInVariable<double>^ Data = gcnew WrapBuiltInVariable<double>();
   Component->Get(Name, Data, false);
   return Data->Value;
   }	
String^ Variable::ToString()
   {
   WrapBuiltInVariable<String^>^ Data = gcnew WrapBuiltInVariable<String^>();
   Component->Get(Name, Data, false);
   return Data->Value;
   }	 
array<int>^ Variable::ToInt32Array()
   {
   WrapBuiltInVariable<array<int>^>^ Data = gcnew WrapBuiltInVariable<array<int>^>();
   Component->Get(Name, Data, false);
   return Data->Value;
   }	
array<float>^ Variable::ToSingleArray()
   {
   WrapBuiltInVariable<array<float>^>^ Data = gcnew WrapBuiltInVariable<array<float>^>();
   Component->Get(Name, Data, false);
   return Data->Value;
   }	     
array<double>^ Variable::ToDoubleArray()
   {
   WrapBuiltInVariable<array<double>^>^ Data = gcnew WrapBuiltInVariable<array<double>^>();
   Component->Get(Name, Data, false);
   return Data->Value;
   }
array<String^>^ Variable::ToStringArray()
   {
   WrapBuiltInVariable<array<String^>^>^ Data = gcnew WrapBuiltInVariable<array<String^>^>();
   Component->Get(Name, Data, false);
   return Data->Value;
   }
void Variable::Set(int Value)
   {
   WrapBuiltInVariable<int>^ Data = gcnew WrapBuiltInVariable<int>();
   Data->Value = Value;
   Component->Set(Name, Data);
   }
void Variable::Set(float Value)
   {
   WrapBuiltInVariable<float>^ Data = gcnew WrapBuiltInVariable<float>();
   Data->Value = Value;
   Component->Set(Name, Data);
   }
void Variable::Set(double Value)
   {
   WrapBuiltInVariable<double>^ Data = gcnew WrapBuiltInVariable<double>();
   Data->Value = Value;
   Component->Set(Name, Data);
   }
void Variable::Set(String^ Value)
   {
   WrapBuiltInVariable<String^>^ Data = gcnew WrapBuiltInVariable<String^>();
   Data->Value = Value;
   Component->Set(Name, Data);
   }
   
void Variable::Set(array<int>^ Values)
   {
   WrapBuiltInVariable<array<int>^>^ Data = gcnew WrapBuiltInVariable<array<int>^>();
   Data->Value = Values;
   Component->Set(Name, Data);
   }
void Variable::Set(array<float>^ Values)
   {
   WrapBuiltInVariable<array<float>^>^ Data = gcnew WrapBuiltInVariable<array<float>^>();
   Data->Value = Values;
   Component->Set(Name, Data);
   }
void Variable::Set(array<double>^ Values)
   {
   WrapBuiltInVariable<array<double>^>^ Data = gcnew WrapBuiltInVariable<array<double>^>();
   Data->Value = Values;
   Component->Set(Name, Data);
   }
void Variable::Set(array<String^>^ Values)
   {
   WrapBuiltInVariable<array<String^>^>^ Data = gcnew WrapBuiltInVariable<array<String^>^>();
   Data->Value = Values;
   Component->Set(Name, Data);
   }
	