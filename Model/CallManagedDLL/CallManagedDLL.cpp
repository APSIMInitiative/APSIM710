#include "stdafx.h"
#include "CallManagedDLL.h"

using namespace System;
using namespace System::Reflection;
using namespace System::Windows::Forms;
using namespace System::Collections::Generic;

	void CallDLLInternal(String^ fileName, String^ className, String^ methodName, String^ methodArgument)
		{	
		try
			{
			// load assembly and create an instance of class.
			Assembly^ assembly = Assembly::LoadFrom(fileName);
			Type^ t = assembly->GetType(className, true);
			Object^ dllObject = Activator::CreateInstance(t);

			// call required method.
			MethodInfo^ Method = t->GetMethod(methodName);
			if (Method == nullptr)
				MessageBox::Show("Cannot find method '" + methodName + "' in class '" + className + "'");
			else
				{
				array<Object^>^ methodArgs = gcnew array<Object^>(1);
				methodArgs[0] = methodArgument;
				Method->Invoke(dllObject, methodArgs);
				}
			}
		catch (Exception^ err)
			{
				MessageBox::Show("Error: " + err->GetBaseException()->Message);
			}
		}
extern "C" __declspec(dllexport) void __stdcall  CallDLL(const char* dllFileName, const char* className,
														 const char* methodName, const char* methodArgument)
	{
	CallDLLInternal(gcnew String(dllFileName), gcnew String(className), gcnew String(methodName), gcnew String(methodArgument));
	}
	
int Instances::CreateClass(String^ dllFileName, String^ className)
	{
	String^ dll = gcnew String(dllFileName);
	if (System::IO::File::Exists(dll))
		{
		Assembly^ assembly = Assembly::LoadFrom(gcnew String(dllFileName));
		array<Type^>^ Types = assembly->GetTypes();
		System::Collections::IEnumerator^ myEnum = Types->GetEnumerator();
		while (myEnum->MoveNext())
			{
			Type^ oType = safe_cast<Type^>(myEnum->Current);
			if (oType->Name == className)
				{
				Object^ o = Activator::CreateInstance(oType);
				objects->Add(o);
				return objects->Count-1;
				}
			}
		}
	else
		{
		String^ msg = gcnew String("Cannot find dll: " + dll);
		System::Windows::Forms::MessageBox::Show(msg, "Error", System::Windows::Forms::MessageBoxButtons::OK);
		}
	return -1;
	}

void Instances::CallMethod(unsigned Index, const char* methodName, Object^ argument)
	{
	String^ MethodName = gcnew String(methodName);
	Object^ o = objects[Index];
	MethodInfo^ Method = o->GetType()->GetMethod(MethodName);
	if (Method == nullptr)
		MessageBox::Show("Cannot find method '" + MethodName + "' in .NET class");
	else
		{
		array<Object^>^ methodArgs = gcnew array<Object^>(1);
		methodArgs[0] = argument;
		Method->Invoke(o, methodArgs);
		}
	}

extern "C" __declspec(dllexport) int __stdcall  CreateClass(const char* dllFileName, const char* className)
	{
	return Instances::CreateClass(gcnew String(dllFileName), gcnew String(className));
	}
	
