#pragma once

#include "Interfaces.h"
#include "..\DataTypes\DOTNETDataTypes.h"
#include "FactoryProperty.h"
#include "FactoryEventHandler.h"
#include <string>
#include <vcclr.h>
using namespace System::Runtime::InteropServices;
using namespace System::Reflection;
using namespace System::Collections::Specialized;

// --------------------------------------------------------------------------
// Raw message data support.
// --------------------------------------------------------------------------
[DllImport("ComponentInterface2.dll", EntryPoint = "CICreateMessageData", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
char* CreateMessageData(char* Message);

[DllImport("ComponentInterface2.dll", EntryPoint = "CIDeleteMessageData", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void DeleteMessageData(char* MessageData);


// --------------------------------------------------------------------------
// Boolean support
// --------------------------------------------------------------------------
[DllImport("ComponentInterface2.dll", EntryPoint = "CIPackBool", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void pack(char* messageData, Boolean Data);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackBool", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
bool unpack(char* messageData, Boolean% Data, String^ VariableName);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackBoolWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void unpackWithConverter(char* messageData, Boolean% Data);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIMemorySizeBool", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
unsigned memorySize(bool Data);
inline String^ DDML(Boolean Data)
	{
	return "<type kind=\"bool\"/>";
	}


// --------------------------------------------------------------------------
// Int32 support
// --------------------------------------------------------------------------
[DllImport("ComponentInterface2.dll", EntryPoint = "CIPackInt", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void pack(char* messageData, Int32 Data);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackInt", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
bool unpack(char* messageData, Int32% Data, String^ VariableName);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackIntWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void unpackWithConverter(char* messageData, Int32% Data);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIMemorySizeInt", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
unsigned memorySize(int Data);
inline String^ DDML(Int32 Data)
	{
	return "<type kind=\"integer4\"/>";
	}



// --------------------------------------------------------------------------
// Single support
// --------------------------------------------------------------------------
[DllImport("ComponentInterface2.dll", EntryPoint = "CIPackSingle", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void pack(char* messageData, Single Data);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackSingle", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
bool unpack(char* messageData, Single% Data, String^ VariableName);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackSingleWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void unpackWithConverter(char* messageData, Single% Data);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIMemorySizeSingle", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
unsigned memorySize(float Data);
inline String^ DDML(Single Data)
	{
	return "<type kind=\"single\"/>";
	}


// --------------------------------------------------------------------------
// Double support
// --------------------------------------------------------------------------
[DllImport("ComponentInterface2.dll", EntryPoint = "CIPackDouble", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void pack(char* messageData, Double Data);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackDouble", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
bool unpack(char* messageData, Double% Data, String^ VariableName);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackDoubleWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void unpackWithConverter(char* messageData, Double% Data);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIMemorySizeDouble", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
unsigned memorySize(double Data);
inline String^ DDML(Double Data)
	{
	return "<type kind=\"double\"/>";
	}


// --------------------------------------------------------------------------
// String support
// --------------------------------------------------------------------------
[DllImport("ComponentInterface2.dll", EntryPoint = "CIPackString", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void packRaw(char* messageData, String^ Data);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackString", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
bool unpack(char* messageData, System::Text::StringBuilder^ Data, String^ VariableName);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackStringWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void unpackWithConverter(char* messageData, System::Text::StringBuilder^ Data);
[DllImport("ComponentInterface2.dll", EntryPoint = "CIMemorySizeString", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
unsigned memorySize(String^ Data);
inline String^ DDML(String^ Data)
	{
	return "<type kind=\"string\"/>";
	}
inline void pack(char* messageData, String^ Data)
	{
	::packRaw(messageData, Data);
	}
inline void unpack(char* messageData, String^% st, String^ VariableName)
	{
	System::Text::StringBuilder^ Contents = gcnew System::Text::StringBuilder(5000);
	::unpack(messageData, Contents, VariableName);
	st = Contents->ToString();
	}
inline void unpackWithConverter(char* messageData, String^% st)
	{
	System::Text::StringBuilder^ Contents = gcnew System::Text::StringBuilder(5000);
	::unpackWithConverter(messageData, Contents);
	st = Contents->ToString();
	}

// --------------------------------------------------------------------------
// Int32 array support
// --------------------------------------------------------------------------
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackIntArrayWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void unpackWithConverter(char* messageData, Int32 Data[], int& numValues);

// --------------------------------------------------------------------------
// Single array support
// --------------------------------------------------------------------------
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackSingleArrayWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void unpackWithConverter(char* messageData, Single Data[], int& numValues);

// --------------------------------------------------------------------------
// Double array support
// --------------------------------------------------------------------------

[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackDoubleArrayWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void unpackWithConverter(char* messageData, Double Data[], int& numValues);

// --------------------------------------------------------------------------
// String array support
// --------------------------------------------------------------------------
[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackStringArrayWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void unpackStringArrayWithConverter(char* messageData, System::Text::StringBuilder^ Data);


// --------------------------------------------------------------------------
// General array support.
// --------------------------------------------------------------------------
template <class T>
void pack(char* messageData, array<T>^ values)
   {
   ::pack(messageData, values->Length);
   for (int i = 0; i < values->Length; i++)
	  ::pack(messageData, values[i]);
   }
template <class T>
void unpack(char* messageData, array<T>^% values, String^ VariableName)
   {
   int count = 0;
   ::unpack(messageData, count, "");
   values = gcnew array<T>(count);
   for (int i = 0; i < count; i++)
	  ::unpack(messageData, values[i], VariableName);
   }
template <class T>
inline void unpackWithConverter(char* messageData, array<T>^% values)
   {
   T data[1000];
   int count = 0;
   ::unpackWithConverter(messageData, data, count);
   values->Resize(values, count);
   for (int i = 0; i < count; i++)
      values[i] = data[i];
   }
template <> // template specialisation for String^
inline void unpackWithConverter(char* messageData, array<String^>^% values)
   {
	System::Text::StringBuilder^ data = gcnew System::Text::StringBuilder(5000);
   ::unpackStringArrayWithConverter(messageData, data);
   StringCollection^ allValues = CSGeneral::StringManip::SplitStringHonouringQuotes(data->ToString(), ",");
   values->Resize(values, allValues->Count);
   for (int i = 0; i < allValues->Count; i++)
      values[i] = allValues[i]->Replace("\"","");
   }
template <class T>
void unpackArrayOfStructures(char* messageData, array<T^>^% values)
   {
   int count = 0;
   ::unpack(messageData, count, "");
   values = gcnew array<T^>(count);
   for (int i = 0; i < count; i++)
	  {
	  values[i] = gcnew T;
	  ::unpack(messageData, values[i]);
	  }
   }
template <class T>
unsigned memorySize(array<T>^ values)
   {
   return 4 + values->Length * ::memorySize(values[0]);
   }
template <class T>
String^ DDML(array<T>^ values)
	{
	T Dummy;
	String^ TypeString = ::DDML(Dummy);
	return TypeString->Substring(0, TypeString->Length-2) + " array=\"T\"/>";
	}

	   


	
		