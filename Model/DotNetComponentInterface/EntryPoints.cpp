#include "StdAfx.h"

#using <mscorlib.dll>
#include <string>
#include <vector>
#include "Interfaces.h"

//#include "ComponentInterface.h"
#include "ApsimComponent.h"
using namespace ModelFramework;
using namespace std;   
using namespace System::Collections::Generic;
using namespace System;
using namespace System::Windows::Forms;
using namespace System::Collections;
using namespace System::Reflection;

   
ref class Instances
	{
	public:
		static ApsimComponent^ createNewInstance(const char* dllFileName,
												const unsigned int* compID,
												const unsigned int* parentID,
												unsigned int* instanceNumber,
												const unsigned int* callbackArg,
												CallbackType callback) 
			{
			String^ dll = gcnew String(dllFileName);
			if (System::IO::File::Exists(dll))
				{
				Assembly^ assembly = Assembly::LoadFrom(gcnew String(dllFileName));
				ApsimComponent^ Component = gcnew ApsimComponent(assembly, components->Count);
				Component->createInstance(dllFileName, *compID, *parentID, callbackArg, callback);
				components->Add(Component);

				return Component;
				}
			else
				{
				String^ msg = gcnew String("Cannot find dll: " + dll);
				System::Windows::Forms::MessageBox::Show(msg, "Error", System::Windows::Forms::MessageBoxButtons::OK);
				}

			return nullptr;
			}
		static ApsimComponent^ at(unsigned index)
			{
			return components[index];
			}
		
		static unsigned count() {return components->Count;}
	
		static List<ApsimComponent^>^ components = gcnew List<ApsimComponent^>();
		
	};

// ------------------------------------------------------------------
// Simple stub to be linked with fortran dlls. This will cause
// Computation::loadComponent(computation.cpp) to firstly load
// the fortran wrapper, followed by the fortran after.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport ) void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy_s(wrapperDll, 2, "");
   }


// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport )
void __stdcall getDescriptionInternal(char* initScript, char* description)
   {
   
   }
   
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport )
void __stdcall getDescription(const char* initScript, char* description)
   {
   try
      {
      XmlDocument^ Doc = gcnew XmlDocument();
      Doc->LoadXml(gcnew String(initScript));
      
      // Load the assembly.
      String^ ExecutableFileName = XmlHelper::Attribute(Doc->DocumentElement, "executable");
      Assembly^ Assemb = Assembly::LoadFile(ExecutableFileName);
      
      XmlNode^ InitData = XmlHelper::Find(Doc->DocumentElement, "initdata");
      if (InitData != nullptr)
         {
         ApsimComponent^ Comp = gcnew ApsimComponent(Assemb, 0);
         char* str = (char*)(void*)Marshal::StringToHGlobalAnsi(Comp->GetDescription(InitData));
         strcpy_s(description, 100000, str);
         Marshal::FreeHGlobal((IntPtr)str);
         }
      }
   catch (System::Exception^ err)
      {
      MessageBox::Show(err->Message);
      }
   }
// ------------------------------------------------------------------
// The PM is instructing us to create an instance of all our data.
// ------------------------------------------------------------------
extern "C"  __declspec( dllexport )
void __stdcall createInstance
   (const char* dllFileName,
    const unsigned int* compID,
    const unsigned int* parentID,
    unsigned int* instanceNumber,
    const unsigned int* callbackArg,
    CallbackType callback)
    {
	ApsimComponent^ c = Instances::createNewInstance(dllFileName, compID, parentID, instanceNumber, callbackArg, callback);
	*instanceNumber = Instances::count()-1;
	}
// ------------------------------------------------------------------
// The PM is instructing us to delete an instance of our data.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport )
void __stdcall deleteInstance (unsigned* instanceNumber)
   {
   ApsimComponent^ c = Instances::at(*instanceNumber);
   c->deleteInstance();
   }
// ------------------------------------------------------------------
// All messages to component go through here.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport )
void __stdcall messageToLogic (unsigned* instanceNumber, char* message, bool* processed)
   {
   ApsimComponent^ c = Instances::at(*instanceNumber);
   c->messageToLogic(message);
   *processed = true;
   }


extern "C" __declspec( dllexport )
unsigned __stdcall CallBack(unsigned instanceNumber, int DataHandle, char* messageData, int OpCode)
   {
   if (instanceNumber < Instances::count())
	  {
      ApsimComponent^ c = Instances::at(instanceNumber); //dph - bad.
      return c->CallBack(DataHandle, messageData, OpCode);
	  }
   else
	   return 0;
   }
