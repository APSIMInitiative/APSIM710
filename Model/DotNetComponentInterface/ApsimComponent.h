#pragma once
#using <System.dll>
#using <System.Xml.dll>
#include <vector>
#include "..\DataTypes\DOTNETDataTypes.h"
#include "Instance.h"
#include "Factory.h"
using namespace System::Reflection;
using namespace System::Xml;
typedef  void  (__stdcall *CallbackType)(const unsigned *callbackArg, char *message);

namespace ModelFramework {
public ref class ApsimComponent
	{
	private:
		Instance^ Model;
		int ComponentI;
		System::Text::StringBuilder^ Contents;	
		Assembly^ modelAssembly;
		bool Init1;
		bool Init2Received;
		Factory^ Fact;
		int instanceNumber;
		List<ApsimType^>^ Registrations;
		XmlNode^ InitData;
		bool EndCropToday;
		
		void RegisterAllProperties(Factory^ F);
		void RegisterAllEventHandlers(Factory^ F);
		void TrapAllEvents(Factory^ F);
      void OnPublish(FactoryEvent^ Event);
		void GetAllInputs();
		void BuildObjects(XmlNode^ XML);
		void PerformInstructions(XmlNode^ Node, XmlNode^% ModelToBuild);
      void InsertParameterIntoModel(XmlNode^ Parameter, XmlNode^ ModelDescription);
      void OnSow(char* messageData);
      void OnEndCrop(char* messageData);
      void OnPost(char* messageData);
      Assembly^ CompileScript(XmlNode^ Node);

      
      void CallEventHandlers(String^ EventName, SowType^ Data);

	internal:
		void Warning(String^ msg);

	public:
		ApsimComponent(Assembly^ ModelAssembly, int instanceNumber);

		// --------------------------------
		// Called by entry point.
		// --------------------------------
		void createInstance(const char* dllFileName,
							unsigned compID,
							unsigned parentID,
							const unsigned* callbackArg,
							CallbackType callback);
		
		// --------------------------------
		// Called by entry point.
		// --------------------------------
		void deleteInstance ();

		// --------------------------------
		// Called by entry point.
		// --------------------------------
		void messageToLogic (char* message);
		
		// --------------------------------
		// Called by entry point.
		// --------------------------------		
      unsigned CallBack(int DataHandle, char* messageData, int OpCode);
      
		// -----------------------------------------------
		// Called by probe tool to return info about model
		// -----------------------------------------------
      String^ GetDescription(XmlNode^ InitData);

	};	
	
	
	

// -----------------------------------
// Create an instance of the component
// -----------------------------------

ApsimComponent^ createInstanceOfComponent(const std::string& dllFileName);
	
};