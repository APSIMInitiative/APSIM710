#include "stdafx.h"
#include "ApsimComponent.h"
#include "MessageData.h"
#include "Factory.h"
#using <System.dll>
using namespace System::CodeDom::Compiler;
using namespace System::Runtime::InteropServices;
using namespace System::IO;
using namespace System::Diagnostics;
using namespace ModelFramework;
using namespace CSGeneral;

extern "C" {
	[DllImport("ComponentInterface2.dll", EntryPoint = "CICreate", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	int CICreate(const unsigned* callbackarg, CallbackType callback, unsigned componentid, unsigned parentid, String^ dllName);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIDelete", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	int CIDelete(int ComponentInterface);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIMessageToLogic", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	int CIMessageToLogic(int ComponentInterface, char* Message);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIDeRegister", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	int CIDeRegister(int ComponentInterface);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CISubscribe", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CISubscribe(int ComponentInterface, String^ Name, void* Handler, int InstanceNumber, int RegistrationIndex, String^ ddml);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIError", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIError(int ComponentInterface, String^ Message, bool IsFatal);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIGet", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	bool CIGet(int ComponentInterface, String^ Name, String^ Units, bool Optional, void* Handler,
			     int InstanceNumber, int RegistrationIndex, String^ ddml);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CISet", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CISet(int ComponentInterface, String^ Name, String^ Units, void* Handler,
			     int InstanceNumber, int RegistrationIndex, String^ ddml);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIExpose", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIExpose(int ComponentInterface, String^ Name, String^ Units, String^ Description, bool Writable, void* Handler,
			        int InstanceNumber, int RegistrationIndex, String^ ddml);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIPublish", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIPublish(int ComponentInterface, String^ Name, void* Handler,
			         int InstanceNumber, int RegistrationIndex, String^ ddml);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIWrite", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIWrite(int ComponentInterface, String^ Line);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIWarning", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIWarning(int ComponentInterface, String^ Line);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CICreateMessageCopy", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	char* CICreateMessageCopy(char* Message);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIDeleteMessageCopy", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIDeleteMessageCopy(char* Message);

}

extern "C" __declspec( dllexport )
unsigned __stdcall CallBack(int instanceNumber, int DataHandle, char* messageData, int OpCode);

#define INIT2INDEX -1
#define SOWINDEX -2
#define ENDCROPINDEX -3
#define POSTINDEX -4
ApsimComponent::ApsimComponent(Assembly^ ModelAssembly, int instancenumber)
	{
	// --------------------------------
	// constructor
	// --------------------------------
	Init1 = true;
   Init2Received = false;
   EndCropToday = false;
	instanceNumber = instancenumber;
	modelAssembly = ModelAssembly;
	Registrations = gcnew List<ApsimType^>();
	DllFileName = ModelAssembly->Location;
	}
void ApsimComponent::createInstance(const char* dllFileName,
					unsigned compID,
					unsigned parentID,
					const unsigned* callbackArg,
					CallbackType callback)
	{
	// -----------------------------------
	// Called by APSIM to create ourselves
	// -----------------------------------
	Contents = gcnew StringBuilder(100000);
        ComponentID = compID;

        // Load in the reflectiion dll in case our host component want's to use it.
        DllFileName = gcnew String(dllFileName);

	ComponentI = CICreate(callbackArg, callback, compID, parentID, DllFileName);
	}

void ApsimComponent::deleteInstance()
	{
   // -----------------------------------
   // Called by APSIM to delete ourselves
   // -----------------------------------
	CIDelete(ComponentI);
	}

void ApsimComponent::messageToLogic (char* message)
	{
   // --------------------------------------
   // Called by APSIM to perform some action
   // --------------------------------------
	try
	   {
	   if (Init1)
		   {
		   char* msgCopy = CICreateMessageCopy(message);
 		   Init1 = false;

		   char* messageData = CreateMessageData(message);
		   ::unpack(messageData, Contents, "");
		   ::unpack(messageData, Name, "");
		   DeleteMessageData(messageData);

		   CISubscribe(ComponentI, "Init2", &::CallBack, instanceNumber, INIT2INDEX, "<type/>");

		   XmlDocument^ Doc = gcnew XmlDocument();
		   Doc->LoadXml(Contents->ToString());
		   InitData = XmlHelper::Find(Doc->DocumentElement, "initdata");
   		bool IsPlant = (XmlHelper::FindByType(InitData,"Plant") != nullptr);
   		bool IsScript = (XmlHelper::FindByType(InitData,"text") != nullptr);   		
   		if (IsScript)
   		   {
   		   String^ ScriptClassName = "";
   		   Assembly^ CompiledAssembly = CompileScript(XmlHelper::FindByType(InitData,"text"));
   		   modelAssembly = CompiledAssembly;
   		   for each (Type^ t in CompiledAssembly->GetTypes())
   		      {
   		      if (t->BaseType != nullptr && t->BaseType->Name == "Instance")
   		         ScriptClassName = t->Name;
   		      }
   		   if (ScriptClassName == "")
   		      throw gcnew Exception("Cannot find a script class inherited from 'Instance'");
   		      
   		   // Create an XML model that we can pass to BuildObjects.
   		   XmlDocument^ NewDoc = gcnew XmlDocument();
   		   XmlNode^ ScriptNode = NewDoc->AppendChild(NewDoc->CreateElement(ScriptClassName));
            XmlNode^ UINode = XmlHelper::Find(InitData, "ui");
            if (UINode != nullptr)
               {
   		      for each (XmlNode^ Child in XmlHelper::ChildNodes(UINode, ""))
                  {
                  if (XmlHelper::Attribute(Child, "type")->ToLower() != "category")
                     XmlHelper::SetValue(ScriptNode, Child->Name, Child->InnerText);   		      
                  }
               }

            // Build all necessary objects
            BuildObjects(ScriptNode);
   		   }
   		else if (IsPlant)
		      {
            SowPlant2Type^ sowDummy = gcnew SowPlant2Type();
            CISubscribe(ComponentI, "Sow", &::CallBack, instanceNumber, SOWINDEX, sowDummy->DDML());
            CISubscribe(ComponentI, "EndCrop", &::CallBack, instanceNumber, ENDCROPINDEX, "<type/>");
            CISubscribe(ComponentI, "Post", &::CallBack, instanceNumber, POSTINDEX, "<type/>");
		      }
		   else
   		   {
            // Insert any other parameters found under the <InitData> section into the model description.
            for each (XmlNode^ Parameter in InitData->ChildNodes)
               {
               if (XmlHelper::ChildNodes(Parameter, "")->Count == 0)
                  {
                  // simple parameter - insert into model description
                  InsertParameterIntoModel(Parameter, InitData->ChildNodes[0]);
                  }
               }
   		     BuildObjects(InitData->ChildNodes[0]);
   		   }
		   CIMessageToLogic(ComponentI, msgCopy);
		   CIDeleteMessageCopy(msgCopy);
		   }
	   else
	     CIMessageToLogic(ComponentI, message);
	   }
	catch (System::Exception^ err)
		{
	   CIError(ComponentI, err->GetBaseException()->Message, true);
		}
	}
void ApsimComponent::Warning(String^ Line)
	{
	CIWarning(ComponentI, Line);
	}

unsigned ApsimComponent::CallBack(int RegistrationIndex, char* messageData, int OpCode)
	{
	// --------------------------------------------------------------------------
	// This method is called by the c++ ComponentInterface2 DLL to perform some
	// action.
	//    When OpCode = 1, we need to pack message data.
	//    When OpCode = 2, we need to unpack message data.
	//    When OpCode = 3, we need to return the size of the data.
	// The RegistrationIndex is the index into the Registrations list.
	// --------------------------------------------------------------------------

	try
		{
		if (RegistrationIndex == INIT2INDEX)
		{
		   Init2Received = true;
           if (Model != nullptr)
		   {
			Console::WriteLine();
			Console::Write("------- " + Model->Name + " Initialisation ");
			int col = Model->Name->Length + 24;
			while (++col < 80)
			   Console::Write("-");
			Console::WriteLine();
		   }
		}
		else if (RegistrationIndex == SOWINDEX)
		   OnSow(messageData);
		else if (RegistrationIndex == ENDCROPINDEX)
		   OnEndCrop(messageData);
		else if (RegistrationIndex == POSTINDEX)
		   OnPost(messageData);
		else if (OpCode == 1)
 			Registrations[RegistrationIndex]->pack(messageData);
		else if (OpCode == 2)
		   {
		   bool IsEvent = dynamic_cast<EvntHandler^>(Registrations[RegistrationIndex]) != nullptr;
		   if (IsEvent && Init2Received)
	   	   GetAllInputs();
           Registrations[RegistrationIndex]->unpack(messageData);
			}
		else if (OpCode == 3)
			return Registrations[RegistrationIndex]->memorySize();
		}
	catch (System::Exception^ err)
		{
		CIError(ComponentI, err->GetBaseException()->Message, true);
		}
	return 0;
	}

void ApsimComponent::RegisterAllProperties(Factory^ F)
	{
	// ----------------------------------------------
	// Look for all properties and register them.
	// ----------------------------------------------

	for (int i = 0; i != F->Properties->Count; i++)
	   {
	   FactoryProperty^ Property = F->Properties[i];
	   if (Property->IsOutput)
	      {
	      int RegistrationIndex = Registrations->Count;
			Registrations->Add(Property);
			CIExpose(ComponentI, Property->OutputName, Property->Units, Property->Description,
			         !Property->ReadOnly, &::CallBack, instanceNumber, RegistrationIndex, Property->DDML());
			}
	   }
	}

void ApsimComponent::RegisterAllEventHandlers(Factory^ F)
	{
	// ----------------------------------------------
	// Look for all event handlers and register them.
	// ----------------------------------------------
	for (int i = 0; i != F->EventHandlers->Count; i++)
	   Subscribe(F->EventHandlers[i]);
   }


void ApsimComponent::GetAllInputs()
   {
	// ----------------------------------------------
	// Go through all [input] variables in model
	// and get the most recent values from APSIM
	// ----------------------------------------------
	for (int i = 0; i != Fact->Properties->Count; i++)
	   {
	   FactoryProperty^ Property = Fact->Properties[i];
	   if (Property->IsInput)
	      {
			  if (Property->regIndex == -1) 
			  { 
			    Property->regIndex = Registrations->Count;
				Registrations->Add(Property);
			  }
			  CIGet(ComponentI, Property->Name, Property->Units, Property->OptionalInput, &::CallBack,
                instanceNumber, Property->regIndex, Property->sDDML);
         }
      }
   }
bool ApsimComponent::Get(String^ PropertyName, ApsimType^ Data, bool isOptional)
   {
   int RegistrationIndex = Registrations->Count;
   Registrations->Add(Data);
   bool result = CIGet(ComponentI, PropertyName, "", isOptional, &::CallBack,
         instanceNumber, RegistrationIndex, Data->DDML());
   Registrations->RemoveAt(RegistrationIndex);
   return result;
   }
void ApsimComponent::Set(String^ PropertyName, ApsimType^ Data)
   {
   int RegistrationIndex = Registrations->Count;
   Registrations->Add(Data);
   CISet(ComponentI, PropertyName, "", &::CallBack,
         instanceNumber, RegistrationIndex, Data->DDML());
   Registrations->RemoveAt(RegistrationIndex);
   }   
void ApsimComponent::Publish(String^ EventName, ApsimType^ Data)
   {
   int RegistrationIndex = Registrations->Count;
   Registrations->Add(Data);
   CIPublish(ComponentI, EventName, &::CallBack,
         instanceNumber, RegistrationIndex, Data->DDML());
   Registrations->RemoveAt(RegistrationIndex);
   }
void ApsimComponent::Subscribe(EvntHandler^ Event)
   {
   int RegistrationIndex = Registrations->Count;
   Registrations->Add(Event);
   CISubscribe(ComponentI, Event->EventName, &::CallBack, instanceNumber, RegistrationIndex, Event->DDML());
   }   
void ApsimComponent::TrapAllEvents(Factory^ F)
	{
	// ----------------------------------------------
	// Look for all event publishers and register them.
	// ----------------------------------------------
	for (int i = 0; i != F->Events->Count; i++)
	   {
	   FactoryEvent^ Event = F->Events[i];
	   Event->ID = Registrations->Count;
      Registrations->Add(Event);
	   Event->OnFired += gcnew FactoryEvent::FactoryEventFired(this, &ApsimComponent::OnPublish);
      }
   }

void ApsimComponent::OnPublish(FactoryEvent^ Event)
	{
	// ----------------------------------------------
	// The model has published an event - go and
	// publish it to APSIM.
	// ----------------------------------------------
	CIPublish(ComponentI, Event->EventName, &::CallBack, instanceNumber, Event->ID, Event->Data->DDML());
	}
void ApsimComponent::BuildObjects(XmlNode^ XML)
   {
	// ----------------------------------------------
	// Build all objects based on the XML passed in.
	// ----------------------------------------------
   Fact = gcnew Factory();
   Fact->Create(XML->OuterXml, modelAssembly, this);
   Model = dynamic_cast<Instance^> (Fact->Root);
   String^ InstanceName = Name;
   if (InstanceName->Contains("."))
      {
      InstanceName = InstanceName->Substring(InstanceName->LastIndexOf('.')+1);
      }
   Model->Name = InstanceName;
   RegisterAllProperties(Fact);
   RegisterAllEventHandlers(Fact);
   TrapAllEvents(Fact);
   Fact->CheckParameters();
   }

void ApsimComponent::PerformInstructions(XmlNode^ Node, XmlNode^% ModelDescription)
   {
	// ----------------------------------------------
	// Perform all instructions found in the
	// specified XML node.
	// ----------------------------------------------
	for each (XmlNode^ Instruction in Node->ChildNodes)
	   {
	   if (Instruction->Name == "Construct")
	      {
	      ModelDescription = XmlHelper::Find(InitData, Instruction->InnerText);
	      if (ModelDescription == nullptr)
	         throw gcnew Exception("Cannot find referenced node: " + Instruction->InnerText);
         }
	   else if (Instruction->Name == "Goto")
	      {
	      XmlNode^ ReferencedNode = XmlHelper::Find(InitData, Instruction->InnerText);
	      if (ReferencedNode == nullptr)
	         throw gcnew Exception("Cannot find cultivar node: " + Instruction->InnerText);
	      PerformInstructions(ReferencedNode, ModelDescription); // recursion
	      }
	   else if (Instruction->Name == "Override")
	      {
	      String^ ReferencedNodeName = XmlHelper::Name(Instruction)->Replace(".", "/");
	      XmlNode^ ReferencedNode = XmlHelper::Find(ModelDescription, ReferencedNodeName);
	      if (ReferencedNode == nullptr)
	         throw gcnew Exception("Cannot find referenced node: " + ReferencedNodeName);
	      for each (XmlNode^ NodeToOverride in  Instruction->ChildNodes)
	         {
	         XmlNode^ NodeToRemove = XmlHelper::Find(ReferencedNode, XmlHelper::Name(NodeToOverride));
	         if (NodeToRemove == nullptr)
	            throw gcnew Exception("Cannot override node: " + XmlHelper::Name(NodeToOverride));
	         XmlNode^ NewNode = ReferencedNode->OwnerDocument->ImportNode(NodeToOverride, true);
	         ReferencedNode->InsertAfter(NewNode, NodeToRemove);
	         ReferencedNode->RemoveChild(NodeToRemove);
	         }
	      }
	   }
   }
void ApsimComponent::OnSow(char* messageData)
   {
	// ----------------------------------------------
	// A sowing event has occurred.
	// ----------------------------------------------
   SowPlant2Type^ Sow = gcnew SowPlant2Type();
   Sow->unpack(messageData);

   if (Sow->Cultivar == nullptr)
      throw gcnew Exception("Cannot find cultivar on sow line");

   if (Model != nullptr)
      throw gcnew Exception("Plant already in ground while trying to sow another crop");

   // Now locate the cultivar parameter section and perform all instructions found there.
   XmlNode^ CultivarNode = XmlHelper::Find(InitData, Sow->Cultivar);
   if (CultivarNode == nullptr)
      throw gcnew Exception("Cannot find cultivar information for: " + Sow->Cultivar);
   XmlNode^ ModelDescription = nullptr;
   PerformInstructions(CultivarNode, ModelDescription);

   // Insert any other parameters found under the <InitData> section into the model description.
   for each (XmlNode^ Parameter in InitData->ChildNodes)
      {
      if (XmlHelper::ChildNodes(Parameter, "")->Count == 0)
         {
         // simple parameter - insert into model description
         InsertParameterIntoModel(Parameter, ModelDescription);
         }
      }

   BuildObjects(ModelDescription);

   CallEventHandlers("Sow", Sow);
   }

void ApsimComponent::CallEventHandlers(String^ EventName, SowPlant2Type^ Data)
   {
   //array<Object^>^ Parameters = gcnew array<Object^>(1);
   //Parameters[0] = Data;
	for (int i = 0; i != Fact->EventHandlers->Count; i++)
	   {
	   EvntHandler^ Event = Fact->EventHandlers[i];
	   if (Event->EventName->Compare(Event->EventName, EventName) == 0)
	      Event->Invoke(Data);
      }
   }

void ApsimComponent::InsertParameterIntoModel(XmlNode^ Parameter, XmlNode^ Node)
   {
   if (Node->Name->ToLower() == Parameter->Name->ToLower())
      Node->InnerText = Parameter->InnerText;
   else
      {
      for each (XmlNode^ Child in Node->ChildNodes)
         InsertParameterIntoModel(Parameter, Child);
      }
   }

void ApsimComponent::OnEndCrop(char* messageData)
   {
	// ----------------------------------------------
	// A harvest event has occurred.
	// ----------------------------------------------
   EndCropToday = true;
   }

void ApsimComponent::OnPost(char* messageData)
   {
	// ----------------------------------------------
	// A post message has been sent. See if we need
	// to do an endcrop.
	// ----------------------------------------------
   if (EndCropToday)
      {
      EndCropToday = false;
      CIDeRegister(ComponentI);
      for (int i = 0; i != Fact->Properties->Count; i++)
	  {
	     FactoryProperty^ Property = Fact->Properties[i];
	     if (Property->IsInput)
			Property->regIndex = -1;
	  }
      Model = nullptr;
      ManagerEventType^ dummy = gcnew ManagerEventType();
      SowPlant2Type^ sowDummy = gcnew SowPlant2Type();
      CISubscribe(ComponentI, "Sow", &::CallBack, instanceNumber, SOWINDEX, sowDummy->DDML());
      CISubscribe(ComponentI, "EndCrop", &::CallBack, instanceNumber, ENDCROPINDEX, "<type/>");
      CISubscribe(ComponentI, "Post", &::CallBack, instanceNumber, POSTINDEX, "<type/>");
      }
   }

Assembly^ ApsimComponent::CompileScript(XmlNode^ Node)
   {
   // Get the language associated with the file extension.
   if (CodeDomProvider::IsDefinedExtension(".cs"))
      {
      bool VB = Node->InnerText->IndexOf("Inherits ") != -1;
      String^ language;
      if (VB)
         language = CodeDomProvider::GetLanguageFromExtension(".vb");
      else
         language = CodeDomProvider::GetLanguageFromExtension(".cs");
      
      if (language && CodeDomProvider::IsDefinedLanguage(language))
         {
         CodeDomProvider^ provider = CodeDomProvider::CreateProvider(language);
         if (provider)
            {
            CompilerParameters^ params = gcnew CompilerParameters();
            params->GenerateInMemory = true;      //Assembly is created in memory
            params->TreatWarningsAsErrors = false;
            params->WarningLevel = 2;
            params->ReferencedAssemblies->Add("System.dll");
            params->ReferencedAssemblies->Add(Path::GetDirectoryName(DllFileName) + "\\DotNetComponentInterface.dll");
            List<String^> ManagerHelpers = LoadManagerHelpers::GetManagerHelpers();
            for each (String^ ManagerHelper in ManagerHelpers)
               params->ReferencedAssemblies->Add(ManagerHelper);
            array<String^>^ source = gcnew array<String^>(1);
            source[0] = Node->InnerText;
            CompilerResults^ results = provider->CompileAssemblyFromSource(params, source);
            String^ Errors = "";
            for each (CompilerError^ err in results->Errors)
               {
               if (Errors != "")
                  Errors += "\r\n";
                  
               Errors += err->ErrorText + ". Line number: " + err->Line.ToString();
               }
            if (Errors != "")
               throw gcnew Exception(Errors);
            return results->CompiledAssembly;
            }
         }
      }
   return nullptr;
   }



String^ ApsimComponent::GetDescription(XmlNode^ InitD)
   {
	// -----------------------------------------------
	// Called by probe tool to return info about model
	// -----------------------------------------------
	InitData = InitD;
	String^ Name = XmlHelper::Name(InitData->ParentNode);
	String^ DLLName = (DllFileName != "") ? DllFileName :
	     XmlHelper::Attribute(InitData->ParentNode, "executable");

	String^ Desc = "<describecomp>\r\n";

	FileVersionInfo^ versionInfo = FileVersionInfo::GetVersionInfo(DLLName);
   Desc += "   <executable>" + DLLName + "</executable>\r\n";
   Desc += "   <class>" + versionInfo->ProductName + "</class>\r\n";
   Desc += "   <version>" + versionInfo->FileVersion + "</version>\r\n";
   Desc += "   <author>" + versionInfo->CompanyName + "</author>\r\n";

   XmlNode^ ModelDescription = nullptr;
   XmlNode^ CultivarNode = XmlHelper::FindByType(InitData, "Cultivar");
   if (CultivarNode != nullptr)
      PerformInstructions(CultivarNode, ModelDescription);
   else if (InitData->ChildNodes->Count > 0)
      ModelDescription = InitData->ChildNodes[0];
      
   if (ModelDescription != nullptr)
      {
      Fact = gcnew Factory();
      Fact->Create(ModelDescription->OuterXml, modelAssembly, this);
      Model = dynamic_cast<Instance^> (Fact->Root);

      // get description for all properties.
      for (int i = 0; i != Fact->Properties->Count; i++)
         {
         String^ St = Fact->Properties[i]->GetDescription();
         if (St != "")
            Desc += St;
         }

      // get description for all events.
      for (int i = 0; i != Fact->Events->Count; i++)
         {
         String^ St = Fact->Events[i]->GetDescription();
         if (St != "")
            Desc += St;
         }

      // get description for all event handlers.
      for (int i = 0; i != Fact->EventHandlers->Count; i++)
         {
         String^ St = Fact->EventHandlers[i]->GetDescription();
         if (St != "")
            Desc += St;
         }

      }
   Desc += "</describecomp>\r\n";
   return Desc;
   }

