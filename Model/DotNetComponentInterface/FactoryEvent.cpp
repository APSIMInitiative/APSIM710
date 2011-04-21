#include "FactoryEvent.h"
#include "ApsimComponent.h"
using namespace ModelFramework;
using namespace System::Xml;

//public delegate void NullTypeDelegate();
public delegate void ApsimTypeDelegate(ApsimType^ Data);
FactoryEvent::FactoryEvent(EventInfo^ Event, Object^ Instance)
   {
   // --------------------------------------------------------------------
   // Constructor
   // --------------------------------------------------------------------
   
   this->Event = Event;
   this->Obj = Instance;
   this->Data = nullptr;
  
   Type^ dataType = Typ;
   if (dataType == nullptr)
      {
      MethodInfo^ Method = GetType()->GetMethod("NullHandler", BindingFlags::Public | BindingFlags::Instance);
      System::Delegate^ Del = Delegate::CreateDelegate(NullTypeDelegate::typeid, this, Method);
      Event->AddEventHandler(Obj, Del);
	  this->Data = gcnew NullType();
      }
   else
      {
      MethodInfo^ Method = GetType()->GetMethod("Handler", BindingFlags::Public | BindingFlags::Instance);
	  System::Delegate^ Del = Delegate::CreateDelegate(Event->EventHandlerType, this, Method);
      Event->AddEventHandler(Obj, Del);
	  if (!ApsimType::typeid->Equals(dataType) && ApsimType::typeid->IsAssignableFrom(dataType))
        this->Data = (ApsimType^)Activator::CreateInstance(dataType);
      }
   }
   
   
void FactoryEvent::NullHandler()
   {
   if (!this->Data)
     this->Data = gcnew NullType();
   OnFired(this);
   }
void FactoryEvent::Handler(ApsimType^ Data)
   {
   if (this->Data && !Data->GetType()->Equals(this->Data->GetType()))
	   throw gcnew Exception("Incorrect datatype provided for the " + EventName + " event.");
   this->Data = Data;
   OnFired(this);
   }
   
String^ FactoryEvent::GetDescription()
   {	
	// ----------------------------------------------
	// Creates a field wrapper for the given property.
	// ----------------------------------------------
	String^ Desc;
    Desc = "   <event name=\"" + EventName + "\" kind=\"published\">\r\n";
    String^ ddml = DDML();
    if (ddml != "")
    {
        XmlDocument^ doc = gcnew XmlDocument();
        doc->LoadXml(ddml);
        Desc += "      " + doc->DocumentElement->InnerXml + "\r\n";
    }
    Desc += "   </event>\r\n";
    return Desc;
   }
   
   