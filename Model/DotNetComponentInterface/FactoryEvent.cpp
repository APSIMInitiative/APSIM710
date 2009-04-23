#include "FactoryEvent.h"
#include "ApsimComponent.h"
using namespace ModelFramework;

public delegate void NullTypeDelegate();
public delegate void ApsimTypeDelegate(ApsimType^ Data);
FactoryEvent::FactoryEvent(EventInfo^ Event, Object^ Instance)
   {
   // --------------------------------------------------------------------
   // Constructor
   // --------------------------------------------------------------------
   
   this->Event = Event;
   this->Obj = Instance;
  
   if (Typ == nullptr)
      {
      MethodInfo^ Method = GetType()->GetMethod("NullHandler", BindingFlags::Public | BindingFlags::Instance);
      System::Delegate^ Del = Delegate::CreateDelegate(NullTypeDelegate::typeid, this, Method);
      Event->AddEventHandler(Obj, Del);
      }
   else
      {
      MethodInfo^ Method = GetType()->GetMethod("Handler", BindingFlags::Public | BindingFlags::Instance);
      System::Delegate^ Del = Delegate::CreateDelegate(ApsimTypeDelegate::typeid, this, Method);
      Event->AddEventHandler(Obj, Del);
      }
   }
   
   
void FactoryEvent::NullHandler()
   {
   this->Data = gcnew NullType();
   OnFired(this);
   }
void FactoryEvent::Handler(ApsimType^ Data)
   {
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
   if (DDML() != "")
      {
      Desc += "      " + DDML() + "\r\n";
      }
   Desc += "   </event>\r\n";
   return Desc;
   }
   
   