using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Xml;
using System.Text;
using CMPServices;


public delegate void NullTypeDelegate();
public delegate void ApsimTypeDelegate(ApsimType Data);

public class FactoryEvent : ApsimType
{
    // --------------------------------------------------------------------
    // A class for representing all registerable events that will be
    // published to other APSIM components.
    // --------------------------------------------------------------------
    private Object Obj;
    private EventInfo Event;
    //private Type EventType;

    public ApsimType Data;
    public int ID;
    public delegate void FactoryEventFired(FactoryEvent Event);
    public event FactoryEventFired OnFired;

    public String EventName
    {
        get
        {
            return Event.Name;
        }
    }

    public Type Typ
    {
        get
        {
            ParameterInfo[] Params = Event.EventHandlerType.GetMethod("Invoke").GetParameters();
            if (Params.Length == 0)
                return null;
            else
                return Params[0].ParameterType;
        }
    }
    public virtual void pack(out byte[] messageData)
    {
        Data.pack(out messageData);
    }
    public virtual void pack(TTypedValue dest)
    {
        Data.pack(dest);
    }
    public virtual void unpack(TTypedValue src)
    {
        throw new Exception("Cannot call unpack on an event.");
    }
    public virtual void unpack(byte[] messageData)
    {
        throw new Exception("Cannot call unpack on an event.");
    }
    public virtual uint memorySize()
    {
        return Data.memorySize();
    }
    public virtual String DDML()
    {
        if (Data != null)
            return Data.DDML();
        return "";
    }
    // --------------------------------------------------------------------
    // Constructor
    // --------------------------------------------------------------------    
    public FactoryEvent(EventInfo Event, Object Instance)
    {
        this.Event = Event;
        this.Obj = Instance;
        this.Data = null;

        Type dataType = Typ;
        if (dataType == null)
        {
            MethodInfo Method = GetType().GetMethod("NullHandler", BindingFlags.Public | BindingFlags.Instance);
            System.Delegate Del = Delegate.CreateDelegate(typeof(NullTypeDelegate), this, Method);
            Event.AddEventHandler(Obj, Del);
            this.Data = new NullType();
        }
        else
        {
            MethodInfo Method = GetType().GetMethod("Handler", BindingFlags.Public | BindingFlags.Instance);
            System.Delegate Del = Delegate.CreateDelegate(Event.EventHandlerType, this, Method);
            Event.AddEventHandler(Obj, Del);
            if ( !(typeof(ApsimType) == dataType) && (typeof(ApsimType).IsAssignableFrom(dataType)) )
                this.Data = (ApsimType)Activator.CreateInstance(dataType);
        }
    }
    public void NullHandler()
    {
        if (this.Data == null)
        {
            this.Data = new NullType();
        }
        OnFired(this);
    }
    public void Handler(ApsimType Data)
    {
        if ( (this.Data != null) && !Data.GetType().Equals(this.Data.GetType()))
            throw new Exception("Incorrect datatype provided for the " + EventName + " event.");
        
        this.Data = Data;
        OnFired(this);
    }
    // ----------------------------------------------
    /// <summary>
    /// Creates a field wrapper for the given property.
    /// </summary>
    /// <returns></returns>
    // ----------------------------------------------
    public String GetDescription()
    {
        StringBuilder Desc = new StringBuilder("   <event name=\"" + EventName + "\"");
        String ddml = DDML();
        String typeName = "";
        String ddmlData = "";
        if (ddml != "")
        {
            XmlDocument doc = new XmlDocument();
            doc.LoadXml(ddml);
            XmlNode tnNode = doc.DocumentElement.GetAttributeNode("typename");
            if (tnNode != null)
                typeName = tnNode.Value;
            ddmlData = doc.DocumentElement.InnerXml;
        }
        if (typeName != "")
            Desc.Append(" typename=\"" + typeName + "\"");
        Desc.Append(" kind=\"published\">\r\n");
        Desc.Append("      " + ddmlData + "\r\n");
        Desc.Append("   </event>\r\n");
        return Desc.ToString();
    }
}

