using System;
using System.Reflection;
using System.Xml;
using System.Text;

public abstract class EvntHandler : ApsimType
{
    // --------------------------------------------------------------------
    // A class for representing all registerable events
    // i.e. APSIM events that the model has subscribed to.
    // --------------------------------------------------------------------
    private String Name;

    public EvntHandler(String EventName)
    {
        Name = EventName;
    }
    public String EventName
    {
        get
        {
            return Name;
        }
        set
        {
            Name = value;
        }
    }
    public abstract Type Typ
    {
        get;
    }
    public abstract void Invoke(Object Parameter);
    public virtual void pack(out byte[] messageData)
    {
        throw new Exception("Cannot call pack on an event handler");
    }
    public abstract void unpack(byte[] messageData);
    public abstract uint memorySize();
    public abstract String DDML();
    public String GetDescription()
    {
        // ----------------------------------------------
        // Creates a field wrapper for the given property.
        // ----------------------------------------------
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
        Desc.Append(" kind=\"subscribed\">\r\n");
        Desc.Append("      " + ddmlData + "\r\n");
        Desc.Append("   </event>\r\n");
        return Desc.ToString();
    }
}

public class FactoryEventHandler : EvntHandler
{
    // --------------------------------------------------------------------
    // A class for representing all registerable events
    // i.e. APSIM events that the model has subscribed to.
    // --------------------------------------------------------------------

    private Object Obj;
    private MethodInfo Method;
    private Object[] Parameters;
    private ApsimType Data;
    public FactoryEventHandler(MethodInfo Method, Object Instance, string EventName)
        : base(EventName)
    {
        this.Method = Method;
        this.Obj = Instance;
        Parameters = new Object[1];
        if (Typ == null)
            Data = new NullType();
        else
            Data = (ApsimType)Activator.CreateInstance(Typ);
    }
    public override Type Typ
    {
        get
        {
            if (Method.GetParameters().Length == 0)
                return null;
            else
                return Method.GetParameters()[0].ParameterType;
        }
    }
    public override void Invoke(Object Parameter)
    {
        if (Parameter == null || Parameter.GetType() == typeof(NullType)) // Order of evaluation does matter here
            Method.Invoke(Obj, null);
        else
        {
            Parameters[0] = Parameter;
            Method.Invoke(Obj, Parameters);
        }
    }
    public override void unpack(byte[] messageData)
    {
        Data.unpack(messageData);
        Invoke(Data);
    }
    public override uint memorySize()
    {
        return Data.memorySize();
    }
    public override String DDML()
    {
        if (Typ == null)
            return "<type/>";
        else
            return Data.DDML();
    }
}

