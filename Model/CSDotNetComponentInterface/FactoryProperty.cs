using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml;
using System.Runtime.InteropServices;
using CSGeneral;
using CMPServices;

// --------------------------------------------------------------------
/// <summary>
/// This class and the next two encapsulate the differences between
/// a FieldInfo and a PropertyInfo. Why did Microsoft make these two
/// types different? .e.g the SetObject method have a different number
/// of parameters.
/// </summary>
// --------------------------------------------------------------------
public abstract class ReflectedType : NamedItem
{
    public abstract Type Typ { get; }
    public abstract bool ReadOnly { get; }
    public abstract Object Get { get; }
    public abstract Object[] MetaData { get; }
    public abstract void SetObject(Object Value);
    public void Set(String Value)
    {
        if (Typ.Name == "Double")
            SetObject(Convert.ToDouble(Value));
        else if (Typ.Name == "Int32")
            SetObject(Convert.ToInt32(Value));
        else if (Typ.Name == "Single")
            SetObject(Convert.ToSingle(Value));
        else if (Typ.Name == "Boolean")
            SetObject(Convert.ToBoolean(Value));
        else if (Typ.Name == "String")
            SetObject(Value);
        else if (Typ.Name == "Double[]")
        {
            String Delimiters = " ";
            String[] StringValues = Value.Split(Delimiters.ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            double[] Values = MathUtility.StringsToDoubles(StringValues);
            SetObject(Values);
        }
        else if (Typ.Name == "String[]")
        {
            String Delimiters = " ";
            String[] StringValues = Value.Split(Delimiters.ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            SetObject(StringValues);
        }
        else
            throw new Exception("Cannot set value of property: " + Name +
                                  ". Cannot convert: " + Value.ToString() + " to: " + Typ.Name);
    }
}
// --------------------------------------------------------------------
/// <summary>
/// 
/// </summary>
// --------------------------------------------------------------------
public class ReflectedField : ReflectedType
{
    private FieldInfo Member;
    private Object Obj;
    public ReflectedField(FieldInfo Field, Object Obj)
    {
        Member = Field;
        this.Obj = Obj;
    }
    public override String Name
    {
        get
        {
            return Member.Name;
        }
    }
    public override Type Typ
    {
        get
        {
            return Member.FieldType;
        }
    }
    public override bool ReadOnly
    {
        get
        {
            return true;
        }
    }
    public override Object Get
    {
        get
        {
            return Member.GetValue(Obj);
        }
    }
    public override Object[] MetaData
    {
        get
        {
            return Member.GetCustomAttributes(false);
        }
    }
    public override void SetObject(Object Value)
    {
        Member.SetValue(Obj, Value);
    }
}
// --------------------------------------------------------------------
/// <summary>
/// 
/// </summary>
// --------------------------------------------------------------------
public class ReflectedProperty : ReflectedType
{
    private PropertyInfo Member;
    private Object Obj;
    public ReflectedProperty(PropertyInfo Field, Object Obj)
    {
        Member = Field;
        this.Obj = Obj;
    }
    public override String Name
    {
        get
        {
            return Member.Name;
        }
    }
    public override Type Typ
    {
        get
        {
            return Member.PropertyType;
        }
    }
    public override bool ReadOnly
    {
        get
        {
            return !Member.CanWrite;
        }
    }
    public override Object Get
    {
        get
        {
            try
            {
                return Member.GetValue(Obj, null);
            }
            catch (System.Exception)
            {
                throw new Exception("The property " + Member.Name + " is not a gettable property.");
            }
        }
    }
    public override Object[] MetaData
    {
        get
        {
            return Member.GetCustomAttributes(false);
        }
    }
    public override void SetObject(Object Value)
    {
        Member.SetValue(Obj, Value, null);
    }
}
// --------------------------------------------------------------------
/// <summary>
/// An class for representing all registerable properties
/// i.e. those properties visible to APSIM.
/// </summary>
// --------------------------------------------------------------------
public class FactoryProperty : Instance, ApsimType
{
    private List<ReflectedField> ChildFields;
    private ReflectedType Property;
    private ApsimType Data;
    protected TDDMLValue DDMLValue;

    public bool IsParam;
    public bool IsInput;
    public bool IsOutput;
    public bool ReadOnly;
    public bool HaveSet;
    public bool OptionalInput;
    public bool OptionalParam;
    public double ParamMinVal;
    public double ParamMaxVal;
    public String TypeName;
    public String Units;
    public String Description;
    public String FQN;
    public String OutputName;
    public String sDDML;
	public int regIndex;

    public bool HasAsValue
    {
        get
        {
            return HaveSet;
        }
    }
    public Object Get
    {
        get
        {
            return Property.Get;
        }
    }
    public void Set(Object Value)
    {
        HaveSet = true;
        Property.SetObject(Value);
    }
    public void Set(String Value)
    {
        HaveSet = true;
        Property.Set(Value);
    }
    public void SetFromXML(XmlNode Node)
    {
        // make sure we have a value.
        if (Property.Get == null)
            Property.SetObject(Activator.CreateInstance(Property.Typ));

        // See if this field has a ReadFromXML method. If so then call it.
        MethodInfo ReadFromXML = Property.Typ.GetMethod("ReadFromXML");
        if (ReadFromXML != null)
        {
            Object[] Params = new Object[1];
            Params[0] = Node;
            ReadFromXML.Invoke(Property.Get, Params);
        }
        else
        {
            if (ChildFields == null)
            {
                ChildFields = new List<ReflectedField>();

                foreach (XmlNode Child in Node.ChildNodes)
                {
                    FieldInfo Field = Property.Typ.GetField(Child.Name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
                    if (Field == null)
                        throw new Exception("Cannot find field: " + Child.Name + " in type: " + Property.Typ.Name);
                    ChildFields.Add(new ReflectedField(Field, Property.Get));
                }
            }
            foreach (XmlNode Child in Node.ChildNodes)
            {
                bool Found = false;
                foreach (ReflectedField Field in ChildFields)
                {
                    if (Field.Name == Child.Name)
                    {
                        Field.Set(Child.InnerText);
                        Found = true;
                    }
                }
                if (!Found)
                    throw new Exception("Cannot find field: " + Child.Name + " in type:" + Property.Typ.Name);
            }
        }
        HaveSet = true;
    }
    /// <summary>
    /// Pack into property data array of identical type.
    /// </summary>
    /// <param name="messageData"></param>
    public virtual void pack(out byte[] messageData)
    {
        Data.pack(out messageData);
    }
    //-------------------------------------------------------------------------
    /// <summary>
    /// Pack this variable into the type specified by the destination DDML.
    /// </summary>
    /// <param name="messageData">The data for the variable.</param>
    /// <param name="sDDML">The DDML type of the destination variable.</param>
    //-------------------------------------------------------------------------
    public virtual void pack(out byte[] messageData, String sDDML)
    {
        //fill a src object with the property value
        byte[] dataBuf;
        Data.pack(out dataBuf);
        TDDMLValue src = new TDDMLValue(Data.DDML(), "");
        src.setData(dataBuf, dataBuf.Length);
        //copy the src value into a destination compatible type
        TDDMLValue dest = new TDDMLValue(sDDML, "");
        dest.setValue(src);
        messageData = new byte[dest.sizeBytes()];
        dest.getData(ref messageData);
    }
    /// <summary>
    /// useable when types are identical
    /// </summary>
    /// <param name="messageData"></param>
    public virtual void unpack(byte[] messageData)
    {
        Data.unpack(messageData);
    }
    //-------------------------------------------------------------------------
    /// <summary>
    /// Unpack messageData into this Property. srcDDML is used for type conversion.
    /// </summary>
    /// <param name="messageData"></param>
    /// <param name="srcDDML">The DDML type of the source variable.</param>
    //-------------------------------------------------------------------------
    public void unpack(byte[] messageData, String srcDDML)
    {
        //create two types that would be compatible and use setValue()
        TDDMLValue src = new TDDMLValue(srcDDML, "");
        src.setData(messageData, messageData.Length);
        TDDMLValue dest = new TDDMLValue(Data.DDML(), "");
        dest.setValue(src);                                 //assign the compatible value
        byte[] data = new byte[dest.sizeBytes()];
        dest.getData(ref data);
        Data.unpack(data);
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Set this property from a compatible TTypedValue.
    /// </summary>
    /// <param name="src">TTypedValue source</param>
    // --------------------------------------------------------------------
    public void setValue(TTypedValue src)
    {
        if (DDMLValue != null)
        {
            DDMLValue.setValue(src);                                 //assign the compatible value
            byte[] data = new byte[DDMLValue.sizeBytes()];
            DDMLValue.getData(ref data);
            Data.unpack(data);
        }
    }
    public virtual uint memorySize()
    {
        return Data.memorySize();
    }
    public virtual String DDML()
    {
        //return Data.DDML();
        TDDMLValue value = new TDDMLValue(Data.DDML(), "");
        value.setUnits(Units);
        value.Name = OutputName;
        return value.getText(value, -1, 0);
        //need to ensure that the units for this property are set correctly
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Constructor
    /// </summary>
    /// <param name="Property"></param>
    /// <param name="Parent"></param>
    // --------------------------------------------------------------------
    public FactoryProperty(ReflectedType Property, XmlNode Parent)
    {
        DDMLValue = null;
        IsParam = false;
        IsInput = false;
        IsOutput = false;
        HaveSet = false;
        ReadOnly = Property.ReadOnly;
        TypeName = Property.Typ.Name;
        Units = "";
        Description = "";
        this.Property = Property;
        this.Name = Property.Name;

        FQN = CalcParentName(Parent) + this.Name;
        this.OutputName = FQN;
        Data = GetFieldWrapper(TypeName);
        String sDDML = Data.DDML();
        regIndex = -1;
        if (sDDML.Length > 0)
            DDMLValue = new TDDMLValue(sDDML, "");

        foreach (Object Attr in Property.MetaData)
        {
            Param P = null;
            Input I = null;
            Output O = null;
            Units U = null;
            Description D = null;

            if (Attr.GetType() == typeof(Param))
            {
                P = (Param)(Attr);
            }
            else if (Attr.GetType() == typeof(Input))
            {
                I = (Input)(Attr);
            }
            else if (Attr.GetType() == typeof(Output))
            {
                O = (Output)(Attr);
            }
            else if (Attr.GetType() == typeof(Units))
            {
                U = (Units)(Attr);
            }
            else if (Attr.GetType() == typeof(Description))
            {
                D = (Description)(Attr);
            }

            if (P != null)
            {
                IsParam = true;
                OptionalParam = P.Optional;
                ParamMinVal = P.MinVal;
                ParamMaxVal = P.MaxVal;
                if (P.Name != "")
                {
                    Name = P.Name;
                    FQN = CalcParentName(Parent) + this.Name;
                }
            }
            else if (I != null)
            {
                IsInput = true;
                OptionalInput = I.Optional;
            }
            else if (O != null)
            {
                IsOutput = true;
                if (O.Name != "")
                    OutputName = O.Name;
            }
            else if (U != null)
                Units = U.ToString();
            else if (D != null)
                Description = D.ToString();
        }
    }

    private ApsimType GetFieldWrapper(String typeName)
    {
        // ----------------------------------------------
        // Creates a field wrapper for the given property.
        // ----------------------------------------------

        if (typeName == "Single")
            return new WrapBuiltIn<Single>(this);
        else if (typeName == "Double")
            return new WrapBuiltIn<Double>(this);
        else if (typeName == "Int32")
            return new WrapBuiltIn<Int32>(this);
        else if (typeName == "String")
            return new WrapBuiltIn<String>(this);
        else if (typeName == "Single[]")
            return new WrapBuiltIn<Single[]>(this);
        else if (typeName == "Double[]")
            return new WrapBuiltIn<Double[]>(this);
        else if (typeName == "Int32[]")
            return new WrapBuiltIn<Int32[]>(this);
        else if (typeName == "String[]")
            return new WrapBuiltIn<String[]>(this);
        else
            return new WrapApsimType(this);
    }

    public String CalcParentName(XmlNode Node)
    {
        // ----------------------------------------------
        // Calculate a parent name
        // ----------------------------------------------

        String ParentName = "";
        if (Node == null || Node.ParentNode == null || Node.ParentNode.ParentNode == null)
            return "";

        ParentName = CalcParentName(Node.ParentNode);
        return ParentName + XmlHelper.Name(Node);
    }
    // ----------------------------------------------
    /// <summary>
    /// Creates a field wrapper for the given property.
    /// </summary>
    /// <returns></returns>
    // ----------------------------------------------
    public String GetDescription()
    {
        String Desc = "";
        if (IsOutput)
        {
            Desc = "   <property name=\"" + OutputName + "\" access=\"";
            if (ReadOnly)
                Desc += "read";
            else
                Desc += "both";
            Desc += "\"  init=\"F\">\r\n";
            if (DDML() != "")
            {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(DDML());
                if (Units != "")
                    XmlHelper.SetAttribute(Doc.DocumentElement, "unit", Units);
                if (Description != "")
                    XmlHelper.SetAttribute(Doc.DocumentElement, "description", Description);
                Desc += "      " + Doc.DocumentElement.OuterXml + "\r\n";
            }
            Desc += "   </property>\r\n";
        }
        else if (IsInput)
        {
            Desc = "   <driver name=\"" + OutputName + "\">\r\n";
            if (DDML() != "")
            {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(DDML());
                if (Units != "")
                    XmlHelper.SetAttribute(Doc.DocumentElement, "unit", Units);
                if (Description != "")
                    XmlHelper.SetAttribute(Doc.DocumentElement, "description", Description);
                Desc += "      " + Doc.DocumentElement.OuterXml + "\r\n";
            }
            Desc += "   </driver>\r\n";
        }
        return Desc;
    }

    //template <class T>
    public class WrapBuiltIn<T> : TypeInterpreter, ApsimType
    {
        // --------------------------------------------------------------------
        // This class wraps a FactoryProperty and a built in type (e.g. Single, 
        // Double etc). It then makes it look like an ApsimType with pack,
        // unpack methods etc.
        // --------------------------------------------------------------------
        private FactoryProperty Property;
        private WrapBuiltInVariable<T> thisProperty;
        public WrapBuiltIn(FactoryProperty Property)
        {
            thisProperty = new WrapBuiltInVariable<T>();    //NH- would be better using an ApsimType or TDDMLValue
            this.Property = Property;
        }
        public override void pack(out byte[] messageData)
        {
            T Data = (T)Property.Get;
            thisProperty.Value = Data;
            thisProperty.pack(out messageData);
        }
        public override void unpack(byte[] messageData)
        {
            T Data = (T)Property.Get;
            thisProperty.unpack(messageData);
            Data = thisProperty.Value;
            Property.Set(Data);
        }
        public override uint memorySize()
        {
            T Data = (T)Property.Get;
            thisProperty.Value = Data;  //update the stored value
            return thisProperty.memorySize(); 
        }
        public override String DDML()
        {
            String result = "<type/>";
            if (typeof(T) == typeof(Boolean))
            {
                result = "<type kind=\"boolean\"/>";
            }
            else if (typeof(T) == typeof(int))
            {
                result = "<type kind=\"integer4\"/>";
            }
            else if (typeof(T) == typeof(Single))
            {
                result = "<type kind=\"single\"/>";
            }
            else if (typeof(T) == typeof(double))
            {
                result = "<type kind=\"double\"/>";
            }
            else if (typeof(T) == typeof(String))
            {
                result = "<type kind=\"string\"/>";
            }
            else if (typeof(T) == typeof(Boolean[]))
            {
                result = "<type kind=\"boolean\" array=\"T\"/>";
            }
            else if (typeof(T) == typeof(int[]))
            {
                result = "<type kind=\"integer\" array=\"T\"/>";
            }
            else if (typeof(T) == typeof(Single[]))
            {
                result = "<type kind=\"single\" array=\"T\"/>";
            }
            else if (typeof(T) == typeof(double[]))
            {
                result = "<type kind=\"double\" array=\"T\"/>";
            }
            else if (typeof(T) == typeof(String[]))
            {
                result = "<type kind=\"string\" array=\"T\"/>";
            }
            return result;
        }

    }
    // --------------------------------------------------------------------
    /// <summary>
    /// This class wraps a FactoryProperty and a built in type (e.g. Single, 
    /// Double etc). It then makes it look like an ApsimType with pack,
    /// unpack methods etc.
    /// </summary>
    // --------------------------------------------------------------------
    public class WrapApsimType : ApsimType
    {
        private FactoryProperty Property;
        public WrapApsimType(FactoryProperty Property)
        {
            this.Property = Property;
        }
        public void pack(out byte[] messageData)
        {
            ApsimType Data = (ApsimType)Property.Get;
            Data.pack(out messageData);
        }
        public void unpack(byte[] messageData)
        {
            ApsimType Data = (ApsimType)Property.Get;
            Data.unpack(messageData);
            Property.Set(Data);
        }
        public uint memorySize()
        {
            ApsimType Data = (ApsimType)Property.Get;
            return Data.memorySize();
        }
        public String DDML()
        {
            ApsimType Data = (ApsimType)(Property.Get);
            if (Data == null)
                return "";
            return Data.DDML();
        }
    }
}
