using System;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using System.Runtime.InteropServices;
using ModelFramework;
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
    public abstract bool WriteOnly { get; }
    public abstract Object Get { get; }
    public abstract Object[] MetaData { get; }
    public abstract void SetObject(Object Value);
    public void Set(XmlNode Node)
    {
        String Value = Node.InnerText;
        try
        {
            if (Typ.Name == "Double")
                SetObject(Convert.ToDouble(Value));
            else if (Typ.Name == "Int32")
                SetObject(Convert.ToInt32(Value));
            else if (Typ.Name == "Single")
                SetObject(Convert.ToSingle(Value));
            else if (Typ.Name == "Boolean")
            {
                if (string.Equals(Value, "yes", StringComparison.CurrentCultureIgnoreCase))
                    SetObject(true);
                else if (string.Equals(Value, "no", StringComparison.CurrentCultureIgnoreCase))
                    SetObject(false);
                else
                    SetObject(Convert.ToBoolean(Value));
            }
            else if (Typ.Name == "String")
                SetObject(Value);
            else if (Typ.Name == "Double[]")
            {
                List<String> StringValues;
                if (XmlHelper.Find(Node, "double") != null)
                    StringValues = XmlHelper.Values(Node, "double");
                else
                    StringValues = XmlHelper.Values(Node.ParentNode, Node.Name);
                double[] DoubleValues = null;
                if (StringValues.Count > 1)
                    DoubleValues = MathUtility.StringsToDoubles(StringValues);
                else if (StringValues.Count == 1)
                {
                    String Delimiters = " ";
                    String[] Values = StringValues[0].Split(Delimiters.ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    DoubleValues = MathUtility.StringsToDoubles(Values);
                }
                SetObject(DoubleValues);
            }
            else if (Typ.Name == "String[]")
            {
                List<String> StringValues;
                if (XmlHelper.Find(Node, "string") != null)
                    StringValues = XmlHelper.Values(Node, "string");
                else
                    StringValues = XmlHelper.Values(Node.ParentNode, Node.Name);
                String[] Values = null;
                if (StringValues.Count > 1)
                {
                    Values = new String[StringValues.Count];
                    StringValues.CopyTo(Values);
                }
                else if (StringValues.Count == 1)
                {
                    String Delimiters = " ";
                    Values = StringValues[0].Split(Delimiters.ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                }
                SetObject(Values);
            }
            else if (Typ.Name == "DateTime")
                SetObject(DateTime.Parse(Value));
            else
                throw new Exception("Cannot set value of property: " + Name +
                                        ". Cannot convert: " + Value.ToString() + " to: " + Typ.Name);

        }
        catch (Exception err)
        {
            String Msg = err.Message + "\nParameter: " + Name + "\nValue: " + Value;
            throw new Exception(Msg);
        }
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
    public override bool WriteOnly
    {
        get
        {
            return false;
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
            return Member.CanRead && !Member.CanWrite;
        }
    }
    public override bool WriteOnly
    {
        get
        {
            return Member.CanWrite && !Member.CanRead;
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
            catch (System.Exception e)
            {
                throw new Exception("The property " + Member.Name + " is not a gettable property. (" + e.Message + ")");
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
internal class FactoryProperty : Instance, ApsimType
{
    //private List<ReflectedField> ChildFields;
    private ReflectedType Property;
    private ApsimType Data;

    public bool IsParam;
    public bool IsInput;
    public bool IsOutput;
    public bool ReadOnly;
    public bool WriteOnly;
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
    public void SetObject(Object Value)
    {
        HaveSet = true;
        Property.SetObject(Value);
    }
    public void Set(XmlNode Value)
    {
        HaveSet = true;
        Property.Set(Value);
    }
    public Type Type { get { return Property.Typ; } }

    /* public void Set(String Value)
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
        } */
    public void AddToList(Instance ChildInstance)
    {
        // make sure we have a value.
        if (Property.Get == null)
            Property.SetObject(Activator.CreateInstance(Property.Typ));

        IList L = (IList)Property.Get;
        L.Add((Object)ChildInstance.Model);
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
    /// Pack this variable into the destinatation
    /// </summary>
    /// <param name="dest">A typed value to receive the packed data</param>
    //-------------------------------------------------------------------------
    public virtual void pack(TTypedValue dest)
    {
        Data.pack(dest);
    }
    public virtual void unpack(TTypedValue src)
    {
        Data.unpack(src);
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
 /*   public void unpack(byte[] messageData, String srcDDML)
    {
        //create two types that would be compatible and use setValue()
        TDDMLValue src = new TDDMLValue(srcDDML, "");
        src.setData(messageData, messageData.Length);
        TDDMLValue dest = new TDDMLValue(Data.DDML(), "");
        dest.setValue(src);                                 //assign the compatible value
        byte[] data = new byte[dest.sizeBytes()];
        dest.getData(ref data);
        Data.unpack(data);
    } */
    // --------------------------------------------------------------------
    /// <summary>
    /// Set this property from a compatible TTypedValue.
    /// </summary>
    /// <param name="src">TTypedValue source</param>
    // --------------------------------------------------------------------
    public void setValue(TTypedValue src)
    {
        Data.unpack(src); //assign the compatible value
    }
    public virtual uint memorySize()
    {
        return Data.memorySize();
    }
    public virtual String DDML()
    {
		return(sDDML);
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
        IsParam = false;
        IsInput = false;
        IsOutput = false;
        HaveSet = false;
        ReadOnly = Property.ReadOnly;
        WriteOnly = Property.WriteOnly;
        TypeName = Property.Typ.Name;
        Units = "";
        Description = "";
        this.Property = Property;
        this.Name = Property.Name;

        FQN = CalcParentName(Parent) + this.Name;
        this.OutputName = FQN;

        Data = GetFieldWrapper(Property.Typ);
        if (Data != null)
            sDDML = Data.DDML();
        else
            sDDML = "<type/>";

        regIndex = -1;

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
                OptionalParam = P.IsOptional;
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
                OptionalInput = I.IsOptional;
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
		if (Units != "") 
		{
		  TDDMLValue DDMLValue = new TDDMLValue(sDDML, "");
		  DDMLValue.setUnits(Units);
          sDDML = DDMLValue.asDDML();
		}
	}

    private ApsimType GetFieldWrapper(Type type)
    {
        // ----------------------------------------------
        // Creates a field wrapper for the given property.
        // ----------------------------------------------
        
        if (type == typeof(Single))
            return new WrapBuiltIn<Single>(this);
        else if (type == typeof(Double)) 
            return new WrapBuiltIn<Double>(this);
        else if (type == typeof(Int32)) 
            return new WrapBuiltIn<Int32>(this);
        else if (type == typeof(String))
            return new WrapBuiltIn<String>(this);
        else if (type == typeof(Single[])) 
            return new WrapBuiltIn<Single[]>(this);
        else if (type == typeof(Double[])) 
            return new WrapBuiltIn<Double[]>(this);
        else if (type == typeof(Int32[])) 
            return new WrapBuiltIn<Int32[]>(this);
        else if (type == typeof(String[])) 
            return new WrapBuiltIn<String[]>(this);
        else if (type == typeof(DateTime)) 
            return new WrapBuiltIn<DateTime>(this);
        else if (type == typeof(Boolean)) 
            return new WrapBuiltIn<Boolean>(this);
        else if (type == typeof(Boolean[])) 
            return new WrapBuiltIn<Boolean[]>(this);
        else if (type.GetInterface("ApsimType") != null)
            return new WrapApsimType(this);
        else
            return null;
        
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
            else if (WriteOnly)
                Desc += "write";
            else
                Desc += "both";
            if (IsParam)
                Desc += "\"  init=\"T\">\r\n";
            else
                Desc += "\"  init=\"F\">\r\n";
            if (DDML() != "")
            {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(DDML());
                if (Units != "")
                    XmlHelper.SetAttribute(Doc.DocumentElement, "unit", Units);
                if (Description != "")
                    XmlHelper.SetAttribute(Doc.DocumentElement, "description", Description);
                Doc.DocumentElement.RemoveAttribute("name"); // Description XML doesn't include a "name" attribute, and the APSIM infrastructure gets confused if one is present
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
                Doc.DocumentElement.RemoveAttribute("name"); // Description XML doesn't include a "name" attribute, and the APSIM infrastructure gets confused if one is present
                Desc += "      " + Doc.DocumentElement.OuterXml + "\r\n";
            }
            Desc += "   </driver>\r\n";
        }
        return Desc;
    }
    //=========================================================================
    /// <summary>
    /// This class wraps a FactoryProperty and a built in type (e.g. Single, 
    /// Double etc). It then makes it look like an ApsimType with pack,
    /// unpack methods etc.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class WrapBuiltIn<T> : TypeInterpreter, ApsimType
    {
        protected Type tType;
        public T Value;
        private FactoryProperty Property;
        public WrapBuiltIn(FactoryProperty Property)
        {
            tType = typeof(T);
            this.Property = Property;
        }
        public override void pack(out byte[] messageData)
        {
            T Data = (T)Property.Get;
            setValue(Data); //ensure the DDMLValue is updated
            base.pack(out messageData);
        }
        public override void unpack(byte[] messageData)
        {
            DDMLValue.setData(messageData, messageData.Length, 0);
            setPropertyValue(); //store the Value property
            T Data = Value;
            Property.SetObject(Data);
        }
        public override void unpack(TTypedValue src)
        {
            DDMLValue.setValue(src);
            setPropertyValue(); //store the Value property
            T Data = Value;
            Property.SetObject(Data);
        }
        public override uint memorySize()
        {
            T Data = (T)Property.Get;
            setValue(Data);    //ensure the DDMLValue is updated
            return DDMLValue.sizeBytes();
        }
        /// <summary>
        /// Returns the DDML type xml for the type T. 
        /// </summary>
        /// <returns></returns>
        public override String DDML()
        {
            String result = "<type/>";
            tType = typeof(T);
            if (tType == typeof(Boolean))
            {
                result = "<type kind=\"boolean\"/>";
            }
            else if (tType == typeof(int))
            {
                result = "<type kind=\"integer4\"/>";
            }
            else if (tType == typeof(Single))
            {
                result = "<type kind=\"single\"/>";
            }
            else if (tType == typeof(double))
            {
                result = "<type kind=\"double\"/>";
            }
            else if (tType == typeof(String))
            {
                result = "<type kind=\"string\"/>";
            }
            else if (tType == typeof(Boolean[]))
            {
                result = "<type kind=\"boolean\" array=\"T\"/>";
            }
            else if (tType == typeof(int[]))
            {
                result = "<type kind=\"integer\" array=\"T\"/>";
            }
            else if (tType == typeof(Single[]))
            {
                result = "<type kind=\"single\" array=\"T\"/>";
            }
            else if (tType == typeof(double[]))
            {
                result = "<type kind=\"double\" array=\"T\"/>";
            }
            else if (tType == typeof(String[]))
            {
                result = "<type kind=\"string\" array=\"T\"/>";
            }
            else if (tType == typeof(DateTime))
            {
                result = "<type kind=\"double\"/>";
            }
            return result;
        }
        /// <summary>
        /// Set the internal Value from the DDMLValue that was set in the unpack()
        /// </summary>
        private void setPropertyValue()
        {
            tType = typeof(T);
            if (tType == typeof(Boolean))
            {
                Value = (T)(Convert.ChangeType(DDMLValue.asBool(), tType));
            }
            else if (tType == typeof(Int32))
            {
                Value = (T)(Convert.ChangeType(DDMLValue.asInt(), tType));
            }
            else if (tType == typeof(Single))
            {
                Value = (T)(Convert.ChangeType(DDMLValue.asSingle(), tType));
            }
            else if (tType == typeof(double))
            {
                Value = (T)(Convert.ChangeType(DDMLValue.asDouble(), tType));
            }
            else if (tType == typeof(String))
            {
                Value = (T)(Convert.ChangeType(DDMLValue.asStr(), tType));
            }
            else if (tType == typeof(Boolean[]))
            {
                Value = (T)(Convert.ChangeType(DDMLValue.asBooleanArray(), tType));
            }
            else if (tType == typeof(Int32[]))
            {
                Value = (T)(Convert.ChangeType(DDMLValue.asIntArray(), tType));
            }
            else if (tType == typeof(Single[]))
            {
                Value = (T)(Convert.ChangeType(DDMLValue.asSingleArray(), tType));
            }
            else if (tType == typeof(double[]))
            {
                Value = (T)(Convert.ChangeType(DDMLValue.asDoubleArray(), tType));
            }
            else if (tType == typeof(String[]))
            {
                Value = (T)(Convert.ChangeType(DDMLValue.asStringArray(), tType));
            }
            else if (tType == typeof(DateTime))
            {
                double JulianDate = DDMLValue.asDouble();               //stored as a double
                DateTime jDate = DateUtility.JulianDayNumberToDateTime((int)Math.Truncate(JulianDate));
                Value = (T)(Convert.ChangeType(jDate, typeof(T)));
            }
        }

        private void setValue(object value)
        {
            tType = typeof(T);
            Value = (T)Convert.ChangeType(value, tType);

            if (tType == typeof(Boolean))
            {
                DDMLValue.setValue(Convert.ToBoolean(value));
            }
            else if (tType == typeof(Int32))
            {
                DDMLValue.setValue(Convert.ToInt32(value));
            }
            else if (tType == typeof(Single))
            {
                DDMLValue.setValue(Convert.ToSingle(value));
            }
            else if (tType == typeof(double))
            {
                DDMLValue.setValue(Convert.ToDouble(value));
            }
            else if (tType == typeof(String))
            {
                DDMLValue.setValue(Convert.ToString(value));
            }
            else if (tType == typeof(Boolean[]))
            {
                DDMLValue.setValue((Boolean[])Convert.ChangeType(value, tType));
            }
            else if (tType == typeof(Int32[]))
            {
                DDMLValue.setValue((Int32[])Convert.ChangeType(value, tType));
            }
            else if (tType == typeof(Single[]))
            {
                DDMLValue.setValue((Single[])Convert.ChangeType(value, tType));
            }
            else if (tType == typeof(Double[]))
            {
                DDMLValue.setValue((Double[])Convert.ChangeType(value, tType));
            }
            else if (tType == typeof(String[]))
            {
                DDMLValue.setValue((String[])Convert.ChangeType(value, tType));
            }
            else if (tType == typeof(DateTime))
            {
                double JulianDate = DateUtility.DateTimeToJulianDayNumber((DateTime)Convert.ChangeType(value, tType));
                DDMLValue.setValue(JulianDate);
            }
        }

    }
    // --------------------------------------------------------------------
    /// <summary>
    /// This class wraps a FactoryProperty.
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
        public void pack(TTypedValue dest)
        {
            ApsimType Data = (ApsimType)Property.Get;
            Data.pack(dest);
        }
        public void unpack(TTypedValue src)
        {
            ApsimType Data = (ApsimType)Property.Get;
            Data.unpack(src);
            Property.SetObject(Data);
        }
        public void unpack(byte[] messageData)
        {
            ApsimType Data = (ApsimType)Property.Get;
            Data.unpack(messageData);
            Property.SetObject(Data);
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
            {
                Data = Activator.CreateInstance(Property.Type) as ApsimType;
                Property.SetObject(Data);
            }

            if (Data == null)
                return "";
            return Data.DDML();
        }
    }

}



