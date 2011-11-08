using System;
using System.Text;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using ModelFramework;
using CSGeneral;


namespace ModelFramework
{
    public class Variable 
    {
        /// --------------------------------------------------------------------------
        /// This class encapsulates an APSIM variable.
        /// --------------------------------------------------------------------------
        //private ApsimComponent Component;
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="component"></param>
        /// <param name="VariableName">FQN of the variable</param>
        public Variable(/*ApsimComponent component, */String VariableName)
        {
            throw new NotImplementedException();
        }

        public bool Exists()
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public bool ToBoolean()
        {
            throw new NotImplementedException();
        }
        public int ToInt32()
        {
            throw new NotImplementedException();
        }
        public float ToSingle()
        {
            throw new NotImplementedException();
        }
        public double ToDouble()
        {
            throw new NotImplementedException();
        }
        public override String ToString()
        {
            throw new NotImplementedException();
        }
        public int[] ToInt32Array()
        {
            throw new NotImplementedException();
        }
        public float[] ToSingleArray()
        {
            throw new NotImplementedException();
        }
        public double[] ToDoubleArray()
        {
            throw new NotImplementedException();
        }
        public String[] ToStringArray()
        {
            throw new NotImplementedException();
        }

        public void Set(int Value)
        {
            throw new NotImplementedException();
        }
        public void Set(float Value)
        {
            throw new NotImplementedException();
        }
        public void Set(double Value)
        {
            throw new NotImplementedException();
        }
        public void Set(String Value)
        {
            throw new NotImplementedException();
        }

        public void Set(int[] Values)
        {
            throw new NotImplementedException();
        }
        public void Set(float[] Values)
        {
            throw new NotImplementedException();
        }
        public void Set(double[] Values)
        {
            throw new NotImplementedException();
        }
        public void Set(String[] Values)
        {
            throw new NotImplementedException();
        }
    }

    // --------------------------------------------------------------------
    // This class wraps a FactoryProperty and a built in type (e.g. Single, 
    // Double etc). It then makes it look like an ApsimType with pack,
    // unpack methods etc.
    // --------------------------------------------------------------------
    //public class WrapBuiltInVariable<T> : TypeInterpreter, ApsimType
    //{
    //    /*This class is a quick way to wrap the TTypedValue into a generic class
    //        * that handles scalars and arrays of scalars - NH
    //        I think this class should be superceded by TypeInterpreter or TDDMLValue
    //        */
    //    protected Type tType;
    //    public T Value;
    //    //protected override TDDMLValue DDMLValue; */
    //    public WrapBuiltInVariable()
    //    {
    //        tType = typeof(T);
    //        // DDMLValue = new TDDMLValue(DDML(), "");
    //    }
    //    public override void pack(out byte[] messageData)
    //    {
    //        setValue(Value);
    //        messageData = new byte[DDMLValue.sizeBytes()];
    //        DDMLValue.getData(ref messageData);
    //    }
    //    public override void unpack(byte[] messageData)
    //    {
    //        //::unpackWithConverter(messageData, Value);
    //        DDMLValue.setData(messageData, messageData.Length, 0);
    //        if (tType == typeof(Boolean))
    //        {
    //            Value = (T)(Convert.ChangeType(DDMLValue.asBool(), typeof(T)));
    //        }
    //        else if (tType == typeof(Int32))
    //        {
    //            Value = (T)(Convert.ChangeType(DDMLValue.asInt(), typeof(T)));
    //        }
    //        else if (tType == typeof(Single))
    //        {
    //            Value = (T)(Convert.ChangeType(DDMLValue.asSingle(), typeof(T)));
    //        }
    //        else if (tType == typeof(double))
    //        {
    //            Value = (T)(Convert.ChangeType(DDMLValue.asDouble(), typeof(T)));
    //        }
    //        else if (tType == typeof(String))
    //        {
    //            Value = (T)(Convert.ChangeType(DDMLValue.asStr(), typeof(T)));
    //        }
    //        else if (tType == typeof(Boolean[]))
    //        {
    //            Value = (T)(Convert.ChangeType(DDMLValue.asBooleanArray(), typeof(T)));
    //        }
    //        else if (tType == typeof(Int32[]))
    //        {
    //            Value = (T)(Convert.ChangeType(DDMLValue.asIntArray(), typeof(T)));
    //        }
    //        else if (tType == typeof(Single[]))
    //        {
    //            Value = (T)(Convert.ChangeType(DDMLValue.asSingleArray(), typeof(T)));
    //        }
    //        else if (tType == typeof(double[]))
    //        {
    //            Value = (T)(Convert.ChangeType(DDMLValue.asDoubleArray(), typeof(T)));
    //        }
    //        else if (tType == typeof(String[]))
    //        {
    //            Value = (T)(Convert.ChangeType(DDMLValue.asStringArray(), typeof(T)));
    //        }
    //        else if (tType == typeof(DateTime))
    //        {
    //            double JulianDate = DDMLValue.asDouble();               //stored as a double
    //            DateTime jDate = TTimeValue.JDToDateTime(JulianDate);
    //            Value = (T)(Convert.ChangeType(jDate, typeof(T)));
    //        }
    //    }
    //    public override uint memorySize()
    //    {
    //        setValue(Value);    //ensure the DDMLValue is updated
    //        return DDMLValue.sizeBytes();
    //    }
    //    public override String DDML()
    //    {
    //        return DDML(Value);
    //    }

    //    public String DDML(T Value)
    //    {
    //        String result = "<type/>";
    //        if (typeof(T) == typeof(Boolean))
    //        {
    //            result = "<type kind=\"boolean\"/>";
    //        }
    //        else if (typeof(T) == typeof(Int32))
    //        {
    //            result = "<type kind=\"integer4\"/>";
    //        }
    //        else if (typeof(T) == typeof(Single))
    //        {
    //            result = "<type kind=\"single\"/>";
    //        }
    //        else if (typeof(T) == typeof(double))
    //        {
    //            result = "<type kind=\"double\"/>";
    //        }
    //        else if (typeof(T) == typeof(String))
    //        {
    //            result = "<type kind=\"string\"/>";
    //        }
    //        else if (typeof(T) == typeof(Boolean[]))
    //        {
    //            result = "<type kind=\"boolean\" array=\"T\"/>";
    //        }
    //        else if (typeof(T) == typeof(Int32[]))
    //        {
    //            result = "<type kind=\"integer\" array=\"T\"/>";
    //        }
    //        else if (typeof(T) == typeof(Single[]))
    //        {
    //            result = "<type kind=\"single\" array=\"T\"/>";
    //        }
    //        else if (typeof(T) == typeof(double[]))
    //        {
    //            result = "<type kind=\"double\" array=\"T\"/>";
    //        }
    //        else if (typeof(T) == typeof(String[]))
    //        {
    //            result = "<type kind=\"string\" array=\"T\"/>";
    //        }
    //        else if (typeof(T) == typeof(DateTime))
    //        {
    //            result = "<type kind=\"double\"/>";
    //        }
    //        return result;
    //    }
    //    protected void setValue(T value)
    //    {
    //        if (tType == typeof(Boolean))
    //        {
    //            DDMLValue.setValue(Convert.ToBoolean(value));
    //        }
    //        else if (tType == typeof(Int32))
    //        {
    //            DDMLValue.setValue(Convert.ToInt32(value));
    //        }
    //        else if (tType == typeof(Single))
    //        {
    //            DDMLValue.setValue(Convert.ToSingle(value));
    //        }
    //        else if (tType == typeof(double))
    //        {
    //            DDMLValue.setValue(Convert.ToDouble(value));
    //        }
    //        else if (tType == typeof(String))
    //        {
    //            DDMLValue.setValue(Convert.ToString(value));
    //        }
    //        else if (tType == typeof(Boolean[]))
    //        {
    //            DDMLValue.setValue((Boolean[])Convert.ChangeType(value, typeof(Boolean[])));
    //        }
    //        else if (tType == typeof(Int32[]))
    //        {
    //            DDMLValue.setValue((Int32[])Convert.ChangeType(value, typeof(Int32[])));
    //        }
    //        else if (tType == typeof(Single[]))
    //        {
    //            DDMLValue.setValue((Single[])Convert.ChangeType(value, typeof(Single[])));
    //        }
    //        else if (tType == typeof(Double[]))
    //        {
    //            DDMLValue.setValue((Double[])Convert.ChangeType(value, typeof(Double[])));
    //        }
    //        else if (tType == typeof(String[]))
    //        {
    //            DDMLValue.setValue((String[])Convert.ChangeType(value, typeof(String[])));
    //        }
    //        else if (tType == typeof(DateTime))
    //        {
    //            double JulianDate = TTimeValue.dateTimeToJD((DateTime)Convert.ChangeType(value, typeof(DateTime)));
    //            DDMLValue.setValue(JulianDate);
    //        }
    //    }
    //}
}
    
