using System;
using System.Globalization;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;


/// <summary>
/// This class is responsible for all type conversions in ApsimX. 
/// There are two ways to use this TypeConverter class.
/// 1. Deferred conversion:  You can call the CreateConverter method (specifying a From and To type) to return a 
///                         converter delegate that can be called later to perform the conversion.
/// 2. Immediate conversion: You can call the Convert method to perform the object conversion immediately.
/// </summary>
public class TypeConverter
{
    internal delegate object Converter(string Name, object From);

    /// <summary>
    /// Create and return a "converter" that will convert an object of type "FromType" to type "ToType".
    /// Will return null if no converter is necessary. Will throw if a converter cannot be found.
    /// </summary>
    internal static Converter CreateConverter(Type FromType, Type ToType)
    {
        if (FromType != ToType)
        {
            Converter C = TryCreateConverter(FromType, ToType);
            if (C == null)
                throw new Exception("Cannot convert from type " + FromType.ToString() + " to type " + ToType.ToString());
            return C;
        }
        return null;
    }

    /// <summary>
    /// Try and create and return a "converter" that will convert an object of type "FromType" to type "ToType".
    /// Will return null if a converter cannot be found. Will not throw.
    /// </summary>
    internal static Converter TryCreateConverter(Type FromType, Type ToType)
    {
        if (CanHandleType(FromType))
        {
            if (ToType.Name == "Boolean")
                return ToBoolean;
            else if (ToType.Name == "Single")
                return ToSingle;
            else if (ToType.Name == "Double")
                return ToDouble;
            else if (ToType.Name == "Int32")
                return ToInt;
            else if (ToType.Name == "Int64")
                return ToInt64;
            else if (ToType.Name == "String")
                return ToString;
            else if (ToType.Name == "Single[]")
                return ToSingleArray;
            else if (ToType.Name == "Double[]")
                return ToDoubleArray;
            else if (ToType.Name == "String[]")
                return ToStringArray;
            else if (ToType.Name == "DateTime")
                return ToDateTime;
        }
        return null;
    }

    /// <summary>
    /// Convert the specified From object to the specified ToType. If no conversion is required then
    /// From will be returned.
    /// </summary>
    public static object Convert(string Name, object From, Type ToType)
    {
        if (From.GetType() == ToType)
            return From;

        if (CanHandleType(From.GetType()))
        {
            if (ToType.Name == "Boolean")
                return ToBoolean(Name, From);
            else if (ToType.Name == "Single")
                return ToSingle(Name, From);
            else if (ToType.Name == "Double")
                return ToDouble(Name, From);
            else if (ToType.Name == "Int32")
                return ToInt(Name, From);
            else if (ToType.Name == "Int64")
                return ToInt64(Name, From);
            else if (ToType.Name == "String")
                return ToString(Name, From);
            else if (ToType.Name == "Single[]")
                return ToSingleArray(Name, From);
            else if (ToType.Name == "Double[]")
                return ToDoubleArray(Name, From);
            else if (ToType.Name == "String[]")
                return ToStringArray(Name, From);
            else if (ToType.Name == "DateTime")
                return ToDateTime(Name, From);
        }
        return From;
    }

    /// <summary>
    /// Return true if this type converter can handle the specified type.
    /// </summary>
    /// <returns></returns>
    internal static bool CanHandleType(Type T)
    {
        return T == typeof(bool) ||
               T == typeof(Single) ||
               T == typeof(Double) ||
               T == typeof(Int32) ||
               T == typeof(Int64) ||
               T == typeof(String) ||
               T == typeof(Single[]) ||
               T == typeof(Double[]) ||
               T == typeof(String[]) ||
               T == typeof(DateTime);
    }

    // ---------------------------------------------------------------------------
    // These are the converter methods.
    // ---------------------------------------------------------------------------

    /// <summary>
    /// Convert specified object to a single.
    /// </summary>
    internal static object ToBoolean(string Name, object From)
    {
        try
        {
            return System.Convert.ToBoolean(From);
        }
        catch (Exception)
        {
            throw new Exception("Cannot convert " + From.ToString() + " to a boolean. Variable name is " + Name);
        }
    }

    /// <summary>
    /// Convert specified object to a single.
    /// </summary>
    internal static object ToSingle(string Name, object From)
    {
        try
        {
            return System.Convert.ToSingle(From);
        }
        catch (Exception)
        {
            throw new Exception("Cannot convert " + From.ToString() + " to a single. Variable name is " + Name);
        }
    }

    /// <summary>
    /// Convert the specified object to a double.
    /// </summary>
    internal static object ToDouble(string Name, object From)
    {
        try
        {
            return System.Convert.ToDouble(From);
        }
        catch (Exception)
        {
            throw new Exception("Cannot convert " + From.ToString() + " to a double. Variable name is " + Name);
        }

    }

    /// <summary>
    /// Convert the specified object to an integer
    /// </summary>
    internal static object ToInt(string Name, object From)
    {
        try
        {
            return System.Convert.ToInt32(From);
        }
        catch (Exception)
        {
            throw new Exception("Cannot convert " + From.ToString() + " to an integer. Variable name is " + Name);
        }
    }

    /// <summary>
    /// Convert the specified object to an integer
    /// </summary>
    internal static object ToInt64(string Name, object From)
    {
        try
        {
            return System.Convert.ToInt64(From);
        }
        catch (Exception)
        {
            throw new Exception("Cannot convert " + From.ToString() + " to an integer64. Variable name is " + Name);
        }
    }

    /// <summary>
    /// Convert the specified object to a string.
    /// </summary>
    internal static object ToString(string Name, object From)
    {
        return System.Convert.ToString(From);
    }

    /// <summary>
    /// Convert the specified object to a DateTime
    /// </summary>
    internal static object ToDateTime(string Name, object From)
    {
        CultureInfo Aus = new CultureInfo("en-AU");
        return System.Convert.ToDateTime(From, Aus);
    }

    /// <summary>
    /// Convert the specified object to a single array
    /// </summary>
    internal static object ToSingleArray(string Name, object From)
    {
        try
        {
            if (From == null)
                return null;
            else if (From.GetType().Name == "String")
            {
                string ValueString = (string)From;
                string[] StringValues = ValueString.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                float[] Values = new float[StringValues.Length];
                for (int i = 0; i < StringValues.Length; i++)
                    Values[i] = System.Convert.ToSingle(StringValues[i]);
                return Values;
            }
            else if (From.GetType().Name == "Double[]")
            {
                double[] DoubleValues = (double[])From;
                float[] Values = new float[DoubleValues.Length];
                for (int i = 0; i < DoubleValues.Length; i++)
                    Values[i] = System.Convert.ToSingle(DoubleValues[i]);
                return Values;
            }
            else
                throw new Exception("");
        }
        catch (Exception)
        {
            throw new Exception("Cannot convert " + From.ToString() + " to a single array. Variable name is " + Name);
        }
    }

    internal static object ToDoubleArray(string Name, object From)
    {
        try
        {
            if (From == null)
                return null;
            else if (From.GetType().Name == "String")
            {
                string ValueString = (string)From;
                string[] StringValues = ValueString.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                double[] Values = new double[StringValues.Length];
                for (int i = 0; i < StringValues.Length; i++)
                    Values[i] = System.Convert.ToDouble(StringValues[i]);
                return Values;
            }
            else if (From.GetType().Name == "Single[]")
            {
                float[] SingleValues = (float[])From;
                double[] DoubleValues = new double[SingleValues.Length];
                for (int i = 0; i < SingleValues.Length; i++)
                    DoubleValues[i] = System.Convert.ToDouble(SingleValues[i]);
                return DoubleValues;
            }
            else if (From.GetType().Name == "String[]")
            {
                string[] StringValues = (string[])From;
                double[] DoubleValues = new double[StringValues.Length];
                for (int i = 0; i < StringValues.Length; i++)
                    DoubleValues[i] = System.Convert.ToDouble(StringValues[i]);
                return DoubleValues;
            }
            else
                throw new Exception("");
        }
        catch (Exception)
        {
            throw new Exception("Cannot convert " + From.ToString() + " to a double array. Variable name is " + Name);
        }
    }

    internal static object ToStringArray(string Name, object From)
    {
        try
        {
            if (From == null)
                return null;
            else if (From.GetType().Name == "String")
            {
                string ValueString = (string)From;

                // See if it is some XML. If so then assume all the nodes are the values we
                // need to return.
                if (ValueString.Contains("<") && ValueString.Contains(">"))
                {
                    XmlDocument Doc = new XmlDocument();
                    Doc.LoadXml("<Dummy>" + ValueString + "</Dummy>");
                    List<XmlNode> Children = XmlHelper.ChildNodes(Doc.DocumentElement, "");
                    string[] Values = new string[Children.Count];
                    for (int i = 0; i < Children.Count; i++)
                        Values[i] = Children[i].InnerText;
                    return Values;
                }
                else
                    return ValueString.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            }
            else
                throw new Exception("");
        }
        catch (Exception)
        {
            throw new Exception("Cannot convert " + From.ToString() + " to a string array. Variable name is " + Name);
        }
    }



}


