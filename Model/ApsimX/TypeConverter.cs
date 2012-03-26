using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;

class ToSingle : VariableBase
{
    private VariableBase Var;
    public ToSingle(VariableBase var) { Var = var; Name = var.Name; }
    public override object Value
    {
        get
        {
            try
            {
                return Convert.ToSingle(Var.Value);
            }
            catch (Exception)
            {
                throw new Exception("Cannot convert " + Var.Value.ToString() + " to a single. Variable name is " + Var.Name);
            }
        }
        set { new NotImplementedException("Type converters are not settable"); }
    }
    public override Type Type { get { return typeof(double); } }
}

class ToDouble : VariableBase
   {
   private VariableBase Var;
   public ToDouble(VariableBase var) { Var = var; Name = var.Name; }
   public override object Value
      {
      get 
         {
         try
            {
            return Convert.ToDouble(Var.Value);
            }
         catch (Exception)
            {
            throw new Exception("Cannot convert " + Var.Value.ToString() + " to a double. Variable name is " + Var.Name);
            }
         }
      set { new NotImplementedException("Type converters are not settable"); }
      }
   public override Type Type {get { return typeof(double); }}
   }
class ToInt : VariableBase
   {
   private VariableBase Var;
   public ToInt(VariableBase var) { Var = var; Name = var.Name; }
   public override object Value
      {
      get
         {
         try
            {
            return Convert.ToInt32(Var.Value);
            }
         catch (Exception)
            {
            throw new Exception("Cannot convert " + Var.Value.ToString() + " to an integer. Variable name is " + Var.Name);
            }
         }
      set { new NotImplementedException("Type converters are not settable"); }
      }
   public override Type Type { get { return typeof(int); } }
   }
class ToString : VariableBase
   {
   private VariableBase Var;
   public ToString(VariableBase var) { Var = var; Name = var.Name; }
   public override object Value
      {
      get 
         {
         return Convert.ToString(Var.Value);
         }
      set { new NotImplementedException("Type converters are not settable"); }
      }
   public override Type Type { get { return typeof(string); } }
   }
class ToDateTime : VariableBase
{
    private VariableBase Var;
    public ToDateTime(VariableBase var) { Var = var; Name = var.Name; }
    public override object Value
    {
        get
        {
            return Convert.ToDateTime(Var.Value);
        }
        set { new NotImplementedException("Type converters are not settable"); }
    }
    public override Type Type { get { return typeof(string); } }
}
class ToSingleArray : VariableBase
{
    private VariableBase Var;
    public ToSingleArray(VariableBase var) { Var = var; Name = var.Name; }
    public override object Value
    {
        get
        {
            try
            {
                if (Var.Value == null)
                    return null;
                else if (Var.Value.GetType().Name == "String")
                {
                    string ValueString = (string)Var.Value;
                    string[] StringValues = ValueString.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    float[] Values = new float[StringValues.Length];
                    for (int i = 0; i < StringValues.Length; i++)
                        Values[i] = Convert.ToSingle(StringValues[i]);
                    return Values;
                }
                else if (Var.Value.GetType().Name == "Double[]")
                {
                    double[] DoubleValues = (double[])Var.Value;
                    float[] Values = new float[DoubleValues.Length];
                    for (int i = 0; i < DoubleValues.Length; i++)
                        Values[i] = Convert.ToSingle(DoubleValues[i]);
                    return Values;
                }
                else
                    throw new Exception("");
            }
            catch (Exception)
            {
                throw new Exception("Cannot convert " + Var.Value.ToString() + " to a single array. Variable name is " + Var.Name);
            }
        }
        set { new NotImplementedException("Type converters are not settable"); }
    }
    public override Type Type { get { return typeof(double); } }
}
class ToDoubleArray : VariableBase
   {
   private VariableBase Var;
   public ToDoubleArray(VariableBase var) { Var = var; Name = var.Name; }
   public override object Value
      {
      get
         {
         try
            {
            if (Var.Value == null)
                    return null; 
             else if (Var.Value.GetType().Name == "String")
               {
               string ValueString = (string)Var.Value;
               string[] StringValues = ValueString.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
               double[] Values = new double[StringValues.Length];
               for (int i = 0; i < StringValues.Length; i++)
                  Values[i] = Convert.ToDouble(StringValues[i]);
               return Values;
               }
            else if (Var.Value.GetType().Name == "Single[]")
            {
                float[] SingleValues = (float[])Var.Value;
                double[] DoubleValues = new double[SingleValues.Length];
                for (int i = 0; i < SingleValues.Length; i++)
                    DoubleValues[i] = Convert.ToDouble(SingleValues[i]);
                return DoubleValues;
            }
            else if (Var.Value.GetType().Name == "String[]")
            {
                string[] StringValues = (string[])Var.Value;
                double[] DoubleValues = new double[StringValues.Length];
                for (int i = 0; i < StringValues.Length; i++)
                    DoubleValues[i] = Convert.ToDouble(StringValues[i]);
                return DoubleValues;
            }
            else
               throw new Exception("");
            }
         catch (Exception)
            {
            throw new Exception("Cannot convert " + Var.Value.ToString() + " to a double array. Variable name is " + Var.Name);
            }
         }
      set { new NotImplementedException("Type converters are not settable"); }
      }
   public override Type Type { get { return typeof(double); } }
   }
class ToStringArray : VariableBase
   {
   private VariableBase Var;
   public ToStringArray(VariableBase var) { Var = var; Name = var.Name; }
   public override object Value
      {
      get
         {
         try
            {
            if (Var.Value == null)
                return null;
            else if (Var.Value.GetType().Name == "String")
               {
               string ValueString = (string)Var.Value;

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
            throw new Exception("Cannot convert " + Var.Value.ToString() + " to a string array. Variable name is " + Var.Name);
            }
         }
      set { new NotImplementedException("Type converters are not settable"); }
      }
   public override Type Type { get { return typeof(double); } }
   }
class TypeConverter
{
    public static VariableBase CreateConverterIfNecessary(VariableBase From, Type ToType)
    {
        if (From.Type != ToType)
        {
            if (ToType.Name == "Single")
                return new ToSingle(From);
            else if (ToType.Name == "Double")
                return new ToDouble(From);
            else if (ToType.Name == "Int32")
                return new ToInt(From);
            else if (ToType.Name == "String")
                return new ToString(From);
            else if (ToType.Name == "Single[]")
                return new ToSingleArray(From);
            else if (ToType.Name == "Double[]")
                return new ToDoubleArray(From);
            else if (ToType.Name == "String[]")
                return new ToStringArray(From);
            else if (ToType.Name == "DateTime")
                return new ToDateTime(From);
            //else if (To.Type.Name == "LinearInterpolation")
            //   return new ToLinearInterpolation(From);
            else
                throw new Exception("Cannot convert from type " + From.Type.ToString() + " to type " + ToType.ToString());
        }
        return From;
    }
    public static object Convert<T>(T From, Type ToType)
    {
        VariableBase V = new StringVariable<T>("", From);
        return CreateConverterIfNecessary(V, ToType).Value;
    }
}

