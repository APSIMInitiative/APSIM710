using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;

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
            if (Var.Value.GetType().Name == "String")
               {
               string ValueString = (string)Var.Value;
               string[] StringValues = ValueString.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
               double[] Values = new double[StringValues.Length];
               for (int i = 0; i < StringValues.Length; i++)
                  Values[i] = Convert.ToDouble(StringValues[i]);
               return Values;
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
            if (Var.Value.GetType().Name == "String")
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
//class ToLinearInterpolation : Variable
//   {
//   private Variable Var;
//   public ToLinearInterpolation(Variable var) { Var = var; Name = var.Name; }
//   public override object Value
//      {
//      get
//         {
//         LinearInterpolation LI = new LinearInterpolation();
//         XmlDocument Doc = new XmlDocument();
//         Doc.LoadXml("<XYPairs>" + Var.Value + "</XYPairs>");
//         LI.ReadFromXML(Doc.DocumentElement);
//         return LI;
//         }
//      set { new NotImplementedException("Type converters are not settable"); }
//      }
//   public override Type Type { get { return typeof(string); } }
//   }
class TypeConverter
   {
   public static VariableBase CreateConverterIfNecessary(VariableBase From, VariableBase To)
      {
      if (From.Type != To.Type)
         {
         if (To.Type.Name == "Double")
            return new ToDouble(From);
         else if (To.Type.Name == "Int32")
            return new ToInt(From);
         else if (To.Type.Name == "String")
            return new ToString(From);
         else if (To.Type.Name == "Double[]")
            return new ToDoubleArray(From);
         else if (To.Type.Name == "String[]")
            return new ToStringArray(From);
         else if (To.Type.Name == "DateTime")
             return new ToDateTime(From);
         //else if (To.Type.Name == "LinearInterpolation")
         //   return new ToLinearInterpolation(From);
         else
            throw new Exception("Cannot convert from type " + From.Type.ToString() + " to type " + To.Type.ToString());
         }
      return From;
      }


   }

