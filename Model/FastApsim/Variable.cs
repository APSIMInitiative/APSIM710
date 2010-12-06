using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Abstract variable class for encapsulating an APSIM variable, be it input, output, param or state.
/// </summary>
abstract class Variable
   {
   public string Name;
   private Variable Output;
   internal void ConnectTo(Variable Out)
      {
      Output = Out;
      }


   internal void UpdateValue()
      {
      if (Output == null)
         throw new Exception("Cannot find a value for [Input] variable: " + Name);
      Value = Output.Value;
      }
   public bool IsConnected { get { return Output != null; } }

   public abstract Type Type { get; }
   public abstract object Value { get; set; }

   }


/// <summary>
/// A derived variable class for encapsulating a reflected FieldInfo.
/// </summary>
class FieldVariable : Variable
   {
   private FieldInfo Info;
   object Model;
   public FieldVariable(FieldInfo FI, object M)
      {
      Info = FI;
      Model = M;
      Name = Info.Name;
      }

   public override object Value
      {
      get { return Info.GetValue(Model); }
      set { Info.SetValue(Model, value); }
      }
   public override Type Type
      {
      get { return Info.FieldType; }
      }
   }

/// <summary>
/// A derived variable class for encapsulating a reflected PropertyInfo.
/// </summary>
class PropertyVariable : Variable
   {
   private PropertyInfo Info;
   object Model;
   public PropertyVariable(PropertyInfo FI, object M)
      {
      Info = FI;
      Model = M;
      Name = Info.Name;
      }

   public override object Value
      {
      get { return Info.GetValue(Model, null); }
      set { Info.SetValue(Model, value, null); }
      }
   public override Type Type
      {
      get { return Info.PropertyType; }
      }
   }

/// <summary>
/// A simple string variable class for holding a string value. Used with [Param] variables.
/// </summary>
class StringVariable : Variable
   {
   private string Val;
   public StringVariable(string name, string value)
      {
      Name = name;
      Val = value;
      }


   public override object Value
      {
      get { return Val; }
      set { new NotImplementedException("A StringVariable is not settable"); }
      }
   public override Type Type
      {
      get { return Val.GetType(); }
      }
   }