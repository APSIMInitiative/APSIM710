using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

[Description("returns the specified value")]
public class Constant : Function
   {
   [Param(Name = "Value") ] private string k = "0";
   [Output]
   public override double Value { get { return Convert.ToDouble(k); } }
   public override string ValueString { get { return k; } }
   }
