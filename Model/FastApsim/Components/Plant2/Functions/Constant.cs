using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;


public class Constant : Function
   {
   [Param("Value") ] private double k = 0;
   public override double Value { get { return k; } }
   }
