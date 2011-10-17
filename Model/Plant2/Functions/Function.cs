using System;
using System.Collections.Generic;
using System.Text;

abstract public class Function : NamedItem
   {
   abstract public double Value { get;}
   virtual public string ValueString { get { return Value.ToString(); } } 

   }
