using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;



public class GenericFunction : Function
   {
   [Param]
   private LinearInterpolation XYPairs = null;
   [Param]
   private string XProperty = "";
   [Input] public ModelAPIInterface API;

   [Output]
   public override double Value
      {
      get
         {
         double Value = (double) API.Get(XProperty);
         return XYPairs.Value(Value);
         }
      }

   //static public double GetPropertyValueFromPlant(Plant P, string PropertyName)
   //   {
   //   Instance I;

   //   if (PropertyName.LastIndexOf('.') > 0)
   //      {
   //      string Iname = PropertyName.Substring(0, PropertyName.LastIndexOf('.'));
   //      I = P.Find(Iname);
   //      }
   //   else
   //      I = P;

   //   string Pname = PropertyName.Substring(PropertyName.LastIndexOf('.') + 1);

   //   FieldInfo FI = I.GetType().GetField(Pname, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
   //   object v;
   //   if (FI == null)
   //      {
   //      PropertyInfo PI = I.GetType().GetProperty(Pname, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
   //      if (PI == null)
   //         throw new Exception("Unable to get the value of: " + PropertyName);
   //      v = PI.GetValue(I, null);
   //      }
   //   else
   //      v = FI.GetValue(I);

   //   return Convert.ToDouble(v);
   //   }
   }

