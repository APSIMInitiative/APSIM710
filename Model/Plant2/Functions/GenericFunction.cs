using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;



public class GenericFunction : Function
   {
   [Param]
   private LinearInterpolation XYPairs = null;
   [Param]
   private string XProperty = "";

   [Output]
   public override double Value
      {
      get
         {
         string PropertyName = XProperty;
         string ArraySpec = StringManip.SplitOffBracketedValue(ref PropertyName, '(', ')');
         double XValue;
         if (ArraySpec == "")
            XValue = Convert.ToDouble(GetPropertyValueFromPlant((Plant)Root, XProperty));
            
         else
            {
            string[] ArraySpecBits = ArraySpec.Split("=".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            if (ArraySpecBits.Length != 2)
               throw new Exception("Invalid depth specifier in generic funciton. Specifier = " + XProperty);
            int Index;
            if (ArraySpecBits[0].ToLower() == "depth")
               {
               double Depth = Convert.ToDouble(GetPropertyValueFromPlant((Plant)Root, ArraySpecBits[1]));

               // Assume dlayer comes from the same object as the depth variable.
               int PosLastPeriod = ArraySpecBits[1].LastIndexOf('.');
               if (PosLastPeriod == -1)
                   throw new Exception("Invalid depth specifier in generic funciton. Specifier = " + XProperty);
               string ObjectName = ArraySpecBits[1].Substring(0, PosLastPeriod);
               double[] dlayer = (double[]) GetPropertyValueFromPlant((Plant)Root, ObjectName + ".dlayer");
               Index = LayerIndex(Depth, dlayer);
               }
            else if (ArraySpecBits[0].ToLower() == "index")
               {
               if (!Int32.TryParse(ArraySpecBits[1], out Index))
                  Index = (int) Convert.ToDouble(GetPropertyValueFromPlant((Plant)Root, ArraySpecBits[1]));
               }
            else
               throw new Exception("Invalid depth specifier in generic funciton. Specifier = " + XProperty);

            double[] Values = (double[]) GetPropertyValueFromPlant((Plant)Root, PropertyName);
            if (Values.Length == 0 || Index >= Values.Length)
               throw new Exception("Index is outside the bounds of array " + PropertyName + " in generic function. Specifier = " + XProperty);
            XValue = Values[Index];
            }
         return XYPairs.Value(XValue); 
         }
      }

   static private int LayerIndex(double depth, double[] dlayer)
      {
      double CumDepth = 0;
      for (int i = 0; i < dlayer.Length; i++)
         {
         CumDepth = CumDepth + dlayer[i];
         if (CumDepth >= depth) { return i; }
         }
      throw new Exception("Depth deeper than bottom of soil profile in GenericFunction.");
      }

   static public object GetPropertyValueFromPlant(Plant P, string PropertyName)
      {
      Instance I;

      if (PropertyName.LastIndexOf('.') > 0)
         {
         string Iname = PropertyName.Substring(0, PropertyName.LastIndexOf('.'));
         I = P.Find(Iname);
         }
      else
         I = P;

      string Pname = PropertyName.Substring(PropertyName.LastIndexOf('.') + 1);

      FieldInfo FI = I.GetType().GetField(Pname, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
      object v;
      if (FI == null)
         {
         PropertyInfo PI = I.GetType().GetProperty(Pname, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
         if (PI == null)
            throw new Exception("Unable to get the value of: " + PropertyName);
         v = PI.GetValue(I, null);
         }
      else
         v = FI.GetValue(I);

      return v;
      }
   }

