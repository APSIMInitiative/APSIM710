using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;
using System.Collections;



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

   /// <summary>
   /// Return the value of the specified property as an object. The PropertyName
   /// is relative to the RelativeTo argument (usually Plant).
   /// e.g. Leaf.MinT
   ///      Leaf.Leaves[*].Live.Wt - sums all live weights of all objects in leaves array.
   ///      Leaf.Leaves[1].Live.Wt   - returns the live weight of the 2nd element of the leaves array.
   /// </summary>
   static public object GetPropertyValueFromPlant(object RelativeTo, string PropertyName)
      {
      while (PropertyName.Contains("."))
         {
         int PosPeriod = PropertyName.IndexOf('.');
         object O = null;
         string NameToLookFor = PropertyName.Substring(0, PosPeriod);
         string ArraySpecifier = "";
         if (NameToLookFor.Contains("[") && NameToLookFor.Contains("]"))
            ArraySpecifier = StringManip.SplitOffBracketedValue(ref NameToLookFor, '[', ']');

         if (RelativeTo is Instance)
            {
            // Try and look in the children list first.
            Instance Inst = (Instance) RelativeTo;
            if (Inst.Children.Contains(NameToLookFor))
               O = Inst.Find(NameToLookFor);
            }
         if (O == null)
            {
            // Look for a field or property
            O = GetValueOfField(NameToLookFor, RelativeTo); 
            }
         if (O == null)
            throw new Exception("Cannot find property: " + PropertyName);

         if (ArraySpecifier != "")
            {
            IList Array = (IList) O;
            if (ArraySpecifier == "*")
               {
               PropertyName = PropertyName.Substring(PosPeriod + 1);
               double Value = 0;
               for (int i = 0; i < Array.Count; i++)
                  Value += Convert.ToDouble(GetPropertyValueFromPlant(Array[i], PropertyName));
               return Value;
               }
            else
               {
               int ArrayIndex = Convert.ToInt32(ArraySpecifier);
               RelativeTo = Array[ArrayIndex];
               }
            }
         RelativeTo = O;
         PropertyName = PropertyName.Substring(PosPeriod + 1);
         }

      return GetValueOfField(PropertyName, RelativeTo);
      }

   private static object GetValueOfField(string PropertyName, object I)
      {
      FieldInfo FI = I.GetType().GetField(PropertyName, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
      object v = null;
      if (FI == null)
         {
         PropertyInfo PI = I.GetType().GetProperty(PropertyName, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
         if (PI != null)
            v = PI.GetValue(I, null);
         }
      else
         v = FI.GetValue(I);
      return v;
      }
   }

