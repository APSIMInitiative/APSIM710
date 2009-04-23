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

   [Output]
   public override double Value
      {
      get
         {
         if (XProperty == "Stem.LiveWt")
         { }
            Plant P = (Plant)Root;
            Instance I;
         //string[] words = XProperty.Split(".".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
         
            if (XProperty.LastIndexOf('.') > 0)
               {
               string Iname = XProperty.Substring(0, XProperty.LastIndexOf('.'));
               I = P.Find(Iname);
               }
            else
               I = P;

         string Pname= XProperty.Substring(XProperty.LastIndexOf('.') + 1);

         FieldInfo FI = I.GetType().GetField(Pname, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
         object v;
         if (FI == null)
            {
            PropertyInfo PI = I.GetType().GetProperty(Pname, BindingFlags.NonPublic | BindingFlags.Instance|BindingFlags.Public);
            if (PI == null)
               throw new Exception("GenericFunction " + Name + " unable to find Xproperty: " + XProperty);
            v = PI.GetValue(I, null);
            }
         else
            v = FI.GetValue(I);

         return XYPairs.Value((double)v);
         }
      }
   }

