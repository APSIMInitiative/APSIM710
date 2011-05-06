using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Reflection;


class CompositeBiomass : Biomass
   {
   [Param] private string[] Property = null;

   [Output]
   [Units("g/m^2")]
   override public double NonStructuralN
      {
      get 
         {
         double Value = 0;
         foreach (string PropertyName in Property)
            Value += Convert.ToDouble(GenericFunction.GetPropertyValueFromPlant((Plant)Root, PropertyName + ".NonStructuralN"));
         return Value;
         }

      set { throw new Exception("Cannot set NonStructuralN in CompositeBiomass"); }
      }

   [Output]
   [Units("g/m^2")]
   override public double StructuralN
      {
      get
         {
         double Value = 0;
         foreach (string PropertyName in Property)
            Value += Convert.ToDouble(GenericFunction.GetPropertyValueFromPlant((Plant)Root, PropertyName + ".StructuralN"));
         return Value;
         }
      set { throw new Exception("Cannot set StructuralN in CompositeBiomass"); }
      }
   [Output]
   [Units("g/m^2")]
   override public double NonStructuralWt
      {
      get
         {
         double Value = 0;
         foreach (string PropertyName in Property)
            Value += Convert.ToDouble(GenericFunction.GetPropertyValueFromPlant((Plant)Root, PropertyName + ".NonStructuralWt"));
         return Value;
         }
      set { throw new Exception("Cannot set NonStructuralWt in CompositeBiomass"); }
      }
   [Output]
   [Units("g/m^2")]
   override public double StructuralWt
      {
      get
         {
         double Value = 0;
         foreach (string PropertyName in Property)
            Value += Convert.ToDouble(GenericFunction.GetPropertyValueFromPlant((Plant)Root, PropertyName + ".StructuralWt"));
         return Value;
         }
      set { throw new Exception("Cannot set StructuralWt in CompositeBiomass"); }
      }

   override public void Clear()
      {
      // This is called in OnCut - for now do nothing.
      }

   }
 