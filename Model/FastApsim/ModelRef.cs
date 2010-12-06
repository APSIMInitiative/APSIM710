using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

class ModelRef
   {
   public string Name;
   public FieldInfo Info;
   public object Model;
   public bool Optional;
   public ModelRef(object TheModel, string RefName, FieldInfo FI, bool optional)
      {
      Name = RefName;
      Info = FI;
      Model = TheModel;
      Optional = optional;
      }
   public object Value
      {
      set 
         {
         Info.SetValue(Model, value); 
         }
      }

   public object Values
      {
      set 
         {
         List<ModelInstance> MultipleValues = (List<ModelInstance>)value;
         if (MultipleValues.Count == 0)
            Info.SetValue(Model, null);
         else
            {
            // Set this ref as an array of object instances.
            if (Info.FieldType.Name.Contains("[]"))
               {
               string ScalarType = Info.FieldType.Name.Replace("[]", "");
               Type T = Info.FieldType.Assembly.GetType(ScalarType);
               Array Values = Array.CreateInstance(T, MultipleValues.Count);
               int NumValues = 0;
               for (int i = 0; i < MultipleValues.Count; i++)
                  {
                  if (T.IsInstanceOfType(MultipleValues[i].TheModel))
                     {
                     Values.SetValue(MultipleValues[i].TheModel, NumValues);
                     NumValues++;
                     }
                  }
               if (Values.Length != NumValues)
                  {
                  Array NewValues = Array.CreateInstance(T, NumValues);
                  Array.Copy(Values, NewValues, NumValues);
                  Values = NewValues;
                  }

               Info.SetValue(Model, Values);

               }
            else
               throw new Exception("Invalid field type name: " + Info.FieldType.Name);
            }
         }
      }
   }
