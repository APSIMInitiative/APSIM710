using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

public class ClassLink
   {
   public string Name;
   public FieldInfo Info;
   public object Model;
   public bool Optional;
   public ClassLink(object TheModel, string RefName, FieldInfo FI, bool optional)
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


   }
