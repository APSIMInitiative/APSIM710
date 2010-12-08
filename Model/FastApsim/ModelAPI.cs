using System;
using System.Collections.Generic;
using System.Text;

public interface ModelAPIInterface
   {
   object Get(string Name);
   void Subscribe(string Name, NullTypeDelegate f);
   void Publish(string Name, object Data);
   }


public class ModelAPI : ModelAPIInterface
   {
   private ModelInstance Instance;
   internal ModelAPI(ModelInstance Inst) { Instance = Inst; }
   Dictionary<string, Variable> Gets = new Dictionary<string, Variable>();
   public object Get(string Name)
      {
      Variable V = null;
      if (Gets.TryGetValue(Name, out V))
         return V.Value;
      else
         {
         if (Name.Contains("."))
            {
            string ModelName = Name.Substring(0, Name.LastIndexOf('.'));
            string VariableName = Name.Substring(Name.LastIndexOf('.') + 1);
            ModelInstance Inst = Instance.FindModelInstance(ModelName);
            if (Inst == null)
               throw new Exception("Cannot find a model called " + ModelName + " while trying to do a get for variable " + VariableName);

            // See if the instance has the output we want.
            foreach (Variable Output in Inst.Outputs)
               {
               if (Output.Name.ToLower() == VariableName.ToLower())
                  {
                  V = Output;
                  break;
                  }
               }
            }
         else
            {
            V = Instance.Root.FindOutput(Name);
            }
         if (V == null)
            throw new Exception("Cannot find a value for the variable " + Name);
         Gets.Add(Name, V);
         return V.Value;
         }
      }
   public void Subscribe(string Name, NullTypeDelegate f)
      {
      }

   public void Publish(string Name, object Data)
      {
      }
   }
