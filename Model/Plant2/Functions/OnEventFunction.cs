using System;
using System.Collections.Generic;
using System.Text;


class OnEventFunction : Function
   {
   [Param] private string Event = "";
   private double _Value = 0;

   public override void  Initialised()
      {
      PaddockType MyPaddock = new PaddockType(this);
      MyPaddock.Subscribe(Event, OnEvent);

      }

   public void OnEvent()
      {

      Function F = Children["Function"] as Function;
      if (F == null)
         throw new Exception("Cannot find function in function: " + Name);
      _Value = F.Value;
      }

   [Output] public override double Value 
      { 
      get
         {
         return _Value;   
         }
      }

   }
   
