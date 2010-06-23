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
      if (Children.Count == 0)
         throw new Exception("Cannot find function in function: " + Name);

      Function F = Children[0] as Function;
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
   
