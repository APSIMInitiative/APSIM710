using System;
using System.Collections.Generic;
using System.Text;


class OnEventFunction : Function
   {
   [Param] private string Event = "";
   private double _Value = 0;
   [Input] public ModelAPIInterface API;
   [Ref("*")] Function[] Children;

   [EventHandler]
   public void  OnInitialised()
      {
      API.Subscribe(Event, OnEvent);
      }

   public void OnEvent()
      {
      if (Children.Length == 0)
         throw new Exception("Cannot find function in function: " + Event);

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
   
