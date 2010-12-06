using System;
using System.Collections.Generic;
using System.Text;


class PhaseJumpFunction : Instance
   {
   [Param] private string Start = "";
   [Param] private string End = "";
   [Param] private string PhaseNameToJumpTo = "";
   [Param] private string Event = "";
   [Input] public ModelAPIInterface API;
   [Ref("parent(Plant).Phenology")] Phenology Phenology;

   [EventHandler]
   public virtual void OnInitialised()
     {
     API.Subscribe(Event, OnEvent);
     }

   public void OnEvent()
      {
      if (Phenology.Between(Start, End))
         Phenology.CurrentPhaseName = PhaseNameToJumpTo;
      }
   }
   
