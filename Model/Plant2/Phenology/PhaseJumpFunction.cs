using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;


class PhaseJumpFunction : Instance
   {
   [Param] private string Start = "";
   [Param] private string End = "";
   [Param] private string PhaseNameToJumpTo = "";
   [Param] private string Event = "";
   [Link] private Paddock MyPaddock;
   public override void  Initialised()
      {
      MyPaddock.Subscribe(Event, OnEvent);
      }

   public void OnEvent()
      {
      Plant ParentPlant = (Plant) Root;
      if (ParentPlant.Phenology.Between(Start, End))
         {
         ParentPlant.Phenology.CurrentPhaseName = PhaseNameToJumpTo;
         }
      }
   }
   
