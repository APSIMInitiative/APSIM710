using System;
using System.Collections.Generic;
using System.Text;


class PhaseJumpFunction : Instance
   {
   [Param] private string Start = "";
   [Param] private string End = "";
   [Param] private string PhaseNameToJumpTo = "";
   [Param] private string Event = "";

   public override void  Initialised()
      {
      PaddockType MyPaddock = new PaddockType(this);
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
   
