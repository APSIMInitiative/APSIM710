using System;
using System.Collections.Generic;
using System.Text;


   class Summary : Instance
      {
      [Ref(".simulation.met")] Met Met;

      [Ref("parent(Plant)")] Plant Plant;
      [Ref("parent(Plant).Phenology")] Phenology Phenology;
      [Ref("parent(Plant).Leaf")] Leaf Leaf;

      [EventHandler] public void OnPhaseChanged(PhenologyChangedType PhaseChange)
         {
         DateTime Today = new DateTime(Met.Year, 1, 1);
         Today = Today.AddDays(Met.Day - 1);
         Console.WriteLine(Today.ToShortDateString() + " - " + Phenology.CurrentPhase.Start );
         Console.WriteLine("                            LAI = " + Leaf.LAI.ToString("f2") + " (m^2/m^2)");
         Console.WriteLine("           Above Ground Biomass = " + Plant.AboveGroundDM.ToString("f2") + " (g/m^2)");
         }
      }

