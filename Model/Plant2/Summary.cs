using System;
using System.Collections.Generic;
using System.Text;


   class Summary : Instance
      {
      [Input] private int Day = 0;
      [Input] private int Year = 0;

      [EventHandler] public void OnGrowthStage()
         {
         DateTime Today = new DateTime(Year, 1, 1);
         Today = Today.AddDays(Day - 1);
         Plant Plant = (Plant)Root;
         Console.WriteLine(Today.ToShortDateString() + " - " + Plant.Phenology.CurrentPhase.Start );
         Leaf Leaf = Plant.Children["Leaf"] as Leaf;
         Console.WriteLine("                            LAI = " + Leaf.LAI.ToString("f2")+" (m^2/m^2)");
         Console.WriteLine("           Above Ground Biomass = " + Plant.AboveGroundDM.ToString("f2") + " (g/m^2)");

         }
      }

