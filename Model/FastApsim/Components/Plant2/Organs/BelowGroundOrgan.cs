using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class BelowGroundOrgan : GenericOrgan, BelowGround, Reproductive
   {
   [Input]
   private int Day = 0;
   [Input]
   private int Year = 0;
   [Ref("parent(Plant)")] Plant Plant;

   [Event]
   public event NullTypeDelegate Harvesting;
   [EventHandler]
   public void OnHarvest()
      {
      Harvesting.Invoke();

      DateTime Today = new DateTime(Year, 1, 1);
      Today = Today.AddDays(Day - 1);
      string Indent = "     ";
      string Title = Indent + Today.ToShortDateString() + "  - Harvesting " + Name + " from " + Plant.Name;
      double YieldDW = (Live.Wt + Dead.Wt);

      Console.WriteLine("");
      Console.WriteLine(Title);
      Console.WriteLine(Indent + new string('-', Title.Length));
      Console.WriteLine(Indent + Name + " Yield DWt: " + YieldDW.ToString("f2") + " (g/m^2)");
      Console.WriteLine("");


      Live.Clear();
      Dead.Clear();

      }
   }
   
