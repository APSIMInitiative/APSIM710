using System;
using System.Collections.Generic;
using System.Text;


class Summary : Instance
{
    [Link]
    Plant Plant = null;

    [Link]
    Biomass AboveGround = null;

    [Link]
    Phenology Phenology = null;

    [Input]
    private int Day = 0;

    [Input]
    private int Year = 0;

    [EventHandler]
    public void OnPhaseChanged(PhaseChangedType PhaseChange)
    {
        DateTime Today = new DateTime(Year, 1, 1);
        Today = Today.AddDays(Day - 1);
        Console.WriteLine(Today.ToShortDateString() + " - " + Phenology.CurrentPhase.Start);
        if (Plant.Children.Contains("Leaf"))
        {
            Leaf Leaf = Plant.Children["Leaf"] as Leaf;
            Console.WriteLine("                            LAI = " + Leaf.LAI.ToString("f2") + " (m^2/m^2)");
            Console.WriteLine("           Above Ground Biomass = " + AboveGround.Wt.ToString("f2") + " (g/m^2)");
        }
    }
}

