using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

class Summary
{
    [Link]
    Biomass AboveGround = null;

    [Link]
    Phenology Phenology = null;

    [Link(IsOptional=true)]
    Leaf Leaf = null;

    [Link]
    Clock Clock = null;

    [EventHandler]
    public void OnPhaseChanged(PhaseChangedType PhaseChange)
    {
        Console.WriteLine(Clock.Today.ToString("d MMMM yyyy") + " - " + Phenology.CurrentPhase.Start);
        if (Leaf != null)
        {
            Console.WriteLine("                            LAI = " + Leaf.LAI.ToString("f2") + " (m^2/m^2)");
            Console.WriteLine("           Above Ground Biomass = " + AboveGround.Wt.ToString("f2") + " (g/m^2)");
        }
    }
}

