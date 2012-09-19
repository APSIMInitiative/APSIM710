using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

class RUEModel1
{
    [Link]
    Plant15 Plant = null;

    [Link]
    PStress PStress = null;

    [Link]
    NStress NStress = null;

    [Link]
    SWStress SWStress = null;

    [Link]
    Function TempStress = null;

    [Link]
    Function RUE = null;

    [Event]
    public event NewPotentialGrowthDelegate NewPotentialGrowth;

    public double PotentialDM(double radiationInterceptedGreen)
    {
        double RUEFactor = 1.0;
        double stress_factor = Math.Min(Math.Min(Math.Min(Math.Min(TempStress.Value, NStress.Photo),
                                                          SWStress.OxygenDeficitPhoto),
                                                 PStress.Photo),
                                        RUEFactor);

        return radiationInterceptedGreen * RUE.Value * stress_factor;

    }

    private void PublishNewPotentialGrowth()
    {
        // Send out a NewPotentialGrowthEvent.
        if (NewPotentialGrowth != null)
        {
            NewPotentialGrowthType GrowthType = new NewPotentialGrowthType();
            GrowthType.sender = Plant.Name;
            GrowthType.frgr = (float) Math.Min(Math.Min(TempStress.Value, NStress.Photo),
                                               Math.Min(SWStress.OxygenDeficitPhoto, PStress.Photo));
            NewPotentialGrowth.Invoke(GrowthType);
        }
    }
    [EventHandler]
    public void OnPrepare()
    {
        PublishNewPotentialGrowth();
    }
}

