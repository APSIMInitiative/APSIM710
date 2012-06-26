using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;


class store
{
    [Param]
    public int NH3Emissionfactor=0;
    [Param]
    public int N2Emissionfactor=0;
    [Param]
    public int N2OEmissionfactor=0;

    [Param]
    public int CO2Emissionfactor=0;
    [Param]
    public int CH4Emissionfactor=0;
    [Param]
    public double Capacity=0; 
    private ManureType manure=new ManureType();
    public void RcvManure(ManureType type)
    {
        manure.amount = manure.amount + type.amount;
    }
    public ManureType CleanStore()
    {
        ManureType ReturnValue = manure;
        manure.amount = 0;
        return ReturnValue;
    }
    public double GetAmountInManureStore()
    {
        return manure.amount;
    }
    public double GetCapacity()
    {
        return Capacity;
    }

    public void Volatize(ref double volatAmmonia, ref  double N2Emission, ref double N2OEmission, ref  double CH4Emission, ref double CO2Emission)
    {
     /*   volatAmmonia = NH3Emissionfactor * manure.amount * manure.N;
        N2Emission = N2Emissionfactor * manure.amount * manure.N;
        N2OEmission = N2OEmissionfactor * manure.amount * manure.N;


       CO2Emission= CO2Emissionfactor * manure.amount * manure.Carbon;
        manure.Carbon *= (1 - CO2Emissionfactor);
        CH4Emission = CH4Emissionfactor * manure.amount * manure.Carbon;
        manure.Carbon *= (1 - CH4Emissionfactor);
*/
    }
}

