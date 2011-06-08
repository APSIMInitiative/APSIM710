using System;
using ModelFramework;

public class SysBal2 : Instance
{
    [Link]
    SurfaceOM SurfaceOM = null;
    [Link]
    SoilN SoilN = null;
    [Link]
    Paddock MyPaddock = null;
    [Input]
    double[] sw_dep = null;
    [Input]
    double rain = 0;
    [Input]
    double drain = 0;
    [Input]
    double runoff = 0;
    [Input]
    double es = 0;
    [Input]
    double leach_no3 = 0;
    [Input]
    double irrigation = 0;
    [Input]
    double fertiliser = 0;
    [Output]
    double WaterBalance = 0;
    [Output]
    double CarbonBalance = 0;
    [Output]
    double NitrogenBalance = 0;

    private double cropTotal = 0;
    private double swStart = 0;
    private double stor = 0;
    private double inFlow = 0;
    private double outFlow = 0;

    [EventHandler]
    public void OnPrepare()
    {
        swStart = 0;
        stor = 0;
        foreach (Double d in sw_dep)
            swStart += d;
    }

    [EventHandler]
    public void OnPost()
    {
        //Carbon and Nitrogen balance; reset variables
        CarbonBalance = SoilN.Variable("CarbonBalance").ToDouble() + SurfaceOM.Variable("CarbonBalance").ToDouble();
        NitrogenBalance = SoilN.Variable("NitrogenBalance").ToDouble() - leach_no3 + fertiliser;

        foreach (Component Crop in MyPaddock.Crops)
        {
            CarbonBalance += Crop.Variable("CarbonBalance").ToDouble(); // variable has not been added to crops yet
            cropTotal += Crop.Variable("ep").ToDouble();  //does not work with Plant2 modules
        }
        if (Math.Abs(CarbonBalance) > 0.1) // catch FP precision issues
        {
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("             !               APSIM Warning            !");
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("          Error in Carbon Balance. Mass balance error(kg/ha): " + CarbonBalance.ToString());
        }
        else
            CarbonBalance = 0;
        if (Math.Abs(NitrogenBalance) > 0.1) // catch FP precision issues
        {
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("             !               APSIM Warning            !");
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("          Error in Nitrogen Balance. Mass balance error(kg/ha): " + NitrogenBalance.ToString());
        }
        else NitrogenBalance = 0;

        //Water balance
        cropTotal = 0;
        foreach (Double d in sw_dep)
            stor += d;
        stor -= swStart;
        inFlow = rain + irrigation;
        outFlow = drain + cropTotal + es + runoff;
        WaterBalance = inFlow - outFlow - stor;
        if (Math.Abs(WaterBalance) > 0.1) // catch FP precision issues
        {
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("             !               APSIM Warning            !");
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("          Error in Water Balance. Balance error: " + WaterBalance.ToString());
        }
        else WaterBalance = 0;
    }
}