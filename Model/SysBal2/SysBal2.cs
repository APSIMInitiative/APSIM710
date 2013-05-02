using System;
using ModelFramework;

public class SysBal2
{
    [Link]
    SurfaceOM SurfaceOM;
    [Link]
    SoilN SoilN;
    [Link]
    Paddock Paddock;
    [Input]
    double[] sw_dep;
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
        CarbonBalance = SoilN.carbonbalance + SurfaceOM.carbonbalance;
        NitrogenBalance = SoilN.nitrogenbalance - leach_no3 + fertiliser + SurfaceOM.nitrogenbalance;

        foreach (Component Child in Paddock.Children)
        {
           /* string ChildName = Child.FullName;
            double ChildCarbonBalance;
          ->  This loop is wrong. It retrieves soilN and surfaceOM and itself!
            if (Paddock.Get(ChildName + ".CarbonBalance", out ChildCarbonBalance))
            {
                CarbonBalance += ChildCarbonBalance;
            }
            */
            double EP;
            if (Paddock.Get("ep", out EP))   //does not work with Plant2 modules
                cropTotal += EP;  

        }
        if (Math.Abs(CarbonBalance) > 0.05) // catch FP precision issues
        {
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("             !               APSIM Warning            !");
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("          Error in Carbon Balance. Mass balance error(kg/ha): " + CarbonBalance.ToString());
        }

        if (Math.Abs(NitrogenBalance) > 0.05) // catch FP precision issues
        {
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("             !               APSIM Warning            !");
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("          Error in Nitrogen Balance. Mass balance error(kg/ha): " + NitrogenBalance.ToString());
        }

        //Water balance
        cropTotal = 0;
        foreach (Double d in sw_dep)
            stor += d;
        stor -= swStart;
        inFlow = rain + irrigation;
        outFlow = drain + cropTotal + es + runoff;
        WaterBalance = inFlow - outFlow - stor;
        if (Math.Abs(WaterBalance) > 0.05) // catch FP precision issues
        {
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("             !               APSIM Warning            !");
            Console.WriteLine("             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            Console.WriteLine("          Error in Water Balance. Balance error: " + WaterBalance.ToString());
        }
    }
}