using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
public class animal
{
    protected double NumberPrDay = 0;
    protected ManureType manurePrDay;
    protected ManureType solidManurePrDay;
    protected ManureType fluidManurePrDay;
    protected feedItemType currentfeed;
    [Param]
    public int amountFluid = 0;
    [Param]
    public double P_growth = 0;
    [Param]
    public int amountSolid = 0;
    public animal()
    {
        solidManurePrDay = new ManureType();
        fluidManurePrDay = new ManureType();
        currentfeed = new feedItemType();
        Console.WriteLine("con animal");
    }
    [EventHandler]
    public void OnProcess()
    {
        fluidManurePrDay.amount=(amountFluid / 1000.0);
        solidManurePrDay.amount=(amountSolid / 1000.0);
        Console.WriteLine("animal");
    }
}

