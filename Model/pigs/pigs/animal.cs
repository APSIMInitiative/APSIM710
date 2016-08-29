using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
public class animal
{
    [Param]
    public double NumberPrDay;
    [Param]
    public double C_growth;          // C partitioned to tissue growth
    public manure solidManurePrDay;
    public manure fluidManurePrDay;
    public manure manurePrDay;
    protected FeedItem currentfeed;
    [Param]
    public int amountFluid = 0;
    [Param]
    public double P_growth = 0;
    [Param]
    public int amountSolid = 0;
    public animal()
    {
        solidManurePrDay = new manure();
        fluidManurePrDay = new manure();
        currentfeed = new FeedItem();
 
    }
    [EventHandler]
    public void OnProcess()
    {
        fluidManurePrDay.amount=(amountFluid / 1000.0);
        solidManurePrDay.amount=(amountSolid / 1000.0);
   
    }
}

