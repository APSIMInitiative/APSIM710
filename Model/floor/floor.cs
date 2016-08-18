using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;

public class floor
{
   
    [Link]
    private Component My;
    store Solid=new store();
    store liquid=new store();
    [Param]
    public double StrawAdded = 10;
    [Param]
    public double ExcretaPartition = 10;
   // [Param]
    public double WashingWater = 0;
  //  [Param]
    public string WhereTo = "NoWare";
    [Output]
    public string WhereFrom = "NoWare";
        [Output]
    public double lom =4.4;
     [EventHandler]
    public void OnInitialised()
    {
        lom = 1.2;
        Console.WriteLine("on init");
    }
    [EventHandler]
    public void OnPrepare()
    {
        Console.WriteLine("on prep");
    }
    [EventHandler]
    public void OnProcess()
    {
        Console.WriteLine("on pro");
        Console.WriteLine(ExcretaPartition);
    }
    [EventHandler]
    public void OnRcvBedding(ManureType aBeddingMaterial)
    {
        Solid.RcvManure(aBeddingMaterial);

    }
    [EventHandler]
    public void OnRcvFluidManure(ManureType Manure)
    {
        liquid.RcvManure(Manure);
    }
    [EventHandler]
    public void OnRcvSolidManure(ManureType Manure)
    {
        Solid.RcvManure(Manure);
    }
    [EventHandler]
    public void OnCleanFloor(DoubleType numberOfAnimals)
    {

        ManureType water = new ManureType();
        water.amount = WashingWater * numberOfAnimals.Value / 1000.0;
        water.amount += Solid.CleanStore().amount;
        water.amount += liquid.CleanStore().amount;
        bool found = false;

        for (int i = 0; i < My.ChildrenAsObjects.Count(); i++)
        {
           /* if (My.ChildrenAsObjects[i].Name.CompareTo(WhereTo) == 0)
            {
                My.ChildrenAsObjects[i].Publish("RcvManure", water);
            }
            */
        }
        if (found == false)
        {
            throw new Exception("Floor::OnCleanFloor: cannot move manure to " + WhereTo.ToString());
        }
    }
}

