using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
public class animalSection
{
    //[Input]
    // ManureType WASHWATER;
    //[Input]
    //ManureType WHEATSTRAW;
    public double getNrOfAnimals()
    {
        return NrOfAnimals;
    }

    [Link]
    private Component My;
    [Param]
    double NrOfAnimals = 1;
    [Param]
    double SpiltDrinkingWater = 0;
    [EventHandler]
    public void OnInitialised()
    {
        
    }
    [EventHandler]
    public void OnPrepare()
    {
        
    }
    [EventHandler]
    public void OnProcess()
    {
      
      
   
  
  
   

        if (NrOfAnimals > 0)
        {
            // ManureType WASHWATERInput = WASHWATER;
            // WASHWATERInput.amount = NrOfAnimals * SpiltDrinkingWater / 1000.0;
            //RcvManure(WASHWATERInput, null);

            ManureType someStraw = new ManureType();
       
            for(int i=0; i<My.ChildrenAsObjects.Count;i++)
            {
                double ExcretaPartition = 0;
               /* My.ChildrenAsObjects[i].Get("ExcretaPartition", out ExcretaPartition);
                double StrawAdded=0;
                My.ChildrenAsObjects[i].Get("StrawAdded", out StrawAdded);
                someStraw.amount = NrOfAnimals * StrawAdded*ExcretaPartition / 1000;
                My.ChildrenAsObjects[i].Publish("RcvBedding", someStraw);*/
            }

        }




    }

    void RcvManure(ManureType aUrine, ManureType aFaeces)
    {

        /*foreach (Component AnimalSection in My.Children)
        {
            Console.WriteLine(AnimalSection.Children.Count);
            Console.WriteLine(AnimalSection.Name);

            double ExcretaPartition;
            AnimalSection.Get("ExcretaPartition", out ExcretaPartition);
            aUrine.amount *= ExcretaPartition;
            AnimalSection.Publish("RcvFluidManure", aUrine);
            if (aFaeces != null)
            {
                aFaeces.amount *= ExcretaPartition;
                AnimalSection.Publish("RcvSolidManure", aFaeces);
            }
        }*/
    }

     
}
