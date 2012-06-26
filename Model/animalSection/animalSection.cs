using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
public class animalSection
{
    [Link]
    Component My;

    [Link]
    private Paddock MyPaddock;
    [Param]
    double NrOfAnimals = 1;
    [Param]
    double SpiltDrinkingWater = 0;
    [EventHandler]
    public void OnInitialised()
    {
        Console.Out.WriteLine("Entered OnInitialised in animalSection");
    }
    [EventHandler]
    public void OnPrepare()
    {
        Console.Out.WriteLine("Entered OnPrepare in 2222");
    }
    [EventHandler]
    public void OnProcess()
    {
        Console.WriteLine(My.Name);
        Console.WriteLine(My.Children.Count);
        My.LinkByName("floor1");
        Component Floor1 = (Component)MyPaddock.LinkByName("floor1");
        double ExcretaPartition;
        Floor1.Get("ExcretaPartition", out ExcretaPartition);
        for (int i = 0; i < My.Children.Count; i++)
        {
           ;
           Console.WriteLine(My.Children[i].Name);
        }

        foreach (Component AnimalSection in My.Children)
        {
            Console.WriteLine(AnimalSection.Children.Count);
            Console.WriteLine(AnimalSection.Name);

             
                AnimalSection.Get("ExcretaPartition", out ExcretaPartition);
            
        }

        if (NrOfAnimals > 0)
        {
            ManureType WASHWATER=new ManureType();
            WASHWATER.amount=NrOfAnimals * SpiltDrinkingWater/1000.0;
            RcvManure(WASHWATER,null);

            ManureType someStraw = new ManureType();
            foreach (Component AnimalSection in My.Children)
            {
               
                AnimalSection.Get("ExcretaPartition", out ExcretaPartition);
                double StrawAdded;
                AnimalSection.Get("StrawAdded", out StrawAdded);
                someStraw.amount = NrOfAnimals * StrawAdded*ExcretaPartition / 1000;
                AnimalSection.Publish("RcvBedding", someStraw);
            }

        }

   

     
    }

    void RcvManure(ManureType aUrine, ManureType aFaeces)
    {

        foreach (Component AnimalSection in My.Children)
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
        }
    }

     
}
