using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
public class PigHousing
{
    [Link]
    private Component My; //is this component really a system with system children?
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
        Console.WriteLine(My.Name.ToString() + " pighousing name in pighousing "+ My.ChildrenAsObjects.Count());

        for (int i = 0; i < My.ChildrenAsObjects.Count(); i++)
        {
            animalSection stuff = (animalSection)My.ChildrenAsObjects[i];
            Console.WriteLine(stuff.getNrOfAnimals());
           Console.WriteLine(My.ChildrenAsObjects[i].GetType());

        //    for (int j = 0; j < ((Component)My.ChildrenAsObjects[i]).ChildrenAsObjects.Count(); j++)
            {
               // Console.WriteLine(((Component)My.ChildrenAsObjects[i]).ChildrenAsObjects.Name+" floor name in pighousing");
            }
        }

    }
}

