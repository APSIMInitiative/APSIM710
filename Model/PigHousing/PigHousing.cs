using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
public class PigHousing
{    [Link]
    private Paddock MyPaddock;
[Link]
Component My;
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
       // MyPaddock.LinkByName("animalSection");
        My.LinkByName("animalSection");
  //      Component section =(Component) 
     //   Component sections = (Component)MyPaddock.LinkByName("animalSection");
        Console.WriteLine(My.Name);
        Console.WriteLine(My.Children.Count);
        for (int i = 0; i < My.Children.Count; i++)
        {
            ;
            Console.WriteLine(My.Children[i].Name);
        }

    }
}

