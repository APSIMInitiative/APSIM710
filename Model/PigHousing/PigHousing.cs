using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
public class PigHousing
{
    [Link]
    private SystemComponent My; //is this component really a system with system children?
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
        Console.WriteLine(My.Name.ToString() + " pighousing name in pighousing");

        for (int i = 0; i < My.Children.Count(); i++)
        {
            Console.WriteLine(My.Children[i].Name+"'s section name in pighousing");

            for (int j = 0; j < ((SystemComponent)My.Children[i]).Children.Count(); j++)
            {
                Console.WriteLine(((SystemComponent)My.Children[i]).Children[j].Name+" floor name in pighousing");
            }
        }

    }
}

