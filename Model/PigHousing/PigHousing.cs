using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
public class PigHousing
{
    [Link]
    private Component My;
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
        Console.WriteLine(My.Name);
        Console.WriteLine(My.Children.Count);
        for (int i = 0; i < My.Children.Count(); i++)
        {
            Console.WriteLine(My.Children[i].Name);

            for (int j = 0; j < My.Children[i].Children.Count(); j++)
            {
                Console.WriteLine(My.Children[i].Children[j].Name);
            }
        }

    }
}

