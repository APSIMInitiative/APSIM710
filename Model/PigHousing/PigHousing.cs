using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
public class PigHousing
{
    [Link]
    private Component MyPaddock;
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
        Console.WriteLine(MyPaddock.Name);
        Console.WriteLine(MyPaddock.Children.Count);
        for (int i = 0; i < MyPaddock.Children.Count(); i++)
        {
            Console.WriteLine(MyPaddock.Children[i].Name);

            for (int j = 0; j < MyPaddock.Children[i].Children.Count(); j++)
            {
                Console.WriteLine(MyPaddock.Children[i].Children[j].Name);
            }
        }

    }
}

