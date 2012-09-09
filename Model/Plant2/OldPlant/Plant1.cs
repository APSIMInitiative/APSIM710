using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;


public class Plant1
{
    [Param]
    public string Name;

    [Link]
    Population1 Population1;

    [Link]
    Leaf1 Leaf1;

    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {


    }

    [EventHandler]
    public void OnPrepare()
    {
        Leaf1.PublishNewCanopyEvent();

    }
}

