using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

public class Clock
{
    public delegate void NullDelegate();
    public event NullDelegate Tick;
    public event NullDelegate Prepare;
    public event NullDelegate Process;
    public event NullDelegate Post;
    public event NullDelegate Report;

    [Param]
    public string Start_Date = null;

    [Param]
    public string End_Date = null; 


    [EventHandler]
    public void OnTimeStep()
    {
        Tick.Invoke();
        if (Prepare != null)
            Prepare.Invoke();
        if (Process != null)
            Process.Invoke();
        if (Post != null)
            Post.Invoke();
        if (Report != null)
            Report.Invoke();
    }

}

