using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

public class Clock
{
    private TimeSpan OneDay = new TimeSpan(1, 0, 0, 0);

    public delegate void NullDelegate();
    public event NullDelegate Initialised;
    public event NullDelegate Tick;
    public event NullDelegate Prepare;
    public event NullDelegate Process;
    public event NullDelegate Post;
    public event NullDelegate Report;

    [Param]
    public DateTime Start_Date;

    [Param]
    public DateTime End_Date;

    [Output]
    DateTime Today;

    [EventHandler]
    public void OnTimeStep()
    {
        if (Today.Year == 1)
            Today = Start_Date;
        else
            Today += OneDay;

        if (Today > End_Date)
            throw new FinishedException("Simulation terminated normally");

        if (Initialised != null)
            Initialised.Invoke();
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

