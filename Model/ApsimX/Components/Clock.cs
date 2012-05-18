using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;

public class Clock
{
    private TimeSpan OneDay = new TimeSpan(1, 0, 0, 0);
    private TimeType t = new TimeType();
    private DateTime _Today;
    private bool FirstDay = true;

    public delegate void NullDelegate();
    public event TimeDelegate Tick;
    public event NullDelegate Prepare;
    public event NullDelegate Process;
    public event NullDelegate Post;
    public event NullDelegate Report;

    [Param]
    public DateTime Start_Date;

    [Param]
    public DateTime End_Date;

    [Output]
    DateTime Today
    {
        get
        {
            if (_Today.Year == 1)
            {
                _Today = Start_Date;
                t.startday = DateUtility.DateTimeToJulianDayNumber(Today);
                t.endday = t.startday;
                FirstDay = true;
            }
             return _Today;
        }
    }

    [EventHandler]
    public void OnTimeStep()
    {
        if (!FirstDay)
        {
            t.startday++;
            t.endday++;
            _Today += OneDay;
        }
        FirstDay = false;
        
        if (_Today > End_Date)
            throw new FinishedException("Simulation terminated normally");

        Tick.Invoke(t);
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

