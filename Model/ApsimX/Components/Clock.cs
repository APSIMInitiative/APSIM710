using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using System.Xml.Serialization;
using System.Globalization;
using ModelFramework;

[XmlType("clock")]
public class Clock
{
    [Link]
    public Simulation Simulation;

    public event TimeDelegate Tick;
    public event NullTypeDelegate Prepare;
    public event NullTypeDelegate Process;
    public event NullTypeDelegate Post;
    public event NullTypeDelegate Report;
    public string start_date
    {
        get { return StartDate.ToString("d/M/yyyy"); }
        set { StartDate = DateTime.ParseExact(value, "d/M/yyyy", CultureInfo.InvariantCulture); }
    }
    public string end_date
    {
        get { return EndDate.ToString("d/M/yyyy"); }
        set { EndDate = DateTime.ParseExact(value, "d/M/yyyy", null); }
    }
    [XmlIgnore]
    public DateTime StartDate { get; private set; }
    [XmlIgnore]
    public DateTime EndDate { get; private set; }
    [XmlIgnore]
    public DateTime Today { get; private set; } 

    /// <summary>
    /// An event handler to allow use to initialise ourselves.
    /// </summary>
    public void OnInitialised()
    {
        Simulation.Commence += new NullTypeDelegate(OnCommence);
        Today = StartDate;
    }

    /// <summary>
    /// An event handler to signal start of a simulation.
    /// </summary>
    public void OnCommence()
    {
        TimeType t = new TimeType();
        t.startday = DateUtility.DateTimeToJulianDayNumber(Today);
        t.endday = t.startday;

        while (Today <= EndDate)
        {
            if (Tick != null)
                Tick.Invoke(t);
            if (Prepare != null)
                Prepare.Invoke();
            if (Process != null)
                Process.Invoke();
            if (Post != null)
                Post.Invoke();
            if (Report != null)
                Report.Invoke();

            t.startday++;
            t.endday++;
            Today = Today.AddDays(1);
        }

        Console.WriteLine("Simulation terminated normally");
    }

}

