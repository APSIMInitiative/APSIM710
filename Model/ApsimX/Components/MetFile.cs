using System;
using System.Text;
using System.Data;
using ApsimFile;

/// <summary>
/// Reads in met data and makes it available for other components.
/// </summary>
public class MetFile
{
    private ApsimFile.APSIMInputFile File = new ApsimFile.APSIMInputFile();
    private DataTable Data = new DataTable();
    private int RowIndex = 0;
    public event NewMetDelegate NewMet;
    [Param]
    string FileName = "";
    [Output]
    DateTime Today;
    [Output]
    public double Radn;
    [Output]
    public double MaxT;
    [Output]
    public double MinT;
    [Output]
    public double Rain;
    [Output]
    public int Day
    {
        get
        {
            return Today.DayOfYear;
        }
    }
    [Output]
    public int Year
    {
        get
        {
            return Today.Year;
        }
    }
    [Output]
    public double Latitude
    {
        get
        {
            return Convert.ToDouble(File.Constant("Latitude").Value);
        }
    }
    [Output]
    public double VP
    {
        get
        {
            return VBMet.Humidity.svp((float)MinT);
        }
    }
    [EventHandler]
    public void OnInitialised()
    {
        File.ReadFromFile(Configuration.RemoveMacros(FileName), Data);
    }

    [EventHandler]
    public void OnTick()
    {
        if (RowIndex >= Data.Rows.Count)
            throw new FinishedException("Simulation terminated due to end of met file");

        Today = CSGeneral.DataTableUtility.GetDateFromRow(Data.Rows[RowIndex]);
        Radn = Convert.ToDouble(Data.Rows[RowIndex]["Radn"]);
        MaxT = Convert.ToDouble(Data.Rows[RowIndex]["MaxT"]);
        MinT = Convert.ToDouble(Data.Rows[RowIndex]["MinT"]);
        Rain = Convert.ToDouble(Data.Rows[RowIndex]["Rain"]);

        if (NewMet != null)
        {
            NewMetType NewMetData = new NewMetType();
            NewMetData.today = (double)Today.Ticks;
            NewMetData.maxt = (float)MaxT;
            NewMetData.mint = (float)MinT;
            NewMetData.radn = (float)Radn;
            NewMetData.rain = (float)Rain;
            NewMet.Invoke(NewMetData);
        }
        RowIndex++;
    }
}
