using System;
using System.Text;
using System.Data;
using ApsimFile;
using CSGeneral;

/// <summary>
/// Reads in met data and makes it available for other components.
/// </summary>
public class MetFile
{
    private ApsimFile.APSIMInputFile File = null;
    private DataTable Data = new DataTable();
    
    private bool HaveReadData = false;
    public event NewMetDelegate NewMet;
    public double[] dummy = new double[3] {10.6, 13.2, 12};
    [Param]
    string FileName = "";
    [Input]
    public DateTime Today;
    [Output]
    public NewMetType MetData = new NewMetType();
    [Output]
    public double MaxT { get { return MetData.maxt; } }
    [Output]
    public double MinT { get { return MetData.mint; } }
    [Output]
    public double Rain { get { return MetData.rain; } }
    [Output]
    public double Radn { get { return MetData.radn; } }
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
    [Output (Immutable = true)]
    public double Latitude
    {
        get
        {
            if (File == null)
                OnInitialised();

            if (File.Constant("Latitude") == null)
                return 0;
            else
                return Convert.ToDouble(File.Constant("Latitude").Value);
        }
    }
    [Output (Immutable = true)]
    public double tav
    {
        get
        {
            if (File == null)
                OnInitialised();

            if (File.Constant("tav") == null)
                return 0;
            else
                return Convert.ToDouble(File.Constant("tav").Value);
        }
    }
    [Output (Immutable = true)]
    public double amp
    {
        get
        {
            if (File == null)
                OnInitialised();

            if (File.Constant("amp") == null)
                return 0;
            else
                return Convert.ToDouble(File.Constant("amp").Value);
        }
    }

    [EventHandler]
    public void OnInitialised()
    {
        if (File == null)
        {
            File = new ApsimFile.APSIMInputFile();
            File.Open(Configuration.RemoveMacros(FileName));
        }
    }

    [EventHandler]
    public void OnTick(TimeType t)
    {
        if (!HaveReadData)
        {
            File.SeekToDate(Today);
            HaveReadData = true;
        }

        File.GetNextLineOfData(Data);

        int RowIndex = Data.Rows.Count - 1;
        if (Today != DataTableUtility.GetDateFromRow(Data.Rows[RowIndex]))
            throw new Exception("Non consecutive dates found in file: " + FileName);

        MetData.today = (double)Today.Ticks;
        MetData.radn = Convert.ToSingle(Data.Rows[RowIndex]["Radn"]);
        MetData.maxt = Convert.ToSingle(Data.Rows[RowIndex]["MaxT"]);
        MetData.mint = Convert.ToSingle(Data.Rows[RowIndex]["MinT"]);
        MetData.rain = Convert.ToSingle(Data.Rows[RowIndex]["Rain"]);

        if (NewMet != null)
            NewMet.Invoke(MetData);
        RowIndex++;
    }
}
