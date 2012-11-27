using System;
using System.Text;
using System.Data;
using ApsimFile;
using CSGeneral;
using System.Xml.Serialization;

/// <summary>
/// Reads in met data and makes it available for other components.
/// </summary>
[XmlType("metfile")]
public class MetFile
{
    private ApsimFile.APSIMInputFile File = null;
    private DataTable Data = new DataTable();  
    private bool HaveReadData = false;

    public event NewMetDelegate NewMet;

    [XmlElement("filename")]
    public string FileName = "";

    [Link]
    public Clock Clock;

    public NewMetType MetData = new NewMetType();
    public double MaxT { get { return MetData.maxt; } }
    public double MinT { get { return MetData.mint; } }
    public double Rain { get { return MetData.rain; } }
    public double Radn { get { return MetData.radn; } }
    public double Latitude
    {
        get
        {
            if (File.Constant("Latitude") == null)
                return 0;
            else
                return Convert.ToDouble(File.Constant("Latitude").Value);
        }
    }
    public double tav
    {
        get
        {
            if (File.Constant("tav") == null)
                return 0;
            else
                return Convert.ToDouble(File.Constant("tav").Value);
        }
    }
    public double amp
    {
        get
        {
            if (File.Constant("amp") == null)
                return 0;
            else
                return Convert.ToDouble(File.Constant("amp").Value);
        }
    }

    /// <summary>
    /// An event handler to allow use to initialise ourselves.
    /// </summary>
    public void OnInitialised()
    {
        if (File == null)
        {
            Clock.Tick += new TimeDelegate(OnTick);
            File = new ApsimFile.APSIMInputFile();
            File.Open(Configuration.RemoveMacros(FileName));
        }
    }

    /// <summary>
    /// An event handler for the tick event.
    /// </summary>
    public void OnTick(TimeType t)
    {
        if (!HaveReadData)
        {
            File.SeekToDate(Clock.Today);
            HaveReadData = true;
        }

        File.GetNextLineOfData(Data);

        int RowIndex = Data.Rows.Count - 1;
        if (Clock.Today != DataTableUtility.GetDateFromRow(Data.Rows[RowIndex]))
            throw new Exception("Non consecutive dates found in file: " + FileName);

        MetData.today = (double)Clock.Today.Ticks;
        MetData.radn = Convert.ToSingle(Data.Rows[RowIndex]["Radn"]);
        MetData.maxt = Convert.ToSingle(Data.Rows[RowIndex]["MaxT"]);
        MetData.mint = Convert.ToSingle(Data.Rows[RowIndex]["MinT"]);
        MetData.rain = Convert.ToSingle(Data.Rows[RowIndex]["Rain"]);

        if (NewMet != null)
            NewMet.Invoke(MetData);
        RowIndex++;
    }
}
