using System;
using System.Text;
using System.Data;
using ApsimFile;
using CSGeneral;
using System.Xml.Serialization;
using System.Collections.Specialized;

/// <summary>
/// Reads in met data and makes it available for other components.
/// </summary>
[XmlType("metfile")]
public class MetFile
{
    private ApsimFile.APSIMInputFile File = null;
    private DataTable Data = new DataTable();  
    private bool HaveReadData = false;
    private int MaxTIndex;
    private int MinTIndex;
    private int RadnIndex;
    private int RainIndex;

    public event NewMetDelegate NewMet;

    [XmlElement("filename")]
    public string FileName = "";

    [XmlAttribute("name")]
    public string Name { get; set; }

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
            MaxTIndex = StringManip.IndexOfCaseInsensitive(File.Headings, "Maxt");
            MinTIndex = StringManip.IndexOfCaseInsensitive(File.Headings, "Mint");
            RadnIndex = StringManip.IndexOfCaseInsensitive(File.Headings, "Radn");
            RainIndex = StringManip.IndexOfCaseInsensitive(File.Headings, "Rain");
            if (MaxTIndex == -1)
                throw new Exception("Cannot find MaxT in weather file: " + FileName);
            if (MinTIndex == -1)
                throw new Exception("Cannot find MinT in weather file: " + FileName);
            if (RadnIndex == -1)
                throw new Exception("Cannot find Radn in weather file: " + FileName);
            if (RainIndex == -1)
                throw new Exception("Cannot find Rain in weather file: " + FileName);
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

        
        object[] Values = File.GetNextLineOfData();

        int RowIndex = Data.Rows.Count - 1;
        if (Clock.Today != File.GetDateFromValues(Values))
            throw new Exception("Non consecutive dates found in file: " + FileName);

        MetData.today = (double)Clock.Today.Ticks;
        MetData.radn = Convert.ToSingle(Values[RadnIndex]);
        MetData.maxt = Convert.ToSingle(Values[MaxTIndex]);
        MetData.mint = Convert.ToSingle(Values[MinTIndex]);
        MetData.rain = Convert.ToSingle(Values[RainIndex]);
        if (NewMet != null)
            NewMet.Invoke(MetData);
        
        RowIndex++;
    }
}
