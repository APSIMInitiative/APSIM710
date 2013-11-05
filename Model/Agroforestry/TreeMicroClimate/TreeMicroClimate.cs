using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
using ApsimFile;
using CSGeneral;

public class TreeMicroClimate
{
    [Link]
    Simulation paddock;
    [Param]
    string inputFilename;
    [Input]
    DateTime Today;

    private ModelFramework.Component localMet;
    private ModelFramework.Component fieldProps;
    private float localMint;
    private float localMaxt;
    private float localRadn;
    private float localRain;
    private string isDry;
    private string isHot;
    private string isShaded;
    private int MaxTIndex;
    private int MinTIndex;
    private int RadnIndex;
    private int RainIndex;
    private APSIMInputFile inputFile = new APSIMInputFile();

    [Output]
    public double Latitude
    {
        get
        {
            if (inputFile.Constant("Latitude") == null)
                return 0;
            else
                return Convert.ToDouble(inputFile.Constant("Latitude").Value);
        }
    }
    [Output]
    public double tav
    {
        get
        {
            if (inputFile.Constant("tav") == null)
                return 0;
            else
                return Convert.ToDouble(inputFile.Constant("tav").Value);
        }
    }
    [Output]
    public double amp
    {
        get
        {
            if (inputFile.Constant("amp") == null)
                return 0;
            else
                return Convert.ToDouble(inputFile.Constant("amp").Value);
        }
    }

    [EventHandler]
    public void OnInitialised()
    {
        inputFile.Open(inputFilename);
        MaxTIndex = StringManip.IndexOfCaseInsensitive(inputFile.Headings, "Maxt");
        MinTIndex = StringManip.IndexOfCaseInsensitive(inputFile.Headings, "Mint");
        RadnIndex = StringManip.IndexOfCaseInsensitive(inputFile.Headings, "Radn");
        RainIndex = StringManip.IndexOfCaseInsensitive(inputFile.Headings, "Rain");
        if (MaxTIndex == -1)
            throw new Exception("Cannot find MaxT in weather file: " + inputFilename);
        if (MinTIndex == -1)
            throw new Exception("Cannot find MinT in weather file: " + inputFilename);
        if (RadnIndex == -1)
            throw new Exception("Cannot find Radn in weather file: " + inputFilename);
        if (RainIndex == -1)
            throw new Exception("Cannot find Rain in weather file: " + inputFilename);
    }

    // The following event handler will be called each day at the beginning of the day
    [EventHandler]
    public void OnPrepare()
    {
        inputFile.SeekToDate(Today);
        object[] values = inputFile.GetNextLineOfData();
        foreach (Paddock p in paddock.ChildPaddocks)
        {
            fieldProps = (ModelFramework.Component)p.LinkByName("FieldProps");
            fieldProps.Get("isDry", out isDry);
            fieldProps.Get("isHot", out isHot);
            fieldProps.Get("isShaded", out isShaded);

            localMint = Convert.ToSingle(values[MinTIndex]);
            localMaxt = Convert.ToSingle(values[MaxTIndex]);
            localRadn = Convert.ToSingle(values[RadnIndex]);
            localRain = Convert.ToSingle(values[RainIndex]);

            if (isDry.Equals("yes"))
                localRain *= 0.9f;
            if (isHot.Equals("yes"))
            {
                localMint += 2;
                localMaxt += 2; //change tav/amp?
            }
            if (isShaded.Equals("yes"))
                localMaxt -= 5;

            localMet = (ModelFramework.Component)p.LinkByName("LocalClimate");
            localMet.Set("mint", localMint);
            localMet.Set("maxt", localMaxt);
            localMet.Set("radn", localRadn);
            localMet.Set("rain", localRain);
        }
    }
}