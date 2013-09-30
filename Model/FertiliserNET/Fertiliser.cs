using System;
using System.Reflection;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Text;
using System.Xml;
using System.Xml.XPath;
using ModelFramework;
using CSGeneral;

/*************************************************************
 *                     Fertiliser.NET
 *                  Justin Fainges Oct 2011
 * This is a direct port of the Fertiliser FORTRAN model. 
 * A number of variables and structures that are no longer
 * needed in C# have been omitted.
 *************************************************************/

public class Fertiliser
{
    [Param]
    XmlNode FertiliserNode = null;

    [Link]
    Paddock MyPaddock = null;

    [Input(IsOptional = true)]
    int cropsta = 0;

    [Input]
    float[] dlayer;

    [Output][Units("kg/ha")]
    float fert_applied = 0;

    [Output]
    ExternalMassFlowType externalMassFlow = new ExternalMassFlowType();

    [Event]
    public event NitrogenChangedDelegate NitrogenChanged;

    int maxLayer = 100;
    string pondActive = "no";
    int year;
    int day;
    int numLayers;

    public void ApplyFertiliser(double amount, double depth, string type)
    {
        string full_name = "";      //full name of fertilizer added
        int layer;                  //layer number of fertiliser placement
        double[] deltaArray;        //holds the changes in fertiliser 
        bool rice_crop_in;          //true or false for whether rice crop in ground or not
        ExternalMassFlowType massBalanceChange = new ExternalMassFlowType();
        List<FertComponent> components = new List<FertComponent>();

        if (amount == 0)
            Console.WriteLine("   No fertiliser supplied. Check parameters.");

        // find the layer that the fertilizer is to be added to.
        layer = getCumulativeIndex(depth, dlayer);

        deltaArray = new double[maxLayer];

        #region XML parameters
        // Read XML parameters
        XPathNavigator xNav = FertiliserNode.CreateNavigator();
        xNav.MoveToFirstChild();
        do
        {
            components.Insert(0, new FertComponent());
            components.ElementAt(0).name = xNav.Name;
            xNav.MoveToFirstChild();
            components.ElementAt(0).desc = xNav.InnerXml;
            xNav.MoveToNext();
            components.ElementAt(0).components = xNav.Value.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            xNav.MoveToNext();
            string[] str = xNav.InnerXml.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            components.ElementAt(0).fraction = new double[str.Count()];
            for (int i = 0; i < str.Count(); i++)
                components.ElementAt(0).fraction[i] = Double.Parse(str[i]);
            xNav.MoveToParent();
        } while (xNav.MoveToNext());

        #endregion

        foreach (FertComponent comp in components)
            if (comp.name.ToLower().Equals(type)) // only use components specified in type
                for (int i = 0; i < comp.fraction.Count(); i++)
                {
                    // APSIM doesn't handle calcium or P right now.
                    try
                    {
                        if (comp.components[i].ToLower().Equals("caco3") || comp.components[i].ToLower().Contains("_p"))
                            continue;
                    }
                    catch (Exception ) { }

                    if (cropsta >= 4)
                        rice_crop_in = true;
                    else
                        rice_crop_in = false;

                    // Fertiliser added to pond
                    if (pondActive.Equals("yes") && rice_crop_in)
                    {
                        deltaArray[layer] = amount * comp.fraction[i];
                        MyPaddock.Set("dlt_pond_" + comp.components[i], deltaArray);
                    }
                    else // Fertiliser added to soil
                    {
                        string dlt_name = "dlt_" + comp.components[i];

                        NitrogenChangedType NitrogenChanges = new NitrogenChangedType();
                        NitrogenChanges.Sender = "Fertiliser";
                        NitrogenChanges.SenderType = "Fertiliser";
                        NitrogenChanges.DeltaUrea = new double[dlayer.Length];
                        NitrogenChanges.DeltaNH4 = new double[dlayer.Length];
                        NitrogenChanges.DeltaNO3 = new double[dlayer.Length];

                        // This variable is being tracked - send the delta to it
                        double[] V;
                        if (MyPaddock.Get(comp.components[i], out V))
                        {
                            deltaArray[layer] = amount * comp.fraction[i];
                            // This is where the fertiliser is actually added.
                            if (comp.components[i] == "urea")
                            {
                                NitrogenChanges.DeltaUrea[layer] = amount * comp.fraction[i];
                                NitrogenChanged.Invoke(NitrogenChanges);
                            }
                            else if (comp.components[i] == "nh4")
                            {
                                NitrogenChanges.DeltaNH4[layer] = amount * comp.fraction[i];
                                NitrogenChanged.Invoke(NitrogenChanges);
                            }
                            else if (comp.components[i] == "no3")
                            {
                                NitrogenChanges.DeltaNO3[layer] = amount * comp.fraction[i];
                                NitrogenChanged.Invoke(NitrogenChanges);
                            }
                            else
                                MyPaddock.Set(dlt_name, deltaArray);

                            massBalanceChange.PoolClass = "soil";
                            massBalanceChange.FlowType = "gain";
                            massBalanceChange.DM = 0.0F;
                            massBalanceChange.C = 0.0F;
                            massBalanceChange.N = 0.0F;
                            massBalanceChange.P = 0.0F;
                            massBalanceChange.SW = 0.0F;

                            if (comp.components[i].Equals("labile_p"))
                            {
                                massBalanceChange.N = 0.0F;
                                massBalanceChange.P = (float)deltaArray.Sum();
                            }
                            else if (comp.components[i].Equals("rock_p"))
                            {
                                massBalanceChange.N = 0.0F;
                                massBalanceChange.P = (float)deltaArray.Sum();
                            }
                            else if (comp.components[i].Equals("banded_p"))
                            {
                                massBalanceChange.N = 0.0F;
                                massBalanceChange.P = (float)deltaArray.Sum();
                            }
                            else if (comp.components[i].Equals("no3"))
                            {
                                massBalanceChange.N = (float)deltaArray.Sum();
                                massBalanceChange.P = 0.0F;
                            }
                            else if (comp.components[i].Equals("nh4"))
                            {
                                massBalanceChange.N = (float)deltaArray.Sum();
                                massBalanceChange.P = 0.0F;
                            }
                            else if (comp.components[i].Equals("urea"))
                            {
                                massBalanceChange.N = (float)deltaArray.Sum();
                                massBalanceChange.P = 0.0F;
                            }
                            externalMassFlow = massBalanceChange;
                        }
                    }
                }
        fert_applied = fert_applied + (float)amount;
        Console.WriteLine("{0} kg/ha of {1} ({2}) added at depth {3} layer {4}", amount, full_name.Trim(), type.Trim(), depth, layer + 1);
    }

    [EventHandler]
    public void OnTick(TimeType tick)
    {
        DateUtility.JulianDayNumberToDayOfYear(tick.startday, out day, out year);
        fert_applied = 0;
    }

    [EventHandler]
    public void OnNewProfile(NewProfileType newProfile)
    {
        dlayer = newProfile.dlayer;
        numLayers = dlayer.Count();
    }

    [EventHandler]
    public void OnApply(FertiliserApplicationType application)
    {
        if (application.Type.Equals(" "))
            throw new Exception("Fertiliser application specification error");
        else
            ApplyFertiliser(application.Amount, application.Depth, application.Type);
    }

    private int getCumulativeIndex(double sum, float[] realArray)
    {
        float cum = 0.0f;
        for (int i = 0; i < realArray.Length; i++)
        {
            cum += realArray[i];
            if (cum >= sum)
                return i;
        }
        return realArray.Length - 1;
    }
}

class FertComponent
{
    public string name;
    public string desc;
    public string[] components;
    public double[] fraction;
}