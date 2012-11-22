using System;
using System.Collections.Specialized;
using System.IO;
using System.Xml;
using ApsimFile;
using CSGeneral;


namespace ApsimFile
{
    /// <summary>
    /// This class implements APSIM soil parameter file importing.
    /// </summary>
    public class SoilParFile
    {
        /// <summary>
        /// Import from specified par file and returns valid soil XML file.
        /// </summary>
        static public string Import(string FileName)
        {
            if (!File.Exists(FileName))
                throw new Exception("Cannot import file " + FileName + ". File doesn't exist");
            string[] Sections = IniFile.INIReadAllSections(FileName);

            // import all water sections.
            string XmlForAllSoils = "";
            foreach (string Section in Sections)
            {
                if (GetStringValue(FileName, Section, "dlayer") != "")
                {
                    StringCollection SectionBits = StringManip.SplitStringHonouringQuotes(Section, ".");
                    if (SectionBits.Count == 3)
                    {
                        if (SectionBits[1].ToLower() == "soilwat2")
                        {
                            string SoilName = SectionBits[0];
                            Soil NewSoil = new Soil();
                            NewSoil.Name = SoilName;

                            // Add in a soil sample node.
                            Sample NewSample = new Sample();
                            NewSample.Thickness = new double[] { 100 };
                            NewSample.SW = new double[] { 0 };
                            NewSample.NO3 = new double[] { 0 };
                            NewSample.NH4 = new double[] { 0 };
                            NewSoil.Samples.Add(NewSample);

                            // Add in a phosphorus node.
                            NewSoil.Phosphorus = new Phosphorus();
                            NewSoil.Phosphorus.Thickness = new double[] { 100 };
                            NewSoil.Phosphorus.LabileP = new double[] { 5 };
                            NewSoil.Phosphorus.Sorption = new double[] { 50 };
                            NewSoil.Phosphorus.BandedP = new double[] { 0 };
                            NewSoil.Phosphorus.RockP = new double[] { 0 };


                            double[] Thickness = ReadWaterSection(SoilName, FileName, NewSoil);
                            NewSoil.Water.Thickness = Thickness;
                            ReadNitrogenSection(SoilName, FileName, NewSoil, Thickness);
                            ReadCropSections(SoilName, FileName, NewSoil, Thickness);
                            ReadPhosphorusSection(SoilName, FileName, NewSoil, Thickness);
                            XmlForAllSoils += NewSoil.ToXml();
                        }
                    }
                }
            }
            return XmlForAllSoils;
        }

        /// <summary>
        /// Read in all water variables.
        /// </summary>
        static private double[] ReadWaterSection(string SectionBit, string FileName, Soil NewSoil)
        {
            // ------------------------------------------------------------
            // Read in all water parameters from specified section and file
            // ------------------------------------------------------------
            string SectionName = SectionBit + ".soilwat2.parameters";
            NewSoil.SoilWater = new SoilWater();
            NewSoil.SoilWater.SummerU = GetDoubleValue(FileName, SectionName, "u");
            NewSoil.SoilWater.WinterU = GetDoubleValue(FileName, SectionName, "u");
            NewSoil.SoilWater.SummerCona = GetDoubleValue(FileName, SectionName, "cona");
            NewSoil.SoilWater.WinterCona = GetDoubleValue(FileName, SectionName, "cona");
            NewSoil.SoilWater.CN2Bare = GetDoubleValue(FileName, SectionName, "cn2_bare");
            NewSoil.SoilWater.Salb = GetDoubleValue(FileName, SectionName, "salb");
            NewSoil.SoilWater.DiffusConst = GetDoubleValue(FileName, SectionName, "diffus_const");
            NewSoil.SoilWater.DiffusSlope = GetDoubleValue(FileName, SectionName, "diffus_slope");
            NewSoil.SoilWater.CNRed = GetDoubleValue(FileName, SectionName, "cn_red");
            NewSoil.SoilWater.CNCov = GetDoubleValue(FileName, SectionName, "cn_cov");

            double[] Thickness = GetDoubleValues(FileName, SectionName, "dlayer");
            NewSoil.Water.LL15 = GetDoubleValues(FileName, SectionName, "ll15");
            NewSoil.Water.AirDry = GetDoubleValues(FileName, SectionName, "air_dry");
            NewSoil.Water.DUL = GetDoubleValues(FileName, SectionName, "dul");
            NewSoil.Water.SAT = GetDoubleValues(FileName, SectionName, "sat");
            NewSoil.SoilWater.SWCON = GetDoubleValues(FileName, SectionName, "swcon");
            NewSoil.SoilWater.MWCON = GetDoubleValues(FileName, SectionName, "mwcon");
            NewSoil.Water.BD = GetDoubleValues(FileName, SectionName, "bd");

            NewSoil.Samples[0].SW = GetDoubleValues(FileName, SectionName, "sw");
            return Thickness;
        }

        /// <summary>
        ///  Read in all crop parameters from all crop sections.
        /// </summary>
        static private void ReadCropSections(string SectionBit, string FileName, Soil NewSoil, double[] Thickness)
        {
            string[] Sections = IniFile.INIReadAllSections(FileName);
            foreach (string Section in Sections)
            {
                if (IniFile.INIRead(FileName, Section, "ll") != "")
                {
                    // get the crop name
                    StringCollection SectionBits = StringManip.SplitStringHonouringQuotes(Section, ".");
                    if (SectionBits.Count == 3 && SectionBits[0].ToLower() == SectionBit.ToLower())
                    {
                        string CropName = SectionBits[1];

                        double[] ll;
                        string LLValue = GetStringValue(FileName, Section, "ll");
                        if (LLValue.ToLower() == "#ll")
                            ll = NewSoil.Water.LL15;
                        else
                            ll = GetDoubleValues(FileName, Section, "ll");

                        CropName = CropName.Replace(" ", "");

                        SoilCrop NewCrop = new SoilCrop();
                        NewCrop.Name = CropName;
                        NewCrop.Thickness = NewSoil.Water.Thickness;
                        NewCrop.LL = ll;
                        NewCrop.KL = GetDoubleValues(FileName, Section, "kl");
                        NewCrop.XF = GetDoubleValues(FileName, Section, "xf");
                        NewSoil.Water.Crops.Add(NewCrop);
                    }
                }
            }
        }

        /// <summary>
        /// Read in all nitrogen parameters from specified section and file.
        /// </summary>
        static private void ReadNitrogenSection(string SectionBit, string FileName, Soil NewSoil, double[] Thickness)
        {
            string SectionName = SectionBit + ".soiln2.parameters";

            NewSoil.SoilOrganicMatter.Thickness = NewSoil.Water.Thickness;
            NewSoil.SoilOrganicMatter.RootCN = GetDoubleValue(FileName, SectionName, "root_cn");
            NewSoil.SoilOrganicMatter.RootWt = GetDoubleValue(FileName, SectionName, "root_wt");
            NewSoil.SoilOrganicMatter.SoilCN = GetDoubleValue(FileName, SectionName, "soil_cn");
            NewSoil.SoilOrganicMatter.EnrACoeff = GetDoubleValue(FileName, SectionName, "enr_a_coeff");
            NewSoil.SoilOrganicMatter.EnrBCoeff = GetDoubleValue(FileName, SectionName, "enr_b_coeff");

            NewSoil.SoilOrganicMatter.OC = GetDoubleValues(FileName, SectionName, "oc");
            NewSoil.SoilOrganicMatter.FBiom = GetDoubleValues(FileName, SectionName, "fbiom");
            NewSoil.SoilOrganicMatter.FInert = GetDoubleValues(FileName, SectionName, "finert");
            NewSoil.Analysis.Thickness = NewSoil.Water.Thickness;
            NewSoil.Analysis.PH = GetDoubleValues(FileName, SectionName, "ph");
            NewSoil.Samples[0].Thickness = NewSoil.Water.Thickness;
            NewSoil.Samples[0].NO3 = GetDoubleValues(FileName, SectionName, "no3ppm");
            NewSoil.Samples[0].NH4 = GetDoubleValues(FileName, SectionName, "nh4ppm");
        }

        /// <summary>
        /// Read in all phosphorus parameters from specified section and file
        /// </summary>
        static private void ReadPhosphorusSection(string SectionBit, string FileName, Soil NewSoil, double[] Thickness)
        {
            string SectionName = SectionBit + ".soilp.parameters";

            NewSoil.Phosphorus.Thickness = NewSoil.Water.Thickness;
            NewSoil.Phosphorus.RootCP = GetDoubleValue(FileName, SectionName, "root_cp");
            NewSoil.Phosphorus.RateDissolRock = GetDoubleValue(FileName, SectionName, "rate_dissol_rock_p");
            NewSoil.Phosphorus.RateLossAvail = GetDoubleValue(FileName, SectionName, "rate_loss_avail_p");

            NewSoil.Phosphorus.LabileP = GetDoubleValues(FileName, SectionName, "labile_P");
            NewSoil.Phosphorus.BandedP = GetDoubleValues(FileName, SectionName, "banded_P");
            NewSoil.Phosphorus.RockP = GetDoubleValues(FileName, SectionName, "rock_P");
            NewSoil.Phosphorus.Sorption = GetDoubleValues(FileName, SectionName, "sorption");
        }
        
        
        
        static private string GetStringValue(string FileName, string SectionName, string Key)
        {
            // ----------------------------------------------------------
            // Get string value from specified table for specified field.
            // ----------------------------------------------------------
            string Value = IniFile.INIRead(FileName, SectionName, Key);
            StringManip.SplitOffAfterDelimiter(ref Value, "!");
            StringManip.SplitOffAfterDelimiter(ref Value, "(");
            Value = Value.Replace("\t", "   ");
            if (Value.StartsWith("$"))
                Value = ResolveVariableMacro(FileName, Value);

            return Value;
        }
        static private double GetDoubleValue(string FileName, string SectionName, string Key)
        {
            // ----------------------------------------------------------
            // Get string value from specified table for specified field.
            // ----------------------------------------------------------
            string Value = GetStringValue(FileName, SectionName, Key);
            if (Value == "")
                return MathUtility.MissingValue;
            else
            {
                try
                {
                    return Convert.ToDouble(Value);
                }
                catch (Exception)
                {
                    throw new Exception("Cannot convert value to a floating point number" +
                                        ". Filename: " + FileName +
                                        ". Section: " + SectionName +
                                   ". Key: " + Key);
                }
            }

        }
        static private double[] GetDoubleValues(string FileName, string SectionName, string Key)
        {
            // ----------------------------------------------------------
            // Get string value from specified table for specified field.
            // ----------------------------------------------------------
            string Value = IniFile.INIRead(FileName, SectionName, Key);
            StringManip.SplitOffAfterDelimiter(ref Value, "!");
            StringManip.SplitOffAfterDelimiter(ref Value, "(");
            Value = Value.Replace("\t", "   ");

            if (Value == "")
                return new double[0];
            else
            {
                StringCollection Values = StringManip.SplitStringHonouringQuotes(Value, " ");
                double[] ReturnValues = new double[Values.Count];
                for (int i = 0; i != Values.Count; i++)
                {
                    if (Values[i].StartsWith("$"))
                        Values[i] = ResolveVariableMacro(FileName, Values[i]);
                    try
                    {
                        ReturnValues[i] = Convert.ToDouble(Values[i]);
                    }
                    catch (Exception)
                    {
                        throw new Exception("Cannot convert value to a floating point number" +
                                       ". Filename: " + FileName +
                                       ". Section: " + SectionName +
                                       ". Key: " + Key);
                    }
                }
                return ReturnValues;
            }
        }
        static private string ResolveVariableMacro(string FileName, string MacroName)
        {
            // ------------------------------------------------------------
            // Resolve the specified macro to a name and return its value.
            // NB It looks in a '*variables' section.
            // ------------------------------------------------------------
            string MacroLine = IniFile.INIRead(FileName, "*variables", MacroName);
            StringManip.SplitOffAfterDelimiter(ref MacroLine, "!");
            if (MacroLine == "")
                throw new Exception("Cannot resolve macro: " + MacroName + " in file:" + FileName);

            StringCollection MacroBits = StringManip.SplitStringHonouringQuotes(MacroLine, " ");
            if (MacroBits.Count != 3)
                throw new Exception("Invalid variables line: " + MacroLine);

            return MacroBits[1];
        }

    }
}
