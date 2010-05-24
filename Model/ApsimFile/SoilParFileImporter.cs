using System;
using System.Collections.Specialized;
using System.IO;
using System.Xml;
using ApsimFile;
using CSGeneral;


namespace ApsimFile
   {
   // -----------------------------------------------
   // This class implements APSIM soil parameter file
   // importing.
   // -----------------------------------------------
   public class SoilParFileImporter
      {
      static public string Import(string FileName)
         {
         // ---------------------------------------------------------------
         // Import from specified par file and returns valid soil XML file.
         // ---------------------------------------------------------------
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
                     Soil NewSoil = Soil.Create(SoilName);
                     ReadWaterSection(SoilName, FileName, NewSoil);
                     ReadNitrogenSection(SoilName, FileName, NewSoil);
                     ReadCropSections(SoilName, FileName, NewSoil);
                     ReadPhosphorusSection(SoilName, FileName, NewSoil);
                     XmlForAllSoils += NewSoil.XML;
                     }
                  }
               }
            }
         return XmlForAllSoils;
         }

      static private void ReadWaterSection(string SectionBit, string FileName, Soil NewSoil)
         {
         // ------------------------------------------------------------
         // Read in all water parameters from specified section and file
         // ------------------------------------------------------------
         string SectionName = SectionBit + ".soilwat2.parameters";
         NewSoil.SetProperty("SummerU", GetStringValue(FileName, SectionName, "u"));
         NewSoil.SetProperty("WinterU", GetStringValue(FileName, SectionName, "u"));
         NewSoil.SetProperty("SummerCona", GetStringValue(FileName, SectionName, "cona"));
         NewSoil.SetProperty("WinterCona", GetStringValue(FileName, SectionName, "cona"));
         NewSoil.SetProperty("CN2Bare", GetStringValue(FileName, SectionName, "cn2_bare"));
         NewSoil.SetProperty("Salb", GetStringValue(FileName, SectionName, "salb"));
         NewSoil.SetProperty("DiffusConst", GetStringValue(FileName, SectionName, "diffus_const"));
         NewSoil.SetProperty("DiffusSlope", GetStringValue(FileName, SectionName, "diffus_slope"));
         NewSoil.SetProperty("CNRed", GetStringValue(FileName, SectionName, "cn_red"));
         NewSoil.SetProperty("CNCov", GetStringValue(FileName, SectionName, "cn_cov"));
         NewSoil.SetProperty("CN2Bare", GetStringValue(FileName, SectionName, "cn2_bare"));
         NewSoil.SetVariable("Thickness", GetDoubleValues(FileName, SectionName, "dlayer"));
         NewSoil.SetVariable("LL15", GetDoubleValues(FileName, SectionName, "ll15"));
         NewSoil.SetVariable("Airdry", GetDoubleValues(FileName, SectionName, "air_dry"));
         NewSoil.SetVariable("DUL", GetDoubleValues(FileName, SectionName, "dul"));
         NewSoil.SetVariable("SAT", GetDoubleValues(FileName, SectionName, "sat"));
         NewSoil.SetVariable("SWCON", GetDoubleValues(FileName, SectionName, "swcon"));
         NewSoil.SetVariable("MWCON", GetDoubleValues(FileName, SectionName, "mwcon"));
         NewSoil.SetVariable("BD", GetDoubleValues(FileName, SectionName, "bd"));

         NewSoil.SetVariable("SW", GetDoubleValues(FileName, SectionName, "sw"), "Sample");
         }
      static private void ReadCropSections(string SectionBit, string FileName, Soil NewSoil)
         {
         // ------------------------------------------------------------
         // Read in all crop parameters from all crop sections in 
         // specified file
         // ------------------------------------------------------------
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
                     ll = NewSoil.Variable("LL15");
                  else
                     ll = GetDoubleValues(FileName, Section, "ll");

                  NewSoil.SetVariable(CropName + " LL", ll);
                  NewSoil.SetVariable(CropName + " KL", GetDoubleValues(FileName, Section, "kl"));
                  NewSoil.SetVariable(CropName + " XF", GetDoubleValues(FileName, Section, "xf"));
                  }
               }
            }
         }
      static private void ReadNitrogenSection(string SectionBit, string FileName, Soil NewSoil)
         {
         // ------------------------------------------------------------
         // Read in all nitrogen parameters from specified section and file
         // ------------------------------------------------------------
         string SectionName = SectionBit + ".soiln2.parameters";

         NewSoil.SetProperty("RootCN", GetStringValue(FileName, SectionName, "root_cn"));
         NewSoil.SetProperty("RootWT", GetStringValue(FileName, SectionName, "root_wt"));
         NewSoil.SetProperty("SoilCN", GetStringValue(FileName, SectionName, "soil_cn"));
         NewSoil.SetProperty("EnrACoeff", GetStringValue(FileName, SectionName, "enr_a_coeff"));
         NewSoil.SetProperty("EnrBCoeff", GetStringValue(FileName, SectionName, "enr_b_coeff"));

         NewSoil.SetVariable("PH", GetDoubleValues(FileName, SectionName, "ph"));
         NewSoil.SetVariable("OC", GetDoubleValues(FileName, SectionName, "oc"));
         NewSoil.SetVariable("FBIOM", GetDoubleValues(FileName, SectionName, "fbiom"));
         NewSoil.SetVariable("FINERT", GetDoubleValues(FileName, SectionName, "finert"));
         NewSoil.SetVariable("NO3", GetDoubleValues(FileName, SectionName, "no3ppm"), "Sample");
         NewSoil.SetVariable("NH4", GetDoubleValues(FileName, SectionName, "nh4ppm"), "Sample");
         }
      static private void ReadPhosphorusSection(string SectionBit, string FileName, Soil NewSoil)
         {
         // -----------------------------------------------------------------
         // Read in all phosphorus parameters from specified section and file
         // -----------------------------------------------------------------
         string SectionName = SectionBit + ".soilp.parameters";

         NewSoil.SetProperty("RootCP", GetStringValue(FileName, SectionName, "root_cp"));
         NewSoil.SetProperty("RateDissolRock", GetStringValue(FileName, SectionName, "rate_dissol_rock_p"));
         NewSoil.SetProperty("RateLossAvail", GetStringValue(FileName, SectionName, "rate_loss_avail_p"));

         NewSoil.SetVariable("LabileP", GetDoubleValues(FileName, SectionName, "labile_P"));
         NewSoil.SetVariable("BandedP", GetDoubleValues(FileName, SectionName, "banded_P"));
         NewSoil.SetVariable("RockP", GetDoubleValues(FileName, SectionName, "rock_P"));
         NewSoil.SetVariable("Sorption", GetDoubleValues(FileName, SectionName, "sorption"));
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
