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
      static string Sample = "<Sample>\n" +
                            "   <Layer>n" +
                            "      <Thickness units=\"mm\">100</Thickness>\n" +
                            "      <SW units=\"mm/mm\">0</SW>\n" +
                            "      <NO3 units=\"ppm\">0</NO3>\n" +
                            "      <NH4 units=\"ppm\">0</NH4>\n" +
                            "   </Layer>n" +
                            "</Sample>\n";
      static string Phosphorus = "<Phosphorus>\n" +
                                 "   <RootCP/>\n" +
                                 "   <RateDissolRock/>\n" +
                                 "   <RateLossAvail/>\n" +
                                 "   <Layer>\n" +
                                 "      <LabileP units=\"mg/kg\">5</LabileP>\n" +
                                 "      <Sorption units=\"-\">50</Sorption>\n" +
                                 "      <BandedP units=\"kg/ha\">0</BandedP>\n" +
                                 "      <RockP units=\"kg/ha\">0</RockP>\n" +
                                 "   </Layer>\n" +
                                 "</Phosphorus>";
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
                     XmlNode NewSoil = Soil.Create(SoilName);


                     // Add in a soil sample node.
                     XmlDocument Doc = new XmlDocument();
                     Doc.LoadXml(Sample);
                     NewSoil.AppendChild(NewSoil.OwnerDocument.ImportNode(Doc.DocumentElement, true));

                     Doc.LoadXml(Phosphorus);
                     NewSoil.AppendChild(NewSoil.OwnerDocument.ImportNode(Doc.DocumentElement, true));
                                          
                     double[] Thickness = ReadWaterSection(SoilName, FileName, NewSoil);
                     ReadNitrogenSection(SoilName, FileName, NewSoil, Thickness);
                     ReadCropSections(SoilName, FileName, NewSoil, Thickness);
                     ReadPhosphorusSection(SoilName, FileName, NewSoil, Thickness);
                     XmlForAllSoils += NewSoil.OuterXml;
                     }
                  }
               }
            }
         return XmlForAllSoils;
         }

      static private double[] ReadWaterSection(string SectionBit, string FileName, XmlNode NewSoil)
         {
         // ------------------------------------------------------------
         // Read in all water parameters from specified section and file
         // ------------------------------------------------------------
         string SectionName = SectionBit + ".soilwat2.parameters";
         Set(NewSoil, "SummerU", GetStringValue(FileName, SectionName, "u"));
         Set(NewSoil, "WinterU", GetStringValue(FileName, SectionName, "u"));
         Set(NewSoil, "SummerCona", GetStringValue(FileName, SectionName, "cona"));
         Set(NewSoil, "WinterCona", GetStringValue(FileName, SectionName, "cona"));
         Set(NewSoil, "CN2Bare", GetStringValue(FileName, SectionName, "cn2_bare"));
         Set(NewSoil, "Salb", GetStringValue(FileName, SectionName, "salb"));
         Set(NewSoil, "DiffusConst", GetStringValue(FileName, SectionName, "diffus_const"));
         Set(NewSoil, "DiffusSlope", GetStringValue(FileName, SectionName, "diffus_slope"));
         Set(NewSoil, "CNRed", GetStringValue(FileName, SectionName, "cn_red"));
         Set(NewSoil, "CNCov", GetStringValue(FileName, SectionName, "cn_cov"));
         Set(NewSoil, "CN2Bare", GetStringValue(FileName, SectionName, "cn2_bare"));

         double[] Thickness = GetDoubleValues(FileName, SectionName, "dlayer");
         Set(NewSoil, "LL15", "mm/mm", GetDoubleValues(FileName, SectionName, "ll15"), Thickness);
         Set(NewSoil, "Airdry", "mm/mm", GetDoubleValues(FileName, SectionName, "air_dry"), Thickness);
         Set(NewSoil, "DUL", "mm/mm", GetDoubleValues(FileName, SectionName, "dul"), Thickness);
         Set(NewSoil, "SAT", "mm/mm", GetDoubleValues(FileName, SectionName, "sat"), Thickness);
         Set(NewSoil, "SWCON", "0-1", GetDoubleValues(FileName, SectionName, "swcon"), Thickness);
         Set(NewSoil, "MWCON", "0-1", GetDoubleValues(FileName, SectionName, "mwcon"), Thickness);
         Set(NewSoil, "BD", "g/cc", GetDoubleValues(FileName, SectionName, "bd"), Thickness);

         Set(NewSoil, "SW", "mm/mm", GetDoubleValues(FileName, SectionName, "sw"), Thickness);
         return Thickness;
         }
      static private void Set(XmlNode NewSoil, string Name, string Value)
         {
         Soil.Set(NewSoil, new Soil.Variable(Name, Value));
         }
      static private void Set(XmlNode NewSoil, string Name, string Units, double[] Values, double[] Thickness)
         {
         Soil.Set(NewSoil, new Soil.Variable(Name, Units, Values, Thickness));
         }
      static private void ReadCropSections(string SectionBit, string FileName, XmlNode NewSoil, double[] Thickness)
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
                     {
                     Soil.Variable ll15 = Soil.Get(NewSoil, "LL15");
                     ll = ll15.Doubles;
                     }
                  else
                     ll = GetDoubleValues(FileName, Section, "ll");

                  CropName = CropName.Replace(" ", "");
                  Set(NewSoil, CropName + " LL", "mm/mm", ll, Thickness);
                  Set(NewSoil, CropName + " KL", "/day", GetDoubleValues(FileName, Section, "kl"), Thickness);
                  Set(NewSoil, CropName + " XF", "0-1", GetDoubleValues(FileName, Section, "xf"), Thickness);
                  }
               }
            }
         }
      static private void ReadNitrogenSection(string SectionBit, string FileName, XmlNode NewSoil, double[] Thickness)
         {
         // ------------------------------------------------------------
         // Read in all nitrogen parameters from specified section and file
         // ------------------------------------------------------------
         string SectionName = SectionBit + ".soiln2.parameters";

         Set(NewSoil, "RootCN", GetStringValue(FileName, SectionName, "root_cn"));
         Set(NewSoil, "RootWT", GetStringValue(FileName, SectionName, "root_wt"));
         Set(NewSoil, "SoilCN", GetStringValue(FileName, SectionName, "soil_cn"));
         Set(NewSoil, "EnrACoeff", GetStringValue(FileName, SectionName, "enr_a_coeff"));
         Set(NewSoil, "EnrBCoeff", GetStringValue(FileName, SectionName, "enr_b_coeff"));

         Set(NewSoil, "OC", "Total %", GetDoubleValues(FileName, SectionName, "oc"), Thickness);
         Set(NewSoil, "FBIOM", "0-1", GetDoubleValues(FileName, SectionName, "fbiom"), Thickness);
         Set(NewSoil, "FINERT", "0-1", GetDoubleValues(FileName, SectionName, "finert"), Thickness);
         Set(NewSoil, "PH", "1:5 water", GetDoubleValues(FileName, SectionName, "ph"), Thickness);
         Set(NewSoil, "NO3", "ppm", GetDoubleValues(FileName, SectionName, "no3ppm"), Thickness);
         Set(NewSoil, "NH4", "ppm", GetDoubleValues(FileName, SectionName, "nh4ppm"), Thickness);
         }
      static private void ReadPhosphorusSection(string SectionBit, string FileName, XmlNode NewSoil, double[] Thickness)
         {
         // -----------------------------------------------------------------
         // Read in all phosphorus parameters from specified section and file
         // -----------------------------------------------------------------
         string SectionName = SectionBit + ".soilp.parameters";

         Set(NewSoil, "RootCP", GetStringValue(FileName, SectionName, "root_cp"));
         Set(NewSoil, "RateDissolRock", GetStringValue(FileName, SectionName, "rate_dissol_rock_p"));
         Set(NewSoil, "RateLossAvail", GetStringValue(FileName, SectionName, "rate_loss_avail_p"));

         Set(NewSoil, "LabileP", "mg/kg", GetDoubleValues(FileName, SectionName, "labile_P"), Thickness);
         Set(NewSoil, "BandedP", "kg/ha", GetDoubleValues(FileName, SectionName, "banded_P"), Thickness);
         Set(NewSoil, "RockP", "kg/ha", GetDoubleValues(FileName, SectionName, "rock_P"), Thickness);
         Set(NewSoil, "Sorption", "-", GetDoubleValues(FileName, SectionName, "sorption"), Thickness);
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
