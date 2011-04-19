
using System;
using System.Collections;
using System.Collections.Specialized;
using System.Data;
using System.IO;
//using System.Windows.Forms;
using System.Xml;
using ApsimFile;
using CSGeneral;
//using ExcelUtility;
using System.Collections.Generic;
using ExcelUtility;


namespace ApsimFile
   {
   public class SoilSpreadsheet
      {
      public delegate void ProgressNotifier(int Percent);


      private static string[] VarNames = {"Name", "Country", "State", "Region", "NearestTown", "Site", 
                                  "ApsoilNumber", "SoilType", "Latitude", "Longitude",
                                  "LocationAccuracy", "DataSource", "Comments", "NaturalVegetation",
                                  "MunsellColour",
                                  "Thickness (mm)", 
                                  "BD (g/cc)",
                                  "Rocks (%)",
                                  "Texture",
                                  "SAT (mm/mm)",
                                  "DUL (mm/mm)",
                                  "LL15 (mm/mm)",
                                  "AirDry (mm/mm)", 
                                  "Wheat LL (mm/mm)",
                                  "Wheat KL(/day)",
                                  "Wheat XF(0-1)",
                                  "Barley LL (mm/mm)",
                                  "Barley KL(/day)",
                                  "Barley XF(0-1)",
                                  "Oats LL (mm/mm)",
                                  "Oats KL(/day)",
                                  "Oats XF(0-1)",
                                  "Canola LL (mm/mm)",
                                  "Canola KL(/day)",
                                  "Canola XF(0-1)",
                                  "Chickpea LL (mm/mm)",
                                  "Chickpea KL(/day)",
                                  "Chickpea XF(0-1)",
                                  "Cotton LL (mm/mm)",
                                  "Cotton KL(/day)",
                                  "Cotton XF(0-1)",
                                  "Sorghum LL (mm/mm)",
                                  "Sorghum KL(/day)",
                                  "Sorghum XF(0-1)",
                                  "SummerU",
                                  "SummerCona",
                                  "WinterU",
                                  "WinterCona",
                                  "SummerDate",
                                  "WinterDate",
                                  "Salb",
                                  "DiffusConst",
                                  "DiffusSlope",
                                  "Cn2Bare",
                                  "CnRed",
                                  "CnCov",
                                  "RootCn",
                                  "RootWt",
                                  "SoilCn",
                                  "EnrACoeff",
                                  "EnrBCoeff",
                                  "SWCON (0-1)",
                                  "MWCON (0-1)",
                                  "FBiom(0-1)",
                                  "FInert(0-1)",
                                  "KS (mm/day)",
                                  "ThicknessChem (mm)",
                                  "OC (Walkley Black %)",
                                  "EC (1:5 dS/m)",
                                  "PH (1:5 water)",
                                  "CL (mg/kg)",
                                  "Boron (mg/kg)",
                                  "CEC (cmol+/kg)",
                                  "Ca (cmol+/kg)",
                                  "Mg (cmol+/kg)",
                                  "Na (cmol+/kg)",
                                  "K (cmol+/kg)", 
                                  "ESP (%)",
                                  "Mn (mg/kg)",
                                  "Al (cmol+/kg)",
                                  "ParticleSizeSand (%)",
                                  "ParticleSizeSilt (%)",
                                  "ParticleSizeClay (%)"};

      private static string[] PropertyNames = {"Name", "Country", "State", "Region", "NearestTown", "Site", 
                                  "ApsoilNumber", "SoilType", "Latitude", "Longitude",
                                  "LocationAccuracy", "DataSource", "Comments", "NaturalVegetation",
                                  "SummerCona",
                                  "SummerU",
                                  "SummerDate",
                                  "WinterCona",
                                  "WinterU",
                                  "WinterDate",
                                  "DiffusConst",
                                  "DiffusSlope",
                                  "Salb",
                                  "Cn2Bare",
                                  "CnRed",
                                  "CnCov",
                                  "CnCanopyFact",
                                  "Slope",
                                  "DischargeWidth",
                                  "CatchmentArea",
                                  "MaxPond",
                                  "RootCn",
                                  "RootWt",
                                  "SoilCn",
                                  "EnrACoeff",
                                  "EnrBCoeff"};
      private static string[] CodeVariableNames = {"BD", "AirDry", "LL15", "DUL", "SAT", "Rocks",
                                                   "Texture", "MunsellColour", 
                                                   "OC", "EC", "PH", "CL", "Boron", "CEC", "Ca", "Mg", 
                                                   "Na", "K", "ESP", "Mn", "Al", 
                                                   "ParticleSizeSand", "ParticleSizeSilt", "ParticleSizeClay",
                                                   "LL"};

      private static string[] ChemVariableNames = {"OC", "EC", "PH", "CL", "Boron", "CEC", "Ca", "Mg", 
                                                   "Na", "K", "ESP", "Mn", "Al", 
                                                   "ParticleSizeSand", "ParticleSizeSilt", "ParticleSizeClay",
                                                   "FBIOM", "FINERT"};
      private static string[] CropList = {
                                          "fieldpea", "mungbean", "sunflower", "fababean", "lucerne", "maize",
                                          "perennialgrass", "cowpea", "navybean", "peanut", "pigeonpea", "soybean",
                                          "stylo", "sugar", "lablab", "millet", "triticale", "weed", "medic",
                                          "Lupins", "lentils", "oatenhay", "broccoli", "peas", "Vetch", "potatoes",
                                          "poppy", "butterflypea", "burgundybean", "desmanthus_v", "centro", "caatingastylo",
                                          "brazilianstylo", "desmanthus_per", "rice"};
      public SoilSpreadsheet()
         {
         }


      static public string OpenXLS(string FileName, ProgressNotifier Notifier)
         {
         // --------------------------------------------------------------
         // Read through all rows and columns in the specified XLS file
         // and create a temporaty .soils file with all the 
         // soils found in the spreadsheet. Return the temporary file to caller
         // --------------------------------------------------------------
         
         DataTable Table = ExcelHelper.GetDataFromSheet(FileName, "SoilData");

         // remove DepthMidPoints and Depth columns if they exist.


         XmlDocument Doc = new XmlDocument();
         XmlNode AllSoils = Doc.CreateElement("folder");
         Doc.AppendChild(AllSoils);
         XmlHelper.SetName(AllSoils, "Soils");
         XmlHelper.SetAttribute(AllSoils, "version", APSIMChangeTool.CurrentVersion.ToString());
         
         // Loop through all blocks of rows in datatable from XLS, create a
         // soil and store soil in correct location in the AllSoils XML.
         int Row = 0;
         int counter = 0;
         while (Row < Table.Rows.Count)
            {
            XmlNode SoilNode = AllSoils.AppendChild(Doc.ImportNode(Soil.Create("soil"), true));
            int NumRows = FillSoilFromTable(SoilNode, Table, Row);

            // Make sure the DataSource and Comment nodes under the Water node are multiedits.
            XmlNode DataSourceNode = XmlHelper.Find(SoilNode, "DataSource");
            if (DataSourceNode != null)
               XmlHelper.SetAttribute(DataSourceNode, "type", "multiedit");
            XmlNode CommentNode = XmlHelper.Find(SoilNode, "Comments");
            if (CommentNode != null)
               XmlHelper.SetAttribute(CommentNode, "type", "multiedit");

            AddSoilToXML(SoilNode, AllSoils);

            Row += NumRows;

            if (Notifier != null)
               Notifier.Invoke(Convert.ToInt32(Row * 100.0 / Table.Rows.Count));
            counter++;
            if (counter == 50)
               {
               GC.Collect();
               counter = 0;
               }

            }
         string TempFileName = Path.GetTempFileName();
         Doc.Save(TempFileName);
         return TempFileName;
         }

      /// <summary>
      /// Add the specified soil to the AllSoils node.
      /// </summary>
      private static void AddSoilToXML(XmlNode NewSoil, XmlNode AllSoils)
         {
         string SoilPath = CalcPathFromSoil(NewSoil);
         XmlNode ParentNode;
         if (SoilPath == "")
            ParentNode = AllSoils;
         else
            ParentNode = EnsureNodeExists(AllSoils, SoilPath);

         ParentNode.AppendChild(NewSoil);
         }

      /// <summary>
      /// Calculate a path for the specified soil.
      /// </summary>
      /// <param name="NewSoil"></param>
      /// <returns></returns>
      private static string CalcPathFromSoil(XmlNode NewSoil)
         {
         Soil.Variable Country = Soil.Get(NewSoil, "Country");
         Soil.Variable State = Soil.Get(NewSoil, "State");
         Soil.Variable Region = Soil.Get(NewSoil, "Region");
         if (Country.Value == "")
            return "";
         else
            {
            string Path = Country.Value;
            if (State.Value != "")
               Path += "/" + State.Value;
            else
               return Path;
            if (Region.Value != "")
               Path += "/" + Region.Value;
            return Path;
            }
         }

      /// <summary>
      /// Ensure a node exists by creating nodes as necessary
      /// for the specified node path.
      /// </summary>
      public static XmlNode EnsureNodeExists(XmlNode Node, string NodePath)
         {
         if (NodePath.Length == 0)
            return Node;

         int PosDelimiter = NodePath.IndexOf(XmlHelper.Delimiter);
         string ChildNameToMatch = NodePath;
         if (PosDelimiter != -1)
            ChildNameToMatch = NodePath.Substring(0, PosDelimiter);

         foreach (XmlNode Child in Node.ChildNodes)
            {
            if (XmlHelper.Name(Child).ToLower() == ChildNameToMatch.ToLower())
               {
               if (PosDelimiter == -1)
                  return Child;
               else
                  return EnsureNodeExists(Child, NodePath.Substring(PosDelimiter + 1));
               }
            }

         // Didn't find the child node so add one and continue.
         XmlNode NewChild = Node.AppendChild(Node.OwnerDocument.CreateElement("Folder"));
         XmlHelper.SetName(NewChild, ChildNameToMatch);

         if (PosDelimiter == -1)
            return NewChild;
         else
            return EnsureNodeExists(NewChild, NodePath.Substring(PosDelimiter + 1));
         }

      static public void Export(string FileName, XmlNode Xml, StringCollection XMLPaths)
         {
         // --------------------------------------------------------------
         // Export the specific XML nodes (as specified by XMLPaths) from
         // the specified Xml to the given XLS filename.
         // --------------------------------------------------------------

         File.Delete(FileName);
         DataTable Table = new DataTable("SoilData");
         int Row = 0;
         foreach (string SelectedPath in XMLPaths)
            {
            XmlNode SelectedNode = XmlHelper.Find(Xml, SelectedPath);
            if (SelectedNode != null)
               CreateTableFromData(SelectedNode, Table, SelectedPath, ref Row);
            }
         // Now go through the table and move as many units from the code column to the column header
         // as possible.
         MoveUnitsFromCodesToHeader(Table);

         // Give the table to excel.
         ExcelHelper.SendDataToSheet(FileName, "SoilData", Table);
         }

      /// <summary>
      /// Return true if the specified variable is a chem variable.
      /// </summary>
      private static bool IsChemVariable(string VariableName)
         {
         StringManip.SplitOffBracketedValue(ref VariableName, '(', ')');
         return StringManip.IndexOfCaseInsensitive(ChemVariableNames, VariableName) != -1;
         }
      /// <summary>
      /// Return true if the specified variable is a code variable.
      /// </summary>
      private static bool IsCodeVariable(string VariableName)
         {
         StringManip.SplitOffBracketedValue(ref VariableName, '(', ')');
         if (VariableName.Contains(" "))
            VariableName = VariableName.Substring(VariableName.IndexOf(' ') + 1);
         return StringManip.IndexOfCaseInsensitive(CodeVariableNames, VariableName) != -1;
         }

      static private void CreateTableFromData(XmlNode Data, DataTable Table, string ChildPath, ref int Row)
         {
         // --------------------------------------------------------------
         // Look at the Data node passed in. If it's a <soil> node then
         // create a table with all the soil data. If it's a <soils> or
         // a <folder> node then recursively go down looking for soil nodes.
         // The end result is a DataTable populated with data for
         // all soil nodes found.
         // --------------------------------------------------------------

         if (XmlHelper.Type(Data).ToLower() == "soil")
            FillTableFromSoil(Table, Data);

         foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
            {
            if (XmlHelper.Type(Child).ToLower() == "soil" ||
                XmlHelper.Type(Child).ToLower() == "soils" || XmlHelper.Type(Child).ToLower() == "folder")
               CreateTableFromData(Child, Table, ChildPath + "/" + XmlHelper.Name(Child), ref Row); // recursion
            }
         }

      private static List<string> SortCrops(string[] Crops)
         {
         List<string> ReturnCrops = new List<string>();
         ReturnCrops.AddRange(CropList);

         foreach (string Crop in Crops)
            {
            if (StringManip.IndexOfCaseInsensitive(CropList, Crop) == -1)
               ReturnCrops.Add(Crop);
            }

         return ReturnCrops;
         }

      /// <summary>
      /// Fill the specified SoilNode with data from the specified table.
      /// </summary>
      private static int FillSoilFromTable(XmlNode SoilNode, DataTable Table, int StartRow)
         {
         string[] Names = DataTableUtility.GetColumnAsStrings(Table, "Name");
         if (Names.Length == 0)
            throw new Exception("Cannot find a name column in spreadsheet");

         // Calculate how many rows for this soil
         int NumRows = 1;
         for (int i = StartRow + 1; i < Names.Length; i++)
            {
            if (Names[i] == Names[StartRow])
               NumRows++;
            else
               break;
            }

         // Set the name.
         XmlHelper.SetName(SoilNode, Names[StartRow]);

         // Try and get a Thickness variable.
         double[] Thickness = null;
         if (Table.Columns.Contains("Thickness (mm)"))
            Thickness = DataTableUtility.GetColumnAsDoubles(Table, "Thickness (mm)", NumRows, StartRow);
         if (Thickness == null)
            throw new Exception("Cannot find a thickness column in spreadsheet.");
         Thickness = MathUtility.RemoveMissingValuesFromBottom(Thickness);

         // Try and get a ThicknessChem variable.
         double[] ThicknessChem = null;
         if (Table.Columns.Contains("ThicknessChem (mm)"))
            ThicknessChem = DataTableUtility.GetColumnAsDoubles(Table, "ThicknessChem (mm)", NumRows, StartRow);
         else
            ThicknessChem = Thickness;
         ThicknessChem = MathUtility.RemoveMissingValuesFromBottom(ThicknessChem);

         // Loop through all variables and store in our soil.
         foreach (DataColumn Column in Table.Columns)
            {
            if (Column.ColumnName != "Name" && 
                Column.ColumnName != "Thickness (mm)" &&
                Column.ColumnName != "ThicknessChem (mm)" &&
                !Column.ColumnName.Contains("Code"))
               {
               string VariableName = Column.ColumnName;
               string Units = StringManip.SplitOffBracketedValue(ref VariableName, '(', ')');

               // Work out codes and units
               string[] Codes = null;
               if (Table.Columns.Contains(VariableName + "Code"))
                  {
                  Codes = DataTableUtility.GetColumnAsStrings(Table, VariableName + "Code", NumRows, StartRow);
                  
                  for (int i = 0; i < Codes.Length; i++)
                     {
                     string CodeUnits = StringManip.SplitOffBracketedValue(ref Codes[i], '(', ')');
                     if (CodeUnits != "")
                        Units = CodeUnits;
                     }
                  }

               // Get the values.
               string[] Values = DataTableUtility.GetColumnAsStrings(Table, Column.ColumnName, NumRows, StartRow);
               bool IsCrop = VariableName.Contains(" ");
               if (!IsCrop || MathUtility.ValuesInArray(Values))
                  {
                  Soil.Variable Var;
                  if (IsPropertyName(VariableName))
                     Var = new Soil.Variable(VariableName, Values[0]);
                  else
                     {
                     if (IsChemVariable(VariableName))
                        Var = new Soil.Variable(VariableName, Units, Values, ThicknessChem, SoilNode);
                     else
                        Var = new Soil.Variable(VariableName, Units, Values, Thickness, SoilNode);
                     }
                  Var.Codes = Codes;
                  Soil.Set(SoilNode, Var);
                  }
               else
                  {
                  // Even though we don't have any values still check for units otherwise
                  // later it will throw and exception when user navigates to that variable.
                  Soil.CheckUnits(VariableName, Units);
                  }
               }
            }
         return NumRows;
         }

      /// <summary>
      /// Return true if variable is a property variable.
      /// </summary>
      private static bool IsPropertyName(string VariableName)
         {
         return StringManip.IndexOfCaseInsensitive(PropertyNames, VariableName) != -1;
         }

      /// <summary>
      /// Fill the specified table with data from the specified soil.
      /// </summary>
      private static void FillTableFromSoil(DataTable Table, XmlNode SoilNode)
         {
         // Get a list of variables to put into table.
         List<string> VariableNames = new List<string>();
         VariableNames.AddRange(VarNames);
         List<string> Crops = SortCrops(Soil.CropsMeasured(SoilNode));
         foreach (string Crop in Crops)
            {
            VariableNames.Add(Crop + " LL(mm/mm)");
            VariableNames.Add(Crop + " KL(/day)");
            VariableNames.Add(Crop + " XF(0-1)");
            }

         // Get a Thickness
         Soil.Variable Var = Soil.Get(SoilNode, "LL15");
         double[] Thickness = Var.ThicknessMM;

         // Get a ThicknessChem
         Var = Soil.Get(SoilNode, "PH");
         double[] ThicknessChem = Var.ThicknessMM;

         int NumValues = Math.Max(Thickness.Length, ThicknessChem.Length);
         int StartRow = Table.Rows.Count;

         foreach (string VariableName in VariableNames)
            {
            string Name = VariableName;
            string Units = StringManip.SplitOffBracketedValue(ref Name, '(', ')');
            string[] Values = null;
            string[] Codes = null;

            if (Name == "Thickness")
               {
               Values = MathUtility.DoublesToStrings(Thickness);
               Units = "mm";
               }
            else if (Name == "ThicknessChem")
               {
               Values = MathUtility.DoublesToStrings(ThicknessChem);
               Units = "mm";
               }
            else if (Name == "Name")
               Values = StringManip.CreateStringArray(XmlHelper.Name(SoilNode), NumValues);
            else if (Name.Contains(" "))
               {
               // Only put crop variable values into the table where we have crop variables.
               string CropName = Name.Substring(0, Name.IndexOf(" "));
               if (Array.IndexOf(Soil.CropsMeasured(SoilNode), CropName) != -1)
                  {
                  Var = Soil.Get(SoilNode, Name);
                  Var.Units = Units;
                  Values = Var.Strings;
                  Codes = Var.Codes;
                  Units = Var.Units;
                  }
               else
                  Codes = StringManip.CreateStringArray("", NumValues);
               }
            else
               {
               Var = Soil.Get(SoilNode, Name);
               if (IsPropertyName(Name))
                  {
                  if (Var.Value != null)
                     Values = StringManip.CreateStringArray(Var.Value, NumValues);
                  }
               else
                  {
                  // layered variable.
                  //Var.Units = Units;
                  Values = Var.Strings;
                  Codes = Var.Codes;
                  Units = Var.Units;
                  }
               }
            // If we don't have any values for this variale then create empty ones. This can
            // happen for many of the crops.
            if (Values == null)
               Values = StringManip.CreateStringArray("", NumValues);

            // If we are to write a code column then put all units in code column for now. 
            // We'll go through later and selectively remove them later.
            if (IsCodeVariable(Name))
               {
               if (Codes == null)
                  throw new Exception("Didn't find a code column for variable " + VariableName);
               for (int i = 0; i < Codes.Length; i++)
                  {
                  if (Units != "" && Codes[i] != "")
                     Codes[i] = Codes[i] + " (" + Units + ")";
                  }
               DataTableUtility.AddColumn(Table, Name, Values, StartRow, NumValues);
               DataTableUtility.AddColumn(Table, Name + "Code", Codes, StartRow, NumValues);
               }
            else
               DataTableUtility.AddColumn(Table, VariableName, Values, StartRow, NumValues);

            }

         }

      /// <summary>
      /// Go through the table and move as many units from the code column to the column header
      /// as possible.
      /// </summary>
      // <param name="Table"></param>
      private static void MoveUnitsFromCodesToHeader(DataTable Table)
         {
         foreach (DataColumn Column in Table.Columns)
            {
            if (Column.ColumnName.Contains("Code"))
               {
               bool MultipleUnitsFound = false;
               string Units = null;
               foreach (DataRow Row in Table.Rows)
                  {
                  string Code = Row[Column].ToString();
                  string CodeUnits = StringManip.SplitOffBracketedValue(ref Code, '(', ')');
                  if (CodeUnits != "")
                     {
                     if (Units == null)
                        Units = CodeUnits;
                     if (Units != null && Units != CodeUnits)
                        {
                        MultipleUnitsFound = true;
                        break;
                        }
                     }
                  }
               string NonCodeColumnName = Column.ColumnName.Replace("Code", "");
               if (!MultipleUnitsFound && Units == null && Soil.ValidUnits(NonCodeColumnName).Count >= 1)
                  {
                  // This can happen when an entire column has no values in the code column e.g. Rocks. 
                  // Need to make sure that column header at least has some units.

                  Units = Soil.ValidUnits(NonCodeColumnName)[0];

                  // Find the non code column and give it units.
                  int ColumnIndex = Table.Columns.IndexOf(NonCodeColumnName);
                  if (ColumnIndex == -1)
                     throw new Exception("Cannot find column " + NonCodeColumnName);
                  Table.Columns[ColumnIndex].ColumnName += " (" + Units + ")";
                  }
               else if (!MultipleUnitsFound && !VariablesHasMultipleUnits(NonCodeColumnName))
                  {
                  foreach (DataRow Row in Table.Rows)
                     {
                     string Code = Row[Column].ToString();
                     string CodeUnits = StringManip.SplitOffBracketedValue(ref Code, '(', ')');
                     Row[Column] = Code;
                     }
                  // Find the non code column and give it units.
                  int ColumnIndex = Table.Columns.IndexOf(NonCodeColumnName);
                  if (ColumnIndex == -1)
                     throw new Exception("Cannot find column " + NonCodeColumnName);

                  Table.Columns[ColumnIndex].ColumnName += " (" + Units + ")";
                  }
               }
            }
         }

      /// <summary>
      /// Return true if the specified variable can have multiple units / methods.
      /// </summary>
      private static bool VariablesHasMultipleUnits(string VariableName)
         {
         return Soil.ValidUnits(VariableName).Count > 1;
         }

      }
   }
