using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data;
using System.Xml;
using System.IO;
using CSGeneral;

namespace ApsimFile
{
    public class SoilDataTable
    {
        public delegate void ProgressNotifier(int Percent);


        private static string[] CropList = {
                                          "fieldpea", "mungbean", "sunflower", "fababean", "lucerne", "maize",
                                          "perennialgrass", "cowpea", "navybean", "peanut", "pigeonpea", "soybean",
                                          "stylo", "sugar", "lablab", "millet", "triticale", "weed", "medic",
                                          "Lupins", "lentils", "oatenhay", "broccoli", "peas", "Vetch", "potatoes",
                                          "poppy", "butterflypea", "burgundybean", "desmanthus_v", "centro", "caatingastylo",
                                          "brazilianstylo", "desmanthus_per", "rice", "mustard", "chickory"};


        /// <summary>
        /// Convert a datatable to a soils XML file. Can handle multiple soils in the table.
        /// </summary>
        public static XmlNode TableToSoilXML(DataTable Table, ProgressNotifier Notifier = null)
        {
            XmlDocument Doc = new XmlDocument();
            XmlNode SoilsNode = Doc.CreateElement("folder");
            Doc.AppendChild(SoilsNode);
            XmlHelper.SetName(SoilsNode, "Soils");
            XmlHelper.SetAttribute(SoilsNode, "version", APSIMChangeTool.CurrentVersion.ToString());

            Soil[] Soils = TableToSoils(Table, Notifier);
            foreach (Soil Soil in Soils)
                AddSoilToXML(Soil, SoilsNode);

            return Doc.DocumentElement;
        }

        /// <summary>
        /// Fill the specified SoilNode with data from the specified table.
        /// </summary>
        public static Soil[] TableToSoils(DataTable Table, ProgressNotifier Notifier = null)
        {
            List<Soil> Soils = new List<Soil>();

            string[] Names = DataTableUtility.GetColumnAsStrings(Table, "Name");
            if (Names.Length == 0)
                throw new Exception("Cannot find a name column in spreadsheet");

            // Loop through all blocks of rows in datatable from XLS, create a
            // soil and store soil in correct location in the AllSoils XML.
            int StartRow = 0;
            int counter = 0;
            while (StartRow < Table.Rows.Count)
            {
                // Calculate how many rows for this soil
                int NumRows = 1;
                for (int i = StartRow + 1; i < Names.Length; i++)
                {
                    if (Names[i] == Names[StartRow])
                        NumRows++;
                    else
                        break;
                }

                Soil NewSoil = new Soil();
                Soils.Add(NewSoil);
                NewSoil.Name = Names[StartRow];
                NewSoil.Country = GetStringValue(Table, StartRow, "Country");
                NewSoil.State = GetStringValue(Table, StartRow, "State");
                NewSoil.Region = GetStringValue(Table, StartRow, "Region");
                NewSoil.NearestTown = GetStringValue(Table, StartRow, "NearestTown");
                NewSoil.Site = GetStringValue(Table, StartRow, "Site");
                NewSoil.ApsoilNumber = GetStringValue(Table, StartRow, "APSoilNumber");
                NewSoil.SoilType = GetStringValue(Table, StartRow, "Soil type, texture or other descriptor");
                NewSoil.LocalName = GetStringValue(Table, StartRow, "LocalName");
                NewSoil.ASCOrder = GetStringValue(Table, StartRow, "ASC_Order");
                NewSoil.ASCSubOrder = GetStringValue(Table, StartRow, "ASC_Sub-order");
                NewSoil.Latitude = GetDoubleValue(Table, StartRow, "Latitude");
                NewSoil.Longitude = GetDoubleValue(Table, StartRow, "Longitude");
                NewSoil.LocationAccuracy = GetStringValue(Table, StartRow, "LocationAccuracy");
                NewSoil.YearOfSampling = GetIntegerValue(Table, StartRow, "YearOfSampling");
                NewSoil.DataSource = GetStringValue(Table, StartRow, "DataSource");
                NewSoil.Comments = GetStringValue(Table, StartRow, "Comments");
                NewSoil.NaturalVegetation = GetStringValue(Table, StartRow, "NaturalVegetation");
                NewSoil.RecordNumber = GetIntegerValue(Table, StartRow, "RecordNo");
                NewSoil.Analysis.MunsellColour = GetStringValues(Table, "MunsellColour", NumRows, StartRow);
                NewSoil.Analysis.MunsellMetadata = GetCodeValues(Table, "MunsellColourCode", NumRows, StartRow);

                NewSoil.Water.Thickness = MathUtility.RemoveMissingValuesFromBottom(GetDoubleValues(Table, "Thickness (mm)", NumRows, StartRow));
                int NumLayers = NewSoil.Water.Thickness.Length;
                NewSoil.Water.BD = GetDoubleValues(Table, "BD (g/cc)", NumLayers, StartRow);
                NewSoil.Water.BDMetadata = GetCodeValues(Table, "BDCode", NumLayers, StartRow);
                NewSoil.Analysis.Rocks = GetDoubleValues(Table, "Rocks (%)", NumRows, StartRow);
                NewSoil.Analysis.RocksMetadata = GetCodeValues(Table, "RocksCode", NumRows, StartRow);
                NewSoil.Analysis.Texture = GetStringValues(Table, "Texture", NumRows, StartRow);
                NewSoil.Analysis.TextureMetadata = GetCodeValues(Table, "TextureCode", NumRows, StartRow);
                NewSoil.Water.SAT = GetDoubleValues(Table, "SAT (mm/mm)", NumLayers, StartRow);
                NewSoil.Water.SATMetadata = GetCodeValues(Table, "SATCode", NumLayers, StartRow);
                NewSoil.Water.DUL = GetDoubleValues(Table, "DUL (mm/mm)", NumLayers, StartRow);
                NewSoil.Water.DULMetadata = GetCodeValues(Table, "DULCode", NumLayers, StartRow);
                NewSoil.Water.LL15 = GetDoubleValues(Table, "LL15 (mm/mm)", NumLayers, StartRow);
                NewSoil.Water.LL15Metadata = GetCodeValues(Table, "LL15Code", NumLayers, StartRow);
                NewSoil.Water.AirDry = GetDoubleValues(Table, "Airdry (mm/mm)", NumLayers, StartRow);
                NewSoil.Water.AirDryMetadata = GetCodeValues(Table, "AirdryCode", NumLayers, StartRow);

                NewSoil.SoilWater = new SoilWater();
                NewSoil.SoilWater.Thickness = NewSoil.Water.Thickness;
                NewSoil.SoilWater.SummerU = GetDoubleValue(Table, StartRow, "SummerU");
                NewSoil.SoilWater.SummerCona = GetDoubleValue(Table, StartRow, "SummerCona");
                NewSoil.SoilWater.WinterU = GetDoubleValue(Table, StartRow, "WinterU");
                NewSoil.SoilWater.WinterCona = GetDoubleValue(Table, StartRow, "WinterCona");
                NewSoil.SoilWater.SummerDate = GetStringValue(Table, StartRow, "SummerDate");
                NewSoil.SoilWater.WinterDate = GetStringValue(Table, StartRow, "WinterDate");
                NewSoil.SoilWater.Salb = GetDoubleValue(Table, StartRow, "Salb");
                NewSoil.SoilWater.DiffusConst = GetDoubleValue(Table, StartRow, "DiffusConst");
                NewSoil.SoilWater.DiffusSlope = GetDoubleValue(Table, StartRow, "DiffusSlope");
                NewSoil.SoilWater.CN2Bare = GetDoubleValue(Table, StartRow, "Cn2Bare");
                NewSoil.SoilWater.CNRed = GetDoubleValue(Table, StartRow, "CnRed");
                NewSoil.SoilWater.CNCov = GetDoubleValue(Table, StartRow, "CnCov");

                if (Table.Columns.Contains("ThicknessChem (mm)"))
                    NewSoil.SoilOrganicMatter.Thickness = MathUtility.RemoveMissingValuesFromBottom(GetDoubleValues(Table, "ThicknessChem (mm)", NumRows, StartRow));
                else
                    NewSoil.SoilOrganicMatter.Thickness = NewSoil.Water.Thickness;
                int NumChemLayers = NewSoil.SoilOrganicMatter.Thickness.Length;
                NewSoil.SoilOrganicMatter.RootCN = GetDoubleValue(Table, StartRow, "RootCn");
                NewSoil.SoilOrganicMatter.RootWt = GetDoubleValue(Table, StartRow, "RootWt");
                NewSoil.SoilOrganicMatter.SoilCN = GetDoubleValue(Table, StartRow, "SoilCN");
                NewSoil.SoilOrganicMatter.EnrACoeff = GetDoubleValue(Table, StartRow, "EnrACoeff");
                NewSoil.SoilOrganicMatter.EnrBCoeff = GetDoubleValue(Table, StartRow, "EnrBCoeff");
                NewSoil.SoilWater.SWCON = GetDoubleValues(Table, "SWCON (0-1)", NumLayers, StartRow);
                NewSoil.SoilWater.MWCON = GetDoubleValues(Table, "MWCON (0-1)", NumLayers, StartRow);
                NewSoil.SoilOrganicMatter.FBiom = GetDoubleValues(Table, "FBIOM (0-1)", NumChemLayers, StartRow);
                NewSoil.SoilOrganicMatter.FInert = GetDoubleValues(Table, "FINERT (0-1)", NumChemLayers, StartRow);
                NewSoil.Water.KS = GetDoubleValues(Table, "KS (mm/day)", NumLayers, StartRow);
                NewSoil.SoilOrganicMatter.OC = GetDoubleValues(Table, "OC", NumChemLayers, StartRow);
                NewSoil.SoilOrganicMatter.OCMetadata = GetCodeValues(Table, "OCCode", NumChemLayers, StartRow);
                NewSoil.SoilOrganicMatter.OCUnits = GetOCUnits(Table, StartRow);

                NewSoil.Analysis.Thickness = NewSoil.SoilOrganicMatter.Thickness;
                NewSoil.Analysis.EC = GetDoubleValues(Table, "EC (1:5 dS/m)", NumChemLayers, StartRow);
                NewSoil.Analysis.ECMetadata = GetCodeValues(Table, "ECCode", NumChemLayers, StartRow);
                NewSoil.Analysis.PH = GetDoubleValues(Table, "PH", NumChemLayers, StartRow);
                NewSoil.Analysis.PHMetadata = GetCodeValues(Table, "PHCode", NumChemLayers, StartRow);
                NewSoil.Analysis.PHUnits = GetPHUnits(Table, StartRow);
                NewSoil.Analysis.CL = GetDoubleValues(Table, "CL (mg/kg)", NumChemLayers, StartRow);
                NewSoil.Analysis.CLMetadata = GetCodeValues(Table, "CLCode", NumChemLayers, StartRow);
                NewSoil.Analysis.Boron = GetDoubleValues(Table, "Boron (Hot water mg/kg)", NumChemLayers, StartRow);
                NewSoil.Analysis.BoronMetadata = GetCodeValues(Table, "BoronCode", NumChemLayers, StartRow);
                NewSoil.Analysis.CEC = GetDoubleValues(Table, "CEC (cmol+/kg)", NumChemLayers, StartRow);
                NewSoil.Analysis.CECMetadata = GetCodeValues(Table, "CECCode", NumChemLayers, StartRow);
                NewSoil.Analysis.Ca = GetDoubleValues(Table, "Ca (cmol+/kg)", NumChemLayers, StartRow);
                NewSoil.Analysis.CaMetadata = GetCodeValues(Table, "CaCode", NumChemLayers, StartRow);
                NewSoil.Analysis.Mg = GetDoubleValues(Table, "Mg (cmol+/kg)", NumChemLayers, StartRow);
                NewSoil.Analysis.MgMetadata = GetCodeValues(Table, "MgCode", NumChemLayers, StartRow);
                NewSoil.Analysis.Na = GetDoubleValues(Table, "Na (cmol+/kg)", NumChemLayers, StartRow);
                NewSoil.Analysis.NaMetadata = GetCodeValues(Table, "NaCode", NumChemLayers, StartRow);
                NewSoil.Analysis.K = GetDoubleValues(Table, "K (cmol+/kg)", NumChemLayers, StartRow);
                NewSoil.Analysis.KMetadata = GetCodeValues(Table, "KCode", NumChemLayers, StartRow);
                NewSoil.Analysis.ESP = GetDoubleValues(Table, "ESP (%)", NumChemLayers, StartRow);
                NewSoil.Analysis.ESPMetadata = GetCodeValues(Table, "ESPCode", NumChemLayers, StartRow);
                NewSoil.Analysis.Mn = GetDoubleValues(Table, "Mn (mg/kg)", NumChemLayers, StartRow);
                NewSoil.Analysis.MnMetadata = GetCodeValues(Table, "MnCode", NumChemLayers, StartRow);
                NewSoil.Analysis.Al = GetDoubleValues(Table, "Al (cmol+/kg)", NumChemLayers, StartRow);
                NewSoil.Analysis.AlMetadata = GetCodeValues(Table, "AlCode", NumChemLayers, StartRow);
                NewSoil.Analysis.ParticleSizeSand = GetDoubleValues(Table, "ParticleSizeSand (%)", NumChemLayers, StartRow);
                NewSoil.Analysis.ParticleSizeSandMetadata = GetCodeValues(Table, "ParticleSizeSandCode", NumChemLayers, StartRow);
                NewSoil.Analysis.ParticleSizeSilt = GetDoubleValues(Table, "ParticleSizeSilt (%)", NumChemLayers, StartRow);
                NewSoil.Analysis.ParticleSizeSiltMetadata = GetCodeValues(Table, "ParticleSizeSiltCode", NumChemLayers, StartRow);
                NewSoil.Analysis.ParticleSizeClay = GetDoubleValues(Table, "ParticleSizeClay (%)", NumChemLayers, StartRow);
                NewSoil.Analysis.ParticleSizeClayMetadata = GetCodeValues(Table, "ParticleSizeClayCode", NumChemLayers, StartRow);

                // crops
                foreach (DataColumn Col in Table.Columns)
                {
                    if (Col.ColumnName.ToLower().Contains(" ll"))
                    {
                        string[] NameBits = Col.ColumnName.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                        if (NameBits.Length == 3)
                        {
                            string CropName = NameBits[0];
                            SoilCrop NewCrop = new SoilCrop();
                            NewCrop.Name = CropName;
                            NewCrop.Thickness = NewSoil.Water.Thickness;
                            NewCrop.LL = GetDoubleValues(Table, CropName + " ll (mm/mm)", NumLayers, StartRow);
                            NewCrop.LLMetadata = GetCodeValues(Table, CropName + " llCode", NumLayers, StartRow);
                            NewCrop.KL = GetDoubleValues(Table, CropName + " kl (/day)", NumLayers, StartRow);
                            NewCrop.XF = GetDoubleValues(Table, CropName + " xf (0-1)", NumLayers, StartRow);
                            if (MathUtility.ValuesInArray(NewCrop.LL) ||
                                MathUtility.ValuesInArray(NewCrop.KL))
                                NewSoil.Water.Crops.Add(NewCrop);
                        }
                    }

                }


                StartRow += NumRows;

                if (Notifier != null)
                    Notifier.Invoke(Convert.ToInt32(StartRow * 100.0 / Table.Rows.Count));
                counter++;
                if (counter == 50)
                {
                    GC.Collect();
                    counter = 0;
                }
            }

            return Soils.ToArray();
        }


        /// <summary>
        /// Convert a soils XML file to a DataTable. The Soil XML can have multiple soils.
        /// </summary>
        public static DataTable SoilXMLToTable(XmlNode Xml, List<string> XMLPaths = null)
        {
            DataTable Table = new DataTable("SoilData");

            List<XmlNode> SoilNodes = new List<XmlNode>();
            if (XMLPaths == null)
                XmlHelper.FindAllRecursivelyByType(Xml, "soil", ref SoilNodes);
            else
            {
                // Go through all paths and look for <folder> nodes. When found
                // replace them with all nested soils.
                foreach (string Path in XMLPaths)
                {
                    XmlNode Node = XmlHelper.Find(Xml, Path);
                    if (Node == null)
                    { }
                    else if (Node.Name.Equals("soil", StringComparison.CurrentCultureIgnoreCase))
                        SoilNodes.Add(Node);
                    else if (Node.Name.Equals("folder", StringComparison.CurrentCultureIgnoreCase))
                        XmlHelper.FindAllRecursivelyByType(Node, "soil", ref SoilNodes);
                }
            }

            foreach (XmlNode SoilNode in SoilNodes)
            {
                Soil mySoil = Soil.Create(SoilNode.OuterXml);
                SoilToTable(mySoil, Table);
            }

            return Table;
        }

        /// <summary>
        /// Fill the specified table with data from the specified soil.
        /// </summary>
        public static void SoilToTable(Soil Soil, DataTable Table)
        {
            int StartRow = Table.Rows.Count;
            int NumValues = Math.Max(Soil.Water.Thickness.Length, Soil.Analysis.Thickness.Length);

            double[] LayerNo = new double[Soil.Water.Thickness.Length];
            for (int i = 1; i <= Soil.Water.Thickness.Length; i++)
                LayerNo[i - 1] = i;

            SetStringValue(Table, "Name", Soil.Name, StartRow, NumValues);
            SetDoubleValue(Table, "RecordNo", Soil.RecordNumber, StartRow, NumValues);
            SetStringValue(Table, "Country", Soil.Country, StartRow, NumValues);
            SetStringValue(Table, "State", Soil.State, StartRow, NumValues);
            SetStringValue(Table, "Region", Soil.Region, StartRow, NumValues);
            SetStringValue(Table, "NearestTown", Soil.NearestTown, StartRow, NumValues);
            SetStringValue(Table, "Site", Soil.Site, StartRow, NumValues);
            SetStringValue(Table, "APSoilNumber", Soil.ApsoilNumber, StartRow, NumValues);
            SetStringValue(Table, "Soil type texture or other descriptor", Soil.SoilType, StartRow, NumValues);
            SetStringValue(Table, "Local name", Soil.LocalName, StartRow, NumValues);
            SetStringValue(Table, "ASC_Order", Soil.ASCOrder, StartRow, NumValues);
            SetStringValue(Table, "ASC_Sub-order", Soil.ASCSubOrder, StartRow, NumValues);
            SetDoubleValue(Table, "Latitude", Soil.Latitude, StartRow, NumValues);
            SetDoubleValue(Table, "Longitude", Soil.Longitude, StartRow, NumValues);
            SetStringValue(Table, "LocationAccuracy", Soil.LocationAccuracy, StartRow, NumValues);
            SetIntegerValue(Table, "YearOfSampling", Soil.YearOfSampling, StartRow, NumValues);
            SetStringValue(Table, "DataSource", Soil.DataSource, StartRow, NumValues);
            SetStringValue(Table, "Comments", Soil.Comments, StartRow, NumValues);
            SetStringValue(Table, "NaturalVegetation", Soil.NaturalVegetation, StartRow, NumValues);
            SetStringValues(Table, "MunsellColour", Soil.Analysis.MunsellColour, StartRow);
            SetCodeValues(Table, "MunsellColourCode", Soil.Analysis.MunsellMetadata, StartRow);
            SetDoubleValues(Table, "LayerNo", LayerNo, StartRow);
            SetDoubleValues(Table, "Thickness (mm)", Soil.Water.Thickness, StartRow);
            SetDoubleValues(Table, "BD (g/cc)", Soil.Water.BD, StartRow);
            SetCodeValues(Table, "BDCode", Soil.Water.BDMetadata, StartRow);
            SetDoubleValues(Table, "Rocks (%)", Soil.Analysis.Rocks, StartRow);
            SetCodeValues(Table, "RocksCode", Soil.Analysis.RocksMetadata, StartRow);
            SetStringValues(Table, "Texture", Soil.Analysis.Texture, StartRow);
            SetCodeValues(Table, "TextureCode", Soil.Analysis.TextureMetadata, StartRow);
            SetDoubleValues(Table, "SAT (mm/mm)", Soil.Water.SAT, StartRow);
            SetCodeValues(Table, "SATCode", Soil.Water.SATMetadata, StartRow);
            SetDoubleValues(Table, "DUL (mm/mm)", Soil.Water.DUL, StartRow);
            SetCodeValues(Table, "DULCode", Soil.Water.DULMetadata, StartRow);
            SetDoubleValues(Table, "LL15 (mm/mm)", Soil.Water.LL15, StartRow);
            SetCodeValues(Table, "LL15Code", Soil.Water.LL15Metadata, StartRow);
            SetDoubleValues(Table, "Airdry (mm/mm)", Soil.Water.AirDry, StartRow);
            SetCodeValues(Table, "AirdryCode", Soil.Water.AirDryMetadata, StartRow);
            SetCropValues(Table, "wheat", Soil, StartRow);
            SetCropValues(Table, "barley", Soil, StartRow);
            SetCropValues(Table, "oats", Soil, StartRow);
            SetCropValues(Table, "canola", Soil, StartRow); 
            SetCropValues(Table, "chickpea", Soil, StartRow);
            SetCropValues(Table, "cotton", Soil, StartRow);
            SetCropValues(Table, "sorghum", Soil, StartRow);

            SetDoubleValue(Table, "SummerU", Soil.SoilWater.SummerU, StartRow, NumValues);
            SetDoubleValue(Table, "SummerCona", Soil.SoilWater.SummerCona, StartRow, NumValues);
            SetDoubleValue(Table, "WinterU", Soil.SoilWater.WinterU, StartRow, NumValues);
            SetDoubleValue(Table, "WinterCona", Soil.SoilWater.WinterCona, StartRow, NumValues);
            SetStringValue(Table, "SummerDate", "=\"" + Soil.SoilWater.SummerDate + "\"", StartRow, NumValues);
            SetStringValue(Table, "WinterDate", "=\"" + Soil.SoilWater.WinterDate + "\"", StartRow, NumValues);
            SetDoubleValue(Table, "Salb", Soil.SoilWater.Salb, StartRow, NumValues);
            SetDoubleValue(Table, "DiffusConst", Soil.SoilWater.DiffusConst, StartRow, NumValues);
            SetDoubleValue(Table, "DiffusSlope", Soil.SoilWater.DiffusSlope, StartRow, NumValues);
            SetDoubleValue(Table, "CN2Bare", Soil.SoilWater.CN2Bare, StartRow, NumValues);
            SetDoubleValue(Table, "CNRed", Soil.SoilWater.CNRed, StartRow, NumValues);
            SetDoubleValue(Table, "CNCov", Soil.SoilWater.CNCov, StartRow, NumValues);
            SetDoubleValue(Table, "RootCN", Soil.SoilOrganicMatter.RootCN, StartRow, NumValues);
            SetDoubleValue(Table, "RootWT", Soil.SoilOrganicMatter.RootWt, StartRow, NumValues);
            SetDoubleValue(Table, "SoilCN", Soil.SoilOrganicMatter.SoilCN, StartRow, NumValues);
            SetDoubleValue(Table, "EnrACoeff", Soil.SoilOrganicMatter.EnrACoeff, StartRow, NumValues);
            SetDoubleValue(Table, "EnrBCoeff", Soil.SoilOrganicMatter.EnrBCoeff, StartRow, NumValues);
            SetDoubleValues(Table, "SWCON (0-1)", Soil.SoilWater.SWCON, StartRow);
            SetDoubleValues(Table, "MWCON (0-1)", Soil.SoilWater.MWCON, StartRow);
            SetDoubleValues(Table, "FBIOM (0-1)", Soil.SoilOrganicMatter.FBiom, StartRow);
            SetDoubleValues(Table, "FINERT (0-1)", Soil.SoilOrganicMatter.FInert, StartRow);
            SetDoubleValues(Table, "KS (mm/day)", Soil.Water.KS, StartRow);

            SetDoubleValues(Table, "ThicknessChem (mm)", Soil.SoilOrganicMatter.Thickness, StartRow);
            SetDoubleValues(Table, "OC", Soil.SoilOrganicMatter.OC, StartRow);
            SetOCCodeValues(Table, Soil, StartRow);
            SetDoubleValues(Table, "EC (1:5 dS/m)", Soil.Analysis.EC, StartRow);
            SetCodeValues(Table, "ECCode", Soil.Analysis.ECMetadata, StartRow);
            SetDoubleValues(Table, "PH", Soil.Analysis.PH, StartRow);
            SetPHCodeValues(Table, Soil, StartRow);
            SetDoubleValues(Table, "CL (mg/kg)", Soil.Analysis.CL, StartRow);
            SetCodeValues(Table, "CLCode", Soil.Analysis.CLMetadata, StartRow);
            SetDoubleValues(Table, "Boron (Hot water mg/kg)", Soil.Analysis.Boron, StartRow);
            SetCodeValues(Table, "BoronCode", Soil.Analysis.BoronMetadata, StartRow);
            SetDoubleValues(Table, "CEC (cmol+/kg)", Soil.Analysis.CEC, StartRow);
            SetCodeValues(Table, "CECCode", Soil.Analysis.CECMetadata, StartRow);
            SetDoubleValues(Table, "Ca (cmol+/kg)", Soil.Analysis.Ca, StartRow);
            SetCodeValues(Table, "CaCode", Soil.Analysis.CaMetadata, StartRow);
            SetDoubleValues(Table, "Mg (cmol+/kg)", Soil.Analysis.Mg, StartRow);
            SetCodeValues(Table, "MgCode", Soil.Analysis.MgMetadata, StartRow);
            SetDoubleValues(Table, "Na (cmol+/kg)", Soil.Analysis.Na, StartRow);
            SetCodeValues(Table, "NaCode", Soil.Analysis.NaMetadata, StartRow);
            SetDoubleValues(Table, "K (cmol+/kg)", Soil.Analysis.K, StartRow);
            SetCodeValues(Table, "KCode", Soil.Analysis.KMetadata, StartRow);
            SetDoubleValues(Table, "ESP (%)", Soil.Analysis.ESP, StartRow);
            SetCodeValues(Table, "ESPCode", Soil.Analysis.ESPMetadata, StartRow);
            SetDoubleValues(Table, "Mn (mg/kg)", Soil.Analysis.Mn, StartRow);
            SetCodeValues(Table, "MnCode", Soil.Analysis.MnMetadata, StartRow);
            SetDoubleValues(Table, "Al (cmol+/kg)", Soil.Analysis.Al, StartRow);
            SetCodeValues(Table, "AlCode", Soil.Analysis.AlMetadata, StartRow);
            SetDoubleValues(Table, "ParticleSizeSand (%)", Soil.Analysis.ParticleSizeSand, StartRow);
            SetCodeValues(Table, "ParticleSizeSandCode", Soil.Analysis.ParticleSizeSandMetadata, StartRow);
            SetDoubleValues(Table, "ParticleSizeSilt (%)", Soil.Analysis.ParticleSizeSilt, StartRow);
            SetCodeValues(Table, "ParticleSizeSiltCode", Soil.Analysis.ParticleSizeSiltMetadata, StartRow);
            SetDoubleValues(Table, "ParticleSizeClay (%)", Soil.Analysis.ParticleSizeClay, StartRow);
            SetCodeValues(Table, "ParticleSizeClayCode", Soil.Analysis.ParticleSizeClayMetadata, StartRow);

            foreach (string CropName in CropList)
                SetCropValues(Table, CropName, Soil, StartRow);
        }




        #region Private methods

        /// <summary>
        /// Add the specified soil to the AllSoils node.
        /// </summary>
        private static void AddSoilToXML(Soil NewSoil, XmlNode AllSoils)
        {
            string SoilPath = CalcPathFromSoil(NewSoil);
            XmlNode ParentNode;
            if (SoilPath == "")
                ParentNode = AllSoils;
            else
                ParentNode = EnsureNodeExists(AllSoils, SoilPath);

            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(NewSoil.ToXml());
            ParentNode.AppendChild(ParentNode.OwnerDocument.ImportNode(Doc.DocumentElement, true));
        }

        /// <summary>
        /// Calculate a path for the specified soil.
        /// </summary>
        private static string CalcPathFromSoil(Soil NewSoil)
        {
            if (NewSoil.Country == "")
                return "";
            else
            {
                string Path = NewSoil.Country;
                if (NewSoil.State != "")
                    Path += "/" + NewSoil.State;
                else
                    return Path;
                if (NewSoil.Region != "")
                    Path += "/" + NewSoil.Region;
                return Path;
            }
        }

        /// <summary>
        /// Ensure a node exists by creating nodes as necessary
        /// for the specified node path.
        /// </summary>
        private static XmlNode EnsureNodeExists(XmlNode Node, string NodePath)
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

        /// <summary>
        ///  Return the PH units from the specified row on the specified table.
        /// </summary>
        private static Analysis.PHUnitsEnum GetPHUnits(DataTable Table, int Row)
        {
            if (Table.Columns.Contains("PHCode"))
            {
                string Code = GetStringValue(Table, Row, "PHCode");
                string Units = StringManip.SplitOffBracketedValue(ref Code, '(', ')');
                if (Units == "CaCl2")
                    return Analysis.PHUnitsEnum.CaCl2;
            }
            return Analysis.PHUnitsEnum.Water;
        }

        /// <summary>
        /// Return the OC units from the specified row on the specified table.
        /// </summary>
        private static SoilOrganicMatter.OCUnitsEnum GetOCUnits(DataTable Table, int Row)
        {
            if (Table.Columns.Contains("OCCode"))
            {
                string Code = GetStringValue(Table, Row, "OCCode");
                string Units = StringManip.SplitOffBracketedValue(ref Code, '(', ')');
                if (Units == "Walkley Black %")
                    return SoilOrganicMatter.OCUnitsEnum.WalkleyBlack;
            }
            return SoilOrganicMatter.OCUnitsEnum.Total;
        }

        /// <summary>
        /// Return a list of code values for the specified variable.
        /// </summary>
        private static string[] GetCodeValues(DataTable Table, string VariableName, int NumRows, int StartRow)
        {
            if (!Table.Columns.Contains(VariableName))
                return null;

            string[] Codes = DataTableUtility.GetColumnAsStrings(Table, VariableName, NumRows, StartRow);
            for (int i = 0; i != Codes.Length; i++)
                StringManip.SplitOffBracketedValue(ref Codes[i], '(', ')');
            Codes = APSIMChangeTool.CodeToMetaData(Codes);
            if (MathUtility.ValuesInArray(Codes))
                return Codes;
            return null;
        }

        /// <summary>
        /// Return a single double value from the specified table and row.
        /// </summary>
        private static double GetDoubleValue(DataTable Table, int Row, string VariableName)
        {
            if (!Table.Columns.Contains(VariableName) ||
                Table.Rows[Row][VariableName] == DBNull.Value)
                return double.NaN;
            return Convert.ToDouble(Table.Rows[Row][VariableName]);
        }

        /// <summary>
        /// Return a single integer value from the specified table and row.
        /// </summary>
        private static int GetIntegerValue(DataTable Table, int Row, string VariableName)
        {
            if (!Table.Columns.Contains(VariableName) ||
                Table.Rows[Row][VariableName] == DBNull.Value)
                return 0;
            return Convert.ToInt32(Table.Rows[Row][VariableName]);
        }

        /// <summary>
        /// Return a single string value from the specified table and row
        /// </summary>
        private static string GetStringValue(DataTable Table, int Row, string VariableName)
        {
            if (Table.Columns.Contains(VariableName))
                return Table.Rows[Row][VariableName].ToString();
            return null;
        }

        /// <summary>
        /// Return an array of values for the specified column.
        /// </summary>
        private static double[] GetDoubleValues(DataTable Table, string VariableName, int NumRows, int StartRow)
        {
            if (Table.Columns.Contains(VariableName))
            {
                double[] Values = DataTableUtility.GetColumnAsDoubles(Table, VariableName, NumRows, StartRow);
                if (MathUtility.ValuesInArray(Values))
                {
                    // Convert MissingValue for Nan
                    for (int i = 0; i != Values.Length; i++)
                        if (Values[i] == MathUtility.MissingValue)
                            Values[i] = double.NaN;
                    return Values;
                }
            }
            return null;
        }

        /// <summary>
        /// Return an array of values for the specified column.
        /// </summary>
        private static string[] GetStringValues(DataTable Table, string VariableName, int NumRows, int StartRow)
        {
            if (Table.Columns.Contains(VariableName))
            {
                string[] Values = DataTableUtility.GetColumnAsStrings(Table, VariableName, NumRows, StartRow);
                if (MathUtility.ValuesInArray(Values))
                    return Values;
            }
            return null;
        }





        /// <summary>
        /// Set the PHCode column of the specified table.
        /// </summary>
        private static void SetPHCodeValues(DataTable Table, Soil Soil, int StartRow)
        {
            string[] Codes = MetaDataToCode(Soil.Analysis.PHMetadata);
            if (Codes != null)
                for (int i = 0; i < Codes.Length; i++)
                    Codes[i] += " (" + Soil.Analysis.PHUnitsToString(Soil.Analysis.PHUnits) + ")";
            SetStringValues(Table, "PHCode", Codes, StartRow);
        }

        /// <summary>
        /// Set the OCCode column of the specified table.
        /// </summary>
        private static void SetOCCodeValues(DataTable Table, Soil Soil, int StartRow)
        {
            string[] Codes = MetaDataToCode(Soil.SoilOrganicMatter.OCMetadata);
            if (Codes != null)
                for (int i = 0; i < Codes.Length; i++)
                    Codes[i] += " (" + Soil.SoilOrganicMatter.OCUnitsToString(Soil.SoilOrganicMatter.OCUnits) + ")";
            SetStringValues(Table, "OCCode", Codes, StartRow);
        }

        /// <summary>
        /// Set a column of metadata values for the specified column.
        /// </summary>
        private static void SetCodeValues(DataTable Table, string ColumnName, string[] Metadata, int StartRow)
        {
            string[] Codes = MetaDataToCode(Metadata);
            SetStringValues(Table, ColumnName, Codes, StartRow);
        }

        /// <summary>
        /// Set the crop values in the table for the specified crop name.
        /// </summary>
        private static void SetCropValues(DataTable Table, string CropName, Soil Soil, int StartRow)
        {
            if (Soil.CropNames.Contains(CropName, StringComparer.CurrentCultureIgnoreCase))
            {
                SetDoubleValues(Table, CropName + " ll (mm/mm)", Soil.Crop(CropName).LL, StartRow);
                SetCodeValues(Table, CropName + " llCode", Soil.Crop(CropName).LLMetadata, StartRow);
                SetDoubleValues(Table, CropName + " kl (/day)", Soil.Crop(CropName).KL, StartRow);
                SetDoubleValues(Table, CropName + " xf (0-1)", Soil.Crop(CropName).XF, StartRow);
            }
            else if (!Table.Columns.Contains(CropName + " ll (mm/mm)"))
            {
                Table.Columns.Add(CropName + " ll (mm/mm)", typeof(double));
                Table.Columns.Add(CropName + " llCode", typeof(string));
                Table.Columns.Add(CropName + " kl (/day)", typeof(double));
                Table.Columns.Add(CropName + " xf (0-1)", typeof(double));
            }
        }

        /// <summary>
        /// Set a column of double values in the specified table.
        /// </summary>
        private static void SetDoubleValues(DataTable Table, string ColumnName, double[] Values, int StartRow)
        {
            if (MathUtility.ValuesInArray(Values))
                DataTableUtility.AddColumn(Table, ColumnName, Values, StartRow, Values.Length);
            else if (!Table.Columns.Contains(ColumnName))
                Table.Columns.Add(ColumnName, typeof(double));
        }

        /// <summary>
        /// Set a column of string values in the specified table.
        /// </summary>
        private static void SetStringValues(DataTable Table, string ColumnName, string[] Values, int StartRow)
        {
            if (MathUtility.ValuesInArray(Values))
                DataTableUtility.AddColumn(Table, ColumnName, Values, StartRow, Values.Length);
            else if (!Table.Columns.Contains(ColumnName))
                Table.Columns.Add(ColumnName, typeof(string));
        }

        /// <summary>
        /// Set a column to the specified Value a specificed numebr of times.
        /// </summary>
        private static void SetDoubleValue(DataTable Table, string ColumnName, double Value, int StartRow, int NumValues)
        {
            double[] Values = new double[NumValues];
            for (int i = 0; i < NumValues; i++)
                Values[i] = Value;
            SetDoubleValues(Table, ColumnName, Values, StartRow);
        }

        /// <summary>
        /// Set a column to the specified Value a specificed numebr of times.
        /// </summary>
        private static void SetIntegerValue(DataTable Table, string ColumnName, int Value, int StartRow, int NumValues)
        {
            double[] Values = new double[NumValues];
            for (int i = 0; i < NumValues; i++)
                Values[i] = Value;
            SetDoubleValues(Table, ColumnName, Values, StartRow);
        }

        /// <summary>
        /// Set a column to the specified Value a specificed numebr of times.
        /// </summary>
        private static void SetStringValue(DataTable Table, string ColumnName, string Value, int StartRow, int NumValues)
        {
            string[] Values = StringManip.CreateStringArray(Value, NumValues);
            SetStringValues(Table, ColumnName, Values, StartRow);
        }

        /// <summary>
        /// Convert a metadata into an abreviated code.
        /// </summary>
        static public string[] MetaDataToCode(string[] Metadata)
        {
            if (Metadata == null)
                return null;

            string[] Codes = new string[Metadata.Length];
            for (int i = 0; i < Metadata.Length; i++)
                if (Metadata[i] == "Field measured and checked for sensibility")
                    Codes[i] = "FM";
                else if (Metadata[i] == "Calculated from gravimetric moisture when profile wet but drained")
                    Codes[i] = "C_grav";
                else if (Metadata[i] == "Estimated based on local knowledge")
                    Codes[i] = "E";
                else if (Metadata[i] == "Unknown source or quality of data")
                    Codes[i] = "U";
                else if (Metadata[i] == "Laboratory measured")
                    Codes[i] = "LM";
                else if (Metadata[i] == "Volumetric measurement")
                    Codes[i] = "V";
                else if (Metadata[i] == "Measured")
                    Codes[i] = "M";
                else if (Metadata[i] == "Calculated from measured, estimated or calculated BD")
                    Codes[i] = "C_bd";
                else if (Metadata[i] == "Developed using a pedo-transfer function")
                    Codes[i] = "C_pt";
                else
                    Codes[i] = Metadata[i];
            return Codes;
        }
        #endregion

    }
}
