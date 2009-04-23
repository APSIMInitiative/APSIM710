
using System;
using System.Collections;
using System.Collections.Specialized;
using System.Data;
using System.IO;
using System.Windows.Forms;
using System.Xml;

using ApsimFile;
using Controllers;
using CSGeneral;
using ExcelUtility;


namespace UIUtility
	{
	public class SoilSpreadsheet
		{
		public SoilSpreadsheet()
			{
			}


        static public void ImportFromFile(string FileName, BaseController Controller)
			{
			// Import all files
			Cursor.Current = Cursors.WaitCursor;

			DataTable Table = ExcelHelper.GetDataFromSheet(FileName, "SoilData");
            StringCollection NewXml = new StringCollection();
			int Row = 0;
            while (Row < Table.Rows.Count)
                NewXml.Add(CreateSoilXmlFromSpreadsheet(Table, ref Row));

            XmlDocument Doc = new XmlDocument();
            XmlNode AllNewData = Doc.CreateElement("dummy");
            foreach (string Xml in NewXml)
                {
                XmlDocument TempDoc = new XmlDocument();
                TempDoc.LoadXml(Xml);
                XmlNode NewData = TempDoc.DocumentElement;
                XmlNode NewNode = CreateNodeFromPath(Controller, AllNewData, XmlHelper.Name(NewData));
                NewNode.InnerXml = NewData.InnerXml;
				}
            Controller.Selection.Add(AllNewData.InnerXml);
			Cursor.Current = Cursors.Default;
			}


        static public void ExportSelectedToFile(string FileName, BaseController Controller)
			{
            Cursor.Current = Cursors.WaitCursor;
            
            File.Delete(FileName);
            DataTable Table = new DataTable("SoilData");
            int Row = 0;
            foreach (string SelectedPath in Controller.SelectedPaths)
                {
                ApsimFile.Component SoilComponent = Controller.ApsimData.Find(SelectedPath);
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(SoilComponent.FullXML());
                CreateTableFromData(Doc.DocumentElement, Table, SelectedPath.Replace("/", "\\"), ref Row);
                }
            ExcelHelper.SendDataToSheet(FileName, "SoilData", Table);
            Cursor.Current = Cursors.Default;
            }

		static private void CreateTableFromData(XmlNode Data, DataTable Table, string ChildPath, ref int Row)
			{
            if (XmlHelper.Type(Data).ToLower() == "soil")
                CreateTableFromSoil(new Soil(Data), Table, ChildPath, ref Row);

			foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
				{
				if (XmlHelper.Type(Child).ToLower() == "soil" ||
				    XmlHelper.Type(Child).ToLower() == "soils" || XmlHelper.Type(Child).ToLower() == "folder")
					CreateTableFromData(Child, Table, ChildPath + "\\" + XmlHelper.Name(Child), ref Row); // recursion
				}
			}


		static private void CreateTableFromSoil(Soil MySoil, DataTable Data, string ChildPath, ref int Row)
			{
			int NumLayers = MySoil.Thickness.Length;
			DataTableUtility.AddValue(Data, "Name", ChildPath, Row, NumLayers);
			DataTableUtility.AddValue(Data, "State", MySoil.State, Row, NumLayers);
            DataTableUtility.AddValue(Data, "Region", MySoil.Region, Row, NumLayers);
            DataTableUtility.AddValue(Data, "NearestTown", MySoil.NearestTown, Row, NumLayers);
            DataTableUtility.AddValue(Data, "Site", MySoil.Site, Row, NumLayers);
            DataTableUtility.AddValue(Data, "APSoilNumber", MySoil.ApsoilNumber, Row, NumLayers);
            DataTableUtility.AddValue(Data, "Classification", MySoil.Classification, Row, NumLayers);
            DataTableUtility.AddValue(Data, "Latitude(WGS84)", MySoil.Latitude, Row, NumLayers);
            DataTableUtility.AddValue(Data, "Longitude(WGS84)", MySoil.Longitude, Row, NumLayers);
            DataTableUtility.AddValue(Data, "LocationAccuracy", MySoil.LocationAccuracy, Row, NumLayers);
            DataTableUtility.AddValue(Data, "DataSource", MySoil.DataSource, Row, NumLayers);
			DataTableUtility.AddValue(Data, "Comments", MySoil.Comment, Row, NumLayers);
			DataTableUtility.AddValue(Data, "NaturalVegetation", MySoil.NaturalVegetation, Row, NumLayers);

            DataTableUtility.AddColumn(Data, "Thickness", MySoil.Thickness, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "BD", MySoil.BD, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "Rocks", MySoil.Rocks, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "Texture", MySoil.Texture, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "SAT", MySoil.SAT, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "DUL", MySoil.DUL, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "LL15", MySoil.LL15, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "Airdry", MySoil.Airdry, Row, NumLayers);

            string[] CropOrder = {"wheat", "barley", "oats", "canola", "chickpea", "cotton", 
                                  "sorghum", "lentil", "lupin", "fieldpea", "mungbean", "sunflower", 
                                  "fababean", "lucerne", "maize", "perennial grass", "cowpea", "navybean", 
                                  "peanut", "pigeonpea", "soybean", "stylo", "sugar", "lablab", 
                                  "millet", "triticale", "weed", "medic"};
            for (int i = 0; i != 7; i++)
                AddCropColumn(Data, CropOrder[i], MySoil, Row, NumLayers);

            DataTableUtility.AddValue(Data, "SummerU", MySoil.SummerU, Row, NumLayers);
            DataTableUtility.AddValue(Data, "SummerCona", MySoil.SummerCona, Row, NumLayers);
            DataTableUtility.AddValue(Data, "WinterU", MySoil.WinterU, Row, NumLayers);
            DataTableUtility.AddValue(Data, "WinterCona", MySoil.WinterCona, Row, NumLayers);
            DataTableUtility.AddValue(Data, "SummerDate", "=\"" + MySoil.SummerDate + "\"", Row, NumLayers);
            DataTableUtility.AddValue(Data, "WinterDate", "=\"" + MySoil.WinterDate + "\"", Row, NumLayers);
            DataTableUtility.AddValue(Data, "U", MySoil.U, Row, NumLayers);
            DataTableUtility.AddValue(Data, "Cona", MySoil.Cona, Row, NumLayers);
            DataTableUtility.AddValue(Data, "Salb", MySoil.Salb, Row, NumLayers);
            DataTableUtility.AddValue(Data, "DiffusConst", MySoil.DiffusConst, Row, NumLayers);
            DataTableUtility.AddValue(Data, "DiffusSlope", MySoil.DiffusSlope, Row, NumLayers);
            DataTableUtility.AddValue(Data, "CN2Bare", MySoil.CN2Bare, Row, NumLayers);
            DataTableUtility.AddValue(Data, "CNRed", MySoil.CNRed, Row, NumLayers);
            DataTableUtility.AddValue(Data, "CNCov", MySoil.CNCov, Row, NumLayers);
            DataTableUtility.AddValue(Data, "RootCN", MySoil.RootCN, Row, NumLayers);
            DataTableUtility.AddValue(Data, "RootWT", MySoil.RootWT, Row, NumLayers);
            DataTableUtility.AddValue(Data, "SoilCN", MySoil.SoilCN, Row, NumLayers);
            DataTableUtility.AddValue(Data, "EnrACoeff", MySoil.EnrACoeff, Row, NumLayers);
            DataTableUtility.AddValue(Data, "EnrBCoeff", MySoil.EnrBCoeff, Row, NumLayers);

            DataTableUtility.AddColumn(Data, "SWCON", MySoil.SWCON, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "MWCON", MySoil.MWCON, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "FBIOM", MySoil.FBIOM, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "FINERT", MySoil.FINERT, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "KS", MySoil.KS, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "OC", MySoil.OC, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "EC", MySoil.EC, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "PH", MySoil.PH, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "CL", MySoil.CL, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "Boron", MySoil.Boron, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "CEC", MySoil.CEC, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "Ca", MySoil.Ca, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "Mg", MySoil.Mg, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "Na", MySoil.Na, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "K", MySoil.K, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "ESP", MySoil.ESP, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "Mn", MySoil.Mn, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "Al", MySoil.Al, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "ParticleSizeSand", MySoil.ParticleSizeSand, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "ParticleSizeSilt", MySoil.ParticleSizeSilt, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "ParticleSizeClay", MySoil.ParticleSizeClay, Row, NumLayers);

            for (int i = 7; i != CropOrder.Length; i++)
                AddCropColumn(Data, CropOrder[i], MySoil, Row, NumLayers);

            // Add other crops not in our list above.
            foreach (string CropName in MySoil.CropsMeasured)
                {
                if (Array.IndexOf(CropOrder, CropName.ToLower()) == -1)
                    AddCropColumn(Data, CropName, MySoil, Row, NumLayers);
                }

			Row += NumLayers;
			}

        private static void AddCropColumn(DataTable Data, string CropName, Soil MySoil, int Row, int NumLayers)
            {
            if (MySoil.CropExists(CropName))
                {
                DataTableUtility.AddColumn(Data, "ll(" + CropName + ")", MySoil.LL(CropName), Row, NumLayers);
                DataTableUtility.AddColumn(Data, "kl(" + CropName + ")", MySoil.KL(CropName), Row, NumLayers);
                DataTableUtility.AddColumn(Data, "xf(" + CropName + ")", MySoil.XF(CropName), Row, NumLayers);
                }
            else
                {
                DataTableUtility.AddValue(Data, "ll(" + CropName + ")", MathUtility.MissingValue, Row, NumLayers);
                DataTableUtility.AddValue(Data, "kl(" + CropName + ")", MathUtility.MissingValue, Row, NumLayers);
                DataTableUtility.AddValue(Data, "xf(" + CropName + ")", MathUtility.MissingValue, Row, NumLayers);
                }
            }


		static private string CreateSoilXmlFromSpreadsheet(DataTable Table, ref int Row)
			{
			// Create a new soil from the the specified table.
			// At end of this method, row will be pointing to the next
			// soil.
			string name = GetStringValue(Table, "Name", Row);
			if (name == "")
				throw new Exception("Cannot find a soil name");

            XmlDocument Doc = new XmlDocument();
            XmlNode SoilNode = Doc.CreateElement("Soil");
            XmlHelper.SetName(SoilNode, name);
			Soil NewSoil = new Soil(SoilNode);
            NewSoil.State = GetStringValue(Table, "State", Row);
            NewSoil.Region = GetStringValue(Table, "Region", Row);
            NewSoil.NearestTown = GetStringValue(Table, "NearestTown", Row);
            NewSoil.Site = GetStringValue(Table, "Site", Row);
            NewSoil.ApsoilNumber = GetStringValue(Table, "APSoilNumber", Row);
            NewSoil.Classification = GetStringValue(Table, "Classification", Row);
            NewSoil.Latitude = MathUtility.Round(GetDoubleValue(Table, "Latitude(WGS84)", Row), 3);
            NewSoil.Longitude = MathUtility.Round(GetDoubleValue(Table, "Longitude(WGS84)", Row), 3);
            NewSoil.LocationAccuracy = GetStringValue(Table, "LocationAccuracy", Row);
            NewSoil.DataSource = GetStringValue(Table, "DataSource", Row);
			NewSoil.Comment = GetStringValue(Table, "Comments", Row);
			NewSoil.NaturalVegetation = GetStringValue(Table, "NaturalVegetation", Row);

            double SummerU = GetDoubleValue(Table, "SummerU", Row);
            if (SummerU != MathUtility.MissingValue)
                NewSoil.SetSummerWinterUCona(GetDoubleValue(Table, "SummerU", Row), GetDoubleValue(Table, "WinterU", Row),
                                             GetDoubleValue(Table, "SummerCona", Row), GetDoubleValue(Table, "WinterCona", Row),
                                             GetStringValue(Table, "SummerDate", Row), GetStringValue(Table, "WinterDate", Row));
            else
			    NewSoil.SetUCona(GetDoubleValue(Table, "U", Row), GetDoubleValue(Table, "Cona", Row));

			NewSoil.Salb = GetDoubleValue(Table, "Salb", Row);
			NewSoil.DiffusConst = GetDoubleValue(Table, "DiffusConst", Row);
			NewSoil.DiffusSlope = GetDoubleValue(Table, "DiffusSlope", Row);
			NewSoil.CN2Bare = GetDoubleValue(Table, "CN2Bare", Row);
			NewSoil.CNRed = GetDoubleValue(Table, "CNRed", Row);
			NewSoil.CNCov = GetDoubleValue(Table, "CNCov", Row);
			NewSoil.RootCN = GetDoubleValue(Table, "RootCN", Row);
			NewSoil.RootWT = GetDoubleValue(Table, "RootWT", Row);
			NewSoil.SoilCN = GetDoubleValue(Table, "SoilCN", Row);
			NewSoil.EnrACoeff = GetDoubleValue(Table, "EnrACoeff", Row);
			NewSoil.EnrBCoeff = GetDoubleValue(Table, "EnrBCoeff", Row);

			// Work out how many layers we're dealing with.
			int NumLayers = 0;
			while (Row+NumLayers < Table.Rows.Count &&
				   name.ToLower() == Table.Rows[Row+NumLayers]["Name"].ToString().ToLower())
				NumLayers++;

			// Store thickness.
            NewSoil.Thickness = GetDoubleValues(Table, "Thickness", NumLayers, Row, 0);
			
			// Store rest of soil layered variables.
			NewSoil.BD = GetDoubleValues(Table, "bd", NumLayers, Row, 3);
            NewSoil.Rocks = GetDoubleValues(Table, "Rocks", NumLayers, Row, 0);
            NewSoil.LL15 = GetDoubleValues(Table, "LL15", NumLayers, Row, 3);
			NewSoil.Airdry = GetDoubleValues(Table, "Airdry", NumLayers, Row, 3);
			NewSoil.DUL = GetDoubleValues(Table, "DUL", NumLayers, Row, 3);
			NewSoil.SAT = GetDoubleValues(Table, "SAT", NumLayers, Row, 3);
            NewSoil.Texture = GetStringValues(Table, "Texture", NumLayers, Row);
            NewSoil.SWCON = GetDoubleValues(Table, "SWCON", NumLayers, Row, 2);
			NewSoil.MWCON = GetDoubleValues(Table, "MWCON", NumLayers, Row, 2);
			NewSoil.FBIOM = GetDoubleValues(Table, "FBIOM", NumLayers, Row, 2);
			NewSoil.FINERT = GetDoubleValues(Table, "FINERT", NumLayers, Row, 1);
            NewSoil.KS = GetDoubleValues(Table, "KS", NumLayers, Row, 2);
            NewSoil.OC = GetDoubleValues(Table, "OC", NumLayers, Row, 2);
			NewSoil.EC = GetDoubleValues(Table, "EC", NumLayers, Row, 1);
			NewSoil.PH = GetDoubleValues(Table, "PH", NumLayers, Row, 1);
			NewSoil.CL = GetDoubleValues(Table, "CL", NumLayers, Row, 1);
			NewSoil.Boron = GetDoubleValues(Table, "Boron", NumLayers, Row, 1);
            NewSoil.CEC = GetDoubleValues(Table, "CEC", NumLayers, Row, 1);
            NewSoil.Ca = GetDoubleValues(Table, "Ca", NumLayers, Row, 1);
            NewSoil.Mg = GetDoubleValues(Table, "Mg", NumLayers, Row, 1);
            NewSoil.Na = GetDoubleValues(Table, "Na", NumLayers, Row, 1);
            NewSoil.K = GetDoubleValues(Table, "K", NumLayers, Row, 1);
            NewSoil.ESP = GetDoubleValues(Table, "ESP", NumLayers, Row, 1);
            NewSoil.Mn = GetDoubleValues(Table, "Mn", NumLayers, Row, 1);
            NewSoil.Al = GetDoubleValues(Table, "Al", NumLayers, Row, 1);
            NewSoil.ParticleSizeSand = GetDoubleValues(Table, "ParticleSizeSand", NumLayers, Row, 1);
            NewSoil.ParticleSizeSilt = GetDoubleValues(Table, "ParticleSizeSilt", NumLayers, Row, 1);
            NewSoil.ParticleSizeClay = GetDoubleValues(Table, "ParticleSizeClay", NumLayers, Row, 1);
            //dph NewSoil.InitialNitrogen.NO3 = GetDoubleValues(Table, "NO3", NumLayers, Row, 3);
            //dph NewSoil.InitialNitrogen.NH4 = GetDoubleValues(Table, "NH4", NumLayers, Row, 3);

			// Now get a list of all crop names.
			StringCollection Crops = new StringCollection();
			for (int i = 0; i != Table.Columns.Count; i++)
				{
				string ColumnName = Table.Columns[i].ColumnName;
				if (ColumnName.Length > 2 && ColumnName.Substring(0, 3).ToLower() == "ll(")
					Crops.Add(StringManip.SplitOffBracketedValue(ref ColumnName, '(', ')'));
				}													 

			// Now import all crop stuff.
			for (int i = 0; i != Crops.Count; i++)
				{
				double[] ll = GetDoubleValues(Table, "LL(" + Crops[i] + ")", NumLayers, Row, 3);
				double[] kl = GetDoubleValues(Table, "KL(" + Crops[i] + ")", NumLayers, Row, 3);
				double[] xf = GetDoubleValues(Table, "XF(" + Crops[i] + ")", NumLayers, Row, 2);

				bool AllMissingValues = true;
				for (int j = 0; j != ll.Length && AllMissingValues; j++)
					AllMissingValues = (AllMissingValues && ll[j] == MathUtility.MissingValue &&
										kl[j] == MathUtility.MissingValue && xf[j] == MathUtility.MissingValue);	

				if (!AllMissingValues)
					{
					NewSoil.AddCrop(Crops[i]);
    				NewSoil.SetCrop(Crops[i], ll, kl, xf);
					}
				}

			Row += NumLayers;
			return NewSoil.Data.OuterXml;
			}

		static private string GetStringValue(DataTable Table, string FieldName, int Row)
			{
			// Get string value from specified table for specified field.
			if (Table.Columns.IndexOf(FieldName) != -1 && Table.Rows[Row][FieldName].ToString() != MathUtility.MissingValue.ToString())
				return Table.Rows[Row][FieldName].ToString();
			else
				return "";
			}

        static private string[] GetStringValues(DataTable Table, string FieldName, int NumValues, int Row)
            {
            // Get string value from specified table for specified field.
            if (Table.Columns.IndexOf(FieldName) != -1)
                return DataTableUtility.GetColumnAsStrings(Table, FieldName, NumValues, Row);
            else
                return new string[0];
            }
		static private double GetDoubleValue(DataTable Table, string FieldName, int Row)
			{
			// Get string value from specified table for specified field.
			string Value = GetStringValue(Table, FieldName, Row);
			if (Value == "")
				return MathUtility.MissingValue;
			else
				return Convert.ToDouble(Value);
			}

        static private double[] GetDoubleValues(DataTable Table, string FieldName, int NumValues, int Row, int DecPlaces)
			{
			// Get string value from specified table for specified field.
			if (Table.Columns.IndexOf(FieldName) != -1)
                return MathUtility.Round(DataTableUtility.GetColumnAsDoubles(Table, FieldName, NumValues, Row), DecPlaces);
			else
				return new double[0];
			}

        static private XmlNode CreateNodeFromPath(BaseController Apsoil, XmlNode NewData, string SoilPath)
            // ------------------------------------------------------
            // The SoilPath name passed in is a full path to a node
            // in the tree. e.g. \soils\Queensland\soilA
            // This method returns that node if it exists or creates
            // a new node and returns that.
            // ------------------------------------------------------
			{
			bool rootNode = true;
			int PosDelimiter;
            if (SoilPath != "")
                SoilPath = SoilPath.Substring(1);  // skip leading \ character.

			while (SoilPath != "")
				{
				PosDelimiter = SoilPath.IndexOf('\\');
				string PathNodeName;
				if (PosDelimiter >= 0)
					{
					PathNodeName = SoilPath.Substring(0, PosDelimiter);
					SoilPath = SoilPath.Substring(PosDelimiter+1);
					}
				else
					{
					PathNodeName = SoilPath.Substring(PosDelimiter+1);
					SoilPath = "";
					}

				if (rootNode)
					{
                    //Apsoil.Data.Name = PathNodeName;
					rootNode = false;
					}
				else
					{
					XmlNode Child = XmlHelper.Find(NewData, PathNodeName);
                    if (Child == null)
                        {
                        if (SoilPath == "")
                            Child = NewData.AppendChild(XmlHelper.CreateNode(NewData.OwnerDocument, "soil", PathNodeName));
                        else
                            Child = NewData.AppendChild(XmlHelper.CreateNode(NewData.OwnerDocument, "folder", PathNodeName));
                        }
					NewData = Child;
					}
				}
			return NewData;
			}

		}
	}
