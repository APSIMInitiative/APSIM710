using NUnit.Framework;
using ApsimFile;
using System.Xml;
using CSGeneral;
using System.Data;
using System.IO;
using System.Collections.Specialized;
using System.Collections.Generic;
using System;

namespace Test
   {
   [TestFixture]
   public class TestSoil
      {
      private XmlNode S;
      private const string SoilContents =
            "<soil name=\"Test Soil\">" +
            "  <Country />" +
            "  <Site>My site</Site>" +
            "  <Region>My region</Region>" +
            "  <LocalName></LocalName>" +
            "  <SoilType></SoilType>" +
            "  <NearestTown></NearestTown>" +
            "  <NaturalVegetation></NaturalVegetation>" +
            "  <State />" +
            "  <ApsoilNumber/>" +
            "  <Latitude/>" +
            "  <InitWater>" +
            "    <percentmethod>" +
            "      <percent>1</percent>" +
            "      <distributed>filled from top</distributed>" +
            "    </percentmethod>" +
            "  </InitWater>" +
            "  <Water>" +
            "    <Layer>" +
            "      <Thickness units=\"mm\">150</Thickness>" +
            "      <BD units=\"g/cc\">1.02</BD>" +
            "      <Rocks units=\"%\">" +
            "      </Rocks>" +
            "      <AirDry units=\"mm/mm\">0.15</AirDry>" +
            "      <LL15 units=\"mm/mm\">0.29</LL15>" +
            "      <DUL units=\"mm/mm\">0.54</DUL>" +
            "      <SAT units=\"mm/mm\">0.59</SAT>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>150</Thickness>" +
            "      <BD>1.03</BD>" +
            "      <Rocks>" +
            "      </Rocks>" +
            "      <AirDry>0.26</AirDry>" +
            "      <LL15>0.29</LL15>" +
            "      <DUL>0.53</DUL>" +
            "      <SAT>0.58</SAT>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>300</Thickness>" +
            "      <BD>1.02</BD>" +
            "      <Rocks>" +
            "      </Rocks>" +
            "      <AirDry>0.29</AirDry>" +
            "      <LL15>0.29</LL15>" +
            "      <DUL>0.54</DUL>" +
            "      <SAT>0.59</SAT>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>300</Thickness>" +
            "      <BD>1.02</BD>" +
            "      <Rocks>" +
            "      </Rocks>" +
            "      <AirDry>0.29</AirDry>" +
            "      <LL15>0.29</LL15>" +
            "      <DUL>0.54</DUL>" +
            "      <SAT>0.58</SAT>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>300</Thickness>" +
            "      <BD>1.06</BD>" +
            "      <Rocks>" +
            "      </Rocks>" +
            "      <AirDry>0.3</AirDry>" +
            "      <LL15>0.3</LL15>" +
            "      <DUL>0.52</DUL>" +
            "      <SAT>0.57</SAT>" +
            "    </Layer>" +
            "    <SoilCrop name=\"Barley\">" +
            "      <Layer>" +
            "        <Thickness units=\"mm\">150</Thickness>" +
            "        <LL units=\"mm/mm\" code=\"Field measured\">0.29</LL>" +
            "        <KL units=\"/day\">0.1</KL>" +
            "        <XF units=\"0-1\">1</XF>" +
            "      </Layer>" +
            "      <Layer>" +
            "        <Thickness>150</Thickness>" +
            "        <LL code=\"Laboratory measured\">0.28</LL>" +
            "        <KL>0.1</KL>" +
            "        <XF>1</XF>" +
            "      </Layer>" +
            "    </SoilCrop>" +
            "    <SoilCrop name=\"Chickpea\">" +
            "      <Layer>" +
            "        <Thickness units=\"mm\">100</Thickness>" +
            "        <LL units=\"mm/mm\">0.29</LL>" +
            "        <KL units=\"/day\">0.1</KL>" +
            "        <XF units=\"0-1\">1</XF>" +
            "      </Layer>" +
            "      <Layer>" +
            "        <Thickness>100</Thickness>" +
            "        <LL>0.29</LL>" +
            "        <KL>0.1</KL>" +
            "        <XF>1</XF>" +
            "      </Layer>" +
            "      <Layer>" +
            "        <Thickness>100</Thickness>" +
            "        <LL>0.36</LL>" +
            "        <KL>0.08</KL>" +
            "        <XF>1</XF>" +
            "      </Layer>" +
            "      <Layer>" +
            "        <Thickness>100</Thickness>" +
            "        <LL>0.43</LL>" +
            "        <KL>0.06</KL>" +
            "        <XF>1</XF>" +
            "      </Layer>" +
            "      <Layer>" +
            "        <Thickness>100</Thickness>" +
            "        <LL>0.51</LL>" +
            "        <KL>0.04</KL>" +
            "        <XF>1</XF>" +
            "      </Layer>" +
            "      <Layer>" +
            "        <Thickness>100</Thickness>" +
            "        <LL>0.5</LL>" +
            "        <KL>0.02</KL>" +
            "        <XF>1</XF>" +
            "      </Layer>" +
            "      <Layer>" +
            "        <Thickness>100</Thickness>" +
            "        <LL>0.5</LL>" +
            "        <KL>0.01</KL>" +
            "        <XF>1</XF>" +
            "      </Layer>" +
            "      <Layer>" +
            "        <Thickness>100</Thickness>" +
            "        <LL>0.48</LL>" +
            "        <KL>0</KL>" +
            "        <XF>0</XF>" +
            "      </Layer>" +
            "    </SoilCrop>" +
            "  </Water>" +
            "  <SoilWat>" +
            "    <Cona>3.5</Cona>" +
            "    <U>6</U>" +
            "    <DiffusConst>40</DiffusConst>" +
            "    <DiffusSlope>16</DiffusSlope>" +
            "    <Salb>0.13</Salb>" +
            "    <Cn2Bare>73</Cn2Bare>" +
            "    <CnRed>20</CnRed>" +
            "    <CnCov>0.8</CnCov>" +
            "    <CnCanopyFact>1</CnCanopyFact>" +
            "    <RootCn>40</RootCn>" +
            "    <RootWt>200</RootWt>" +
            "    <SoilCn>12.5</SoilCn>" +
            "    <EnrACoeff>7.4</EnrACoeff>" +
            "    <EnrBCoeff>0.2</EnrBCoeff>" +
            "    <Layer>" +
            "      <Thickness units=\"mm\">100</Thickness>" +
            "      <SWCON units=\"0-1\">0.3</SWCON>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>100</Thickness>" +
            "      <SWCON>0.3</SWCON>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>200</Thickness>" +
            "      <SWCON>0.3</SWCON>" +
            "    </Layer>" +
            "  </SoilWat>" +
            "  <SoilOrganicMatter>" +
            "    <Layer>" +
            "      <Thickness units=\"mm\">200</Thickness>" +
            "      <OC units=\"Walkley Black %\">1.04</OC>" +
            "      <FBiom units=\"0-1\">0.025</FBiom>" +
            "      <FInert units=\"0-1\">0.4</FInert>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>200</Thickness>" +
            "      <OC>0.89</OC>" +
            "      <FBiom>0.02</FBiom>" +
            "      <FInert>0.6</FInert>" +
            "    </Layer>" +
            "  </SoilOrganicMatter>" +
            "  <Lab>" +
            "    <Layer>" +
            "      <Thickness units=\"mm\">100</Thickness>" +
            "      <PH units=\"1:5 water\">8.4</PH>" +
            "      <EC units=\"1:5 dS/m\">0.2</EC>" +
            "    </Layer>" +
            "  </Lab>" +
            "  <Sample name=\"Initial nitrogen\">" +
            "    <Date type=\"date\" description=\"Sample date:\" >1/6/2008</Date>" +
            "    <Layer>" +
            "      <Thickness units=\"mm\">150</Thickness>" +
            "      <NO3 units=\"ppm\">6.50300344798777</NO3>" +
            "      <NH4 units=\"ppm\">0.599004378686979</NH4>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>150</Thickness>" +
            "      <NO3>2.10100111398159</NO3>" +
            "      <NH4>0.100000730999496</NH4>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>300</Thickness>" +
            "      <NO3>2.10100111398159</NO3>" +
            "      <NH4>0.100000730999496</NH4>" +
            "    </Layer>" +
            "  </Sample>" +
            "</soil>";

      /// <summary>
      /// Initialise the test suite.
      /// </summary>
      [SetUp]
      public void Init()
         {
         S = Soil.CreateFromXML(SoilContents);
         }


      /// <summary>
      /// Make sure a soil can process a string macro replacement. Used for 
      /// writing .sim and .par files.
      /// </summary>
      [Test]
      public void MacroReplace()
         {
         string Str = "     Name = [soil.Name]" +
                      "  CN2Bare = [soil.CN2Bare]" +
                      "Thickness = [soil.Thickness (mm)]" +
                      "       BD = [soil.BD (g/cc)]" +
                      "    SWCon = [soil.SWCON(0-1)]" +
                      "      NO3 = [soil.NO3(ppm)]" +
                      "      NH4 = [soil.NH4(ppm)]" +
                      "Barley LL = [soil.barley ll(mm/mm)]";
         string NewStr = Soil.ReplaceSoilMacros(S, Str);
         Assert.AreEqual(NewStr, "     Name = Test Soil" +
                                 "  CN2Bare = 73" +
                                 "Thickness =    150.000   150.000   300.000   300.000   300.000" +
                                 "       BD =      1.020     1.030     1.020     1.020     1.060" +
                                 "    SWCon =      0.300     0.300     0.100     0.000     0.000" +
                                 "      NO3 =      6.503     2.101     2.101     0.000     0.000" +
                                 "      NH4 =      0.599     0.100     0.100     0.000     0.000" +
                                 "Barley LL =      0.290     0.280     0.290     0.290     0.300");
         }


      /// <summary>
      /// Make sure a soil can get the values of crop variables.
      /// </summary>
      [Test]
      public void GetCropVariables()
         {
         string[] Crops = Soil.Crops(S);
         Assert.AreEqual(Crops.Length, 2);
         Assert.AreEqual(Crops[0], "Barley");
         Assert.AreEqual(Crops[1], "Chickpea");
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "Barley LL").Doubles,
                                            new double[] { 0.290, 0.280 }));
         Soil.Variable PAW = Soil.Get(S, "Barley PAW");
         PAW.Units = "mm";
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(PAW.Doubles), 75.0));
         }


      /// <summary>
      /// Make sure a new crop can be added to a soil
      /// </summary>
      [Test]
      public void AddNewCrop()
         {
         double[] Thickness = new double[] { 100, 600 };

         Soil.Variable ll = new Soil.Variable("x LL", "mm/mm", new double[] { 0.290, 0.280 }, Thickness, null);
         Soil.Variable kl = new Soil.Variable("x KL", "/day", new double[] { 0.060, 0.040 }, Thickness, null);
         Soil.Variable xf = new Soil.Variable("x XF", "0-1", new double[] { 1.000, 0.500 }, Thickness, null);

         Soil.Set(S, ll);
         Soil.Set(S, kl);
         Soil.Set(S, xf);

         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "x ll").Doubles, new double[] { 0.290, 0.280}));
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "x kl").Doubles, new double[] { 0.060, 0.040}));
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "x xf").Doubles, new double[] { 1.000, 0.500}));
         }

      /// <summary>
      /// Make sure we can get the values of predicted crop values.
      /// </summary>
      [Test]
      public void PredictedCrops()
         {
         Soil.Variable SoilType = new Soil.Variable("SoilType", "Black Vertosol");
         Soil.Set(S, SoilType);
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "Wheat LL").Doubles,
                                            new double[] { 0.29, 0.29, 0.29, 0.29, 0.30 }));
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "Wheat KL").Doubles,
                                            new double[] { 0.06, 0.06, 0.06, 0.05, 0.04 }));
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "Wheat XF").Doubles,
                                            new double[] { 1.0, 1.0, 0.0, 0.0, 0.0 }));
         }

      /// <summary>
      /// Make sure that a soil can get the PAWC by layer relative to LL15 
      /// and also to a crop LL.
      /// </summary>
      [Test]
      public void PAWC()
         {
         Soil.Variable SoilPAWC = Soil.Get(S, "PAWC");
         SoilPAWC.Units = "mm";
         Soil.Variable ChickpeaPAWC = Soil.Get(S, "Chickpea PAWC");
         ChickpeaPAWC.Units = "mm";
         Soil.Variable PAW = Soil.Get(S, "PAW");
         PAW.Units = "mm";
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(SoilPAWC.Doubles), 289.5));
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(ChickpeaPAWC.Doubles), 88.5));
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(PAW.Doubles), 289.5));
         }

      /// <summary>
      /// Make sure that a soil get the value of SW when there is an
      /// <initwater> element in the soil.
      /// </summary>
      [Test]
      public void SWFromInitWater()
         {
         Soil.Variable SW = Soil.Get(S, "SW");
         SW.Units = "mm/mm";
         Assert.IsTrue(MathUtility.AreEqual(SW.Doubles,
                                            new double[] { 0.540, 0.530, 0.540, 0.540, 0.520 }));
         }

      /// <summary>
      /// Make sure the soil class can read variables
      /// from a data table.
      /// </summary>
      [Test]
      public void DataTableRead()
         {
         // Create a data table with 2 soils in it.
         DataTable Table = new DataTable();
         DataTableUtility.AddColumn(Table, "Thickness (mm)",       new double[] {     100,          300,        300,});
         DataTableUtility.AddColumn(Table, "BD (g/cc)",            new double[] {     1.1,          1.2,        1.3,});
         DataTableUtility.AddColumn(Table, "OC (Walkley Black %)", new double[] { 4.6, 3.6, 2.6});
         DataTableUtility.AddColumn(Table, "Wheat LL(mm/mm)",      new double[] {     20,           19,         18});
         DataTableUtility.AddColumn(Table, "Wheat KL(/day)",      new double[] {     1.0,          0.9,        0.8});

         XmlNode S1 = Soil.Create("Soil1");
         Soil.ReadFromTable(S1, Table);

         Assert.AreEqual(Soil.Get(S1, "BD").Doubles, new double[] { 1.1, 1.2, 1.3 });
         Assert.AreEqual(Soil.Get(S1, "OC").Doubles, new double[] { 4.6, 3.6, 2.6 });
         Assert.AreEqual(Soil.Get(S1, "Wheat LL").Doubles, new double[] { 20, 19, 18 });
         Assert.AreEqual(Soil.Get(S1, "Wheat KL").Doubles, new double[] { 1.0, 0.9, 0.8 });
         }


      /// <summary>
      /// Ensure that a soil can write a datatable of a specific profile 
      /// node. i.e. No mapping. This is used by the ProfileUI.
      /// </summary>
      [Test]
      public void WriteToTableUnMapped()
         {
         List<string> VariableNames = new List<string>();
         VariableNames.Add("Thickness (mm)");
         VariableNames.Add("Depth (cm)");
         VariableNames.Add("SWCON (0-1)");

         DataTable Table = new DataTable();

         Soil.WriteToTable(S, Table, VariableNames);

         // Check that we have variables.
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Thickness (mm)"),
                                            new double[] { 100, 100, 200 }));
         string[] Depths = DataTableUtility.GetColumnAsStrings(Table, "Depth (cm)");
         Assert.AreEqual(Depths[0], "0-10");
         Assert.AreEqual(Depths[1], "10-20");
         Assert.AreEqual(Depths[2], "20-40");
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "SWCON (0-1)"),
                                            new double[] { 0.3, 0.3, 0.3 }));
         }

      /// <summary>
      /// Ensure that a soil can write a datatable of a water and crop and Crop PAWC
      /// numbers with no mapping of water variables but with crop variables mapped.
      /// This is used by the ProfileUI. e.g.
      ///     Depth   LL15   DUL  Barley LL   Barley PAWC   Barley KL  Barley XF
      /// </summary>
      [Test]
      public void WriteCropsToWaterTable()
         {
         List<string> VariableNames = new List<string>();
         VariableNames.Add("Thickness (mm)");
         VariableNames.Add("Depth (cm)");
         VariableNames.Add("LL15 (mm/mm)");
         VariableNames.Add("DUL (mm/mm)");
         VariableNames.Add("Barley LL (mm/mm)");
         VariableNames.Add("Barley PAWC (mm)");
         VariableNames.Add("Barley KL (/day)");
         VariableNames.Add("Barley XF (0-1)");

         DataTable Table = new DataTable();

         Soil.WriteToTable(S, Table, VariableNames);

         // Check that we have variables.
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Thickness (mm)"),
                                   new double[] { 150, 150, 300, 300, 300 }));

         string[] Depths = DataTableUtility.GetColumnAsStrings(Table, "Depth (cm)");
         Assert.AreEqual(Depths[0], "0-15");
         Assert.AreEqual(Depths[1], "15-30");
         Assert.AreEqual(Depths[2], "30-60");
         Assert.AreEqual(Depths[3], "60-90");
         Assert.AreEqual(Depths[4], "90-120");
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley LL (mm/mm)"),
                                   new double[] { 0.29, 0.28, 0.29, 0.29, 0.30 }));
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley PAWC (mm)"),
                                   new double[] { 37.5, 37.5, 0, 0, 0 }));
         }

      /// <summary>
      /// Ensure that a soil can write a datatable of a Crop specific profile 
      /// node. i.e. No mapping. This is used by the ProfileUI.
      /// </summary>
      [Test]
      public void WriteCropDataTable()
         {
         List<string> VariableNames = new List<string>();
         VariableNames.Add("Thickness (mm)");
         VariableNames.Add("Depth (cm)");
         VariableNames.Add("Barley LL (mm)");
         VariableNames.Add("Barley KL (/day)");
         VariableNames.Add("Barley XF (0-1)");
         VariableNames.Add("Barley PAWC (mm)");

         DataTable Table = new DataTable();

         Soil.WriteToTable(S, Table, VariableNames);

         // Check that we have variables.
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Thickness (mm)"),
                                            new double[] { 150, 150}));
         string[] Depths = DataTableUtility.GetColumnAsStrings(Table, "Depth (cm)");
         Assert.AreEqual(Depths[0], "0-15");
         Assert.AreEqual(Depths[1], "15-30");
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley LL (mm)"),
                                            new double[] { 43.5, 42 }));
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley KL (/day)"),
                                            new double[] { 0.1, 0.1 }));
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley XF (0-1)"),
                                            new double[] { 1, 1 }));
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley PAWC (mm)"),
                                            new double[] { 37.5, 37.5 }));

         }


      /// <summary>
      /// Ensure that a soil can write a datatable given a custom list of variable names and
      /// properties. The import from spreadsheet option will use this.
      /// </summary>
      [Test]
      public void DataTableWriteCustom()
         {
         List<string> VariableNames = new List<string>();
         VariableNames.Add("Site");
         VariableNames.Add("Region");
         VariableNames.Add("Latitude");
         VariableNames.Add("Thickness (mm)");
         VariableNames.Add("DepthMidPoints (cm)");
         VariableNames.Add("Depth (cm)");
         VariableNames.Add("BD (g/cc)");
         VariableNames.Add("Rocks (%)");
         VariableNames.Add("FBiom (0-1)");
         VariableNames.Add("Barley LL (mm/mm)");
         VariableNames.Add("Barley KL (/day)");
         VariableNames.Add("Chickpea XF (0-1)");


         DataTable Table = new DataTable();
         Soil.WriteToTable(S, Table, VariableNames);

         // Check that we have properties.
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table, "Site", 5)[0], "My site");
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table, "Site", 5)[4], "My site");
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table, "Region", 5)[0], "My region");
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table, "Region", 5)[4], "My region");
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table, "Latitude", 5)[0], "");

         // Check that we have variables.
         Assert.AreEqual(Table.Columns.IndexOf("Thickness (mm)"), 3);
         Assert.AreEqual(Table.Columns.IndexOf("DepthMidPoints (cm)"), 4);
         Assert.AreEqual(Table.Columns.IndexOf("Depth (cm)"), 5);
         Assert.AreEqual(Table.Columns.IndexOf("BD (g/cc)"), 6);
         Assert.AreEqual(Table.Columns.IndexOf("Rocks (%)"), 7);
         Assert.IsTrue(Table.Columns.Contains("FBiom (0-1)"));
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "FBiom (0-1)"),
                                            new double[] {0.025, 0.02167, 0.0066666, 0.0, 0.0})); 

         // Now check that we have the crops.
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley LL (mm/mm)"),
                                            new double[] { 0.29, 0.28, 0.29, 0.29, 0.30 }));
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley KL (/day)"),
                                            new double[] { 0.1, 0.1, 0.0, 0.0, 0.0 }));
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Chickpea XF (0-1)"),
                                            new double[] { 1.0, 1.0, 1.0, 0.33333, 0.0 })); 
         }


      /// <summary>
      /// Make sure the caller can delete values from a soil variable using the 
      /// MathUtility.MissingValue.
      /// </summary>
      [Test]
      public void DeleteValues()
         {
         double[] Thickness = new double[4] {100, 100, 100, 100}; 
         Soil.Variable ChickpeaLL = new Soil.Variable("Chickpea LL", "mm/mm",
                                                      new double[] { 0.30, 0.31, 999999.0, 0.32 },
                                                      Thickness, null);

         Soil.Set(S, ChickpeaLL);
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "Chickpea LL").Doubles,
                                   new double[] { 0.30, 0.31, 999999.0, 0.32 }));
         DataTable Table = new DataTable();
         List<string> VariableNames = new List<string>();
         VariableNames.Add("Chickpea LL (mm/mm)");
         Soil.WriteToTable(S, Table, VariableNames);
         Assert.AreEqual(Table.Rows[0][0], 0.30);
         Assert.AreEqual(Table.Rows[1][0], 0.31);
         Assert.AreEqual(Table.Rows[2][0], DBNull.Value);
         Assert.AreEqual(Table.Rows[3][0], 0.32);
         }

      /// <summary>
      /// Make sure we can get soil organic matter calculated variables that are NOT mapped.
      /// </summary>
      [Test]
      public void GetSoilOrganicMatterVariables()
         {
         double[] InertC = Soil.Get(S, "InertC").Doubles;
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(InertC), 25290.46));
         Assert.AreEqual(InertC.Length, 2);

         double[] BiomC = Soil.Get(S, "BiomC").Doubles;
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(BiomC), 590.63818));
         Assert.AreEqual(BiomC.Length, 2);

         // Make sure we can get them in a DataTable as well.
         List<string> VariableNames = new List<string>();
         VariableNames.Add("Thickness (mm)");
         VariableNames.Add("Depth (cm)");
         VariableNames.Add("OC (Total %)");
         VariableNames.Add("InertC (kg/ha)");
         VariableNames.Add("BiomC (kg/ha)");

         DataTable Table = new DataTable();

         Soil.WriteToTable(S, Table, VariableNames);

         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Thickness (mm)"),
                                            new double[] { 200, 200 }));
         string[] Depths = DataTableUtility.GetColumnAsStrings(Table, "Depth (cm)");
         Assert.AreEqual(Depths[0], "0-20");
         Assert.AreEqual(Depths[1], "20-40");

         InertC = DataTableUtility.GetColumnAsDoubles(Table, "InertC (kg/ha)");
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(InertC), 25290.46));
         Assert.AreEqual(InertC.Length, 2);

         BiomC = DataTableUtility.GetColumnAsDoubles(Table, "BiomC (kg/ha)");
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(BiomC), 590.63818));
         Assert.AreEqual(BiomC.Length, 2);
         }

      /// <summary>
      /// Make sure we can get codes for soil variables
      /// </summary>
      [Test]
      public void GetCodes()
         {
         // Only need to put codes on calculated variables (e.g. PAWC) and not normal variable (e.g. LL)
         // Check codes on crop variables.

         Soil.Variable BarleyLL = Soil.Get(S, "Barley LL");
         Soil.Variable BarleyPAWC = Soil.Get(S, "Barley PAWC");
         Soil.Variable InertC = Soil.Get(S, "InertC");

         Assert.AreEqual(BarleyLL.Codes[0], "Field measured");
         Assert.AreEqual(BarleyLL.Codes[1], "Laboratory measured");
         Assert.AreEqual(BarleyPAWC.Codes[0], "Calculated");
         Assert.AreEqual(InertC.Codes[0], "Calculated");
         }

      /// <summary>
      /// Make sure we can delete depths from a node by specifying a reduced number 
      /// of values for Depth(cm). This is what ProfileUI does.
      /// </summary>
      [Test]
      public void DeleteDepths()
         {
         Soil.Variable LL = Soil.Get(S, "Barley LL");
         LL.Name = "Barley LL";
         LL.ThicknessMM = new double[1];
         LL.ThicknessMM[0] = 100;
         Soil.Set(S, LL);
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "Barley LL").Doubles,
                                   new double[] { 0.29 }));
         }

      /// <summary>
      /// Make sure we can get a list of allowable units for a particular variable.
      /// </summary>
      [Test]
      public void GetAllowableUnits()
         {
         List<string> OkUnits = Soil.ValidUnits("no3");
         Assert.AreEqual(OkUnits.Count, 2);
         Assert.AreEqual(OkUnits[0], "ppm");
         Assert.AreEqual(OkUnits[1], "kg/ha");
         }

      /// <summary>
      /// Make sure we can change the units of a variable in a soil sample.
      /// </summary>
      [Test]
      public void ChangeUnitsInSoilSample()
         {
         Soil.Variable NO3 = Soil.Get(S, "NO3");
         NO3.Units = "kg/ha";
         Assert.IsTrue(MathUtility.AreEqual(NO3.Doubles, new double[] {9.949595, 3.2460467, 6.4290634}));
         }


      /// <summary>
      /// Make sure we can set codes for our soil variables.
      /// </summary>
      [Test]
      public void SetCodes()
         {
         string[] Codes = { "U", "E" };
         Soil.Variable LL = Soil.Get(S, "Barley LL");
         LL.Name = "Barley LL";
         LL.Codes[0] = "U";
         LL.Codes[1] = "E";
         Soil.Set(S, LL);

         Codes = Soil.Get(S, "Barley LL").Codes;
         Assert.AreEqual(Codes.Length, 2);
         Assert.AreEqual(Codes[0], "U");
         Assert.AreEqual(Codes[1], "E");

         }

      }
   }
