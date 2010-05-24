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
      private Soil S;
      private const string SoilContents =
            "<soil name=\"Test Soil\">" +
            "  <Site>My site</Site>" +
            "  <Region>My region</Region>" +
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
            "  <Sample name=\"Initial nitrogen2\">" +
            "    <Date type=\"date\" description=\"Sample date:\">2/6/2009</Date>" +
            "    <Layer>" +
            "      <Thickness units=\"mm\">150</Thickness>" +
            "      <NO3 units=\"ppm\">10</NO3>" +
            "      <NH4 units=\"ppm\">29</NH4>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>150</Thickness>" +
            "      <NO3>11</NO3>" +
            "      <NH4>3</NH4>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>300</Thickness>" +
            "      <NO3>12</NO3>" +
            "      <NH4>4</NH4>" +
            "    </Layer>" +
            "    <Layer>" +
            "      <Thickness>300</Thickness>" +
            "      <NO3>13</NO3>" +
            "      <NH4>5</NH4>" +
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
         string NewStr = S.ReplaceSoilMacros(Str);
         Assert.AreEqual(NewStr, "     Name = Test Soil" +
                                 "  CN2Bare = 73" +
                                 "Thickness =    150.000   150.000   300.000   300.000   300.000" +
                                 "       BD =      1.020     1.030     1.020     1.020     1.060" +
                                 "    SWCon =      0.300     0.300     0.100     0.000     0.000" +
                                 "      NO3 =      6.503     2.101     2.101     1.000     1.000" +
                                 "      NH4 =      0.599     0.100     0.100     0.200     0.200" +
                                 "Barley LL =      0.290     0.280     0.280     0.280     0.280");
         }


      /// <summary>
      /// Make sure a soil can get the values of crop variables.
      /// </summary>
      [Test]
      public void GetCropVariables()
         {
         string[] Crops = S.Crops;
         Assert.AreEqual(Crops.Length, 2);
         Assert.AreEqual(Crops[0], "Barley");
         Assert.AreEqual(Crops[1], "Chickpea");
         Assert.IsTrue(MathUtility.AreEqual(S.Variable("Barley LL (mm/mm)"),
                                            new double[] { 0.290, 0.280, 0.280, 0.280, 0.280 }));
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(S.Variable("Barley PAW(mm)")), 187.5));
         }


      /// <summary>
      /// Make sure a new crop can be added to a soil
      /// </summary>
      [Test]
      public void AddNewCrop()
         {
         double[] ll = new double[] { 0.290, 0.280 };
         double[] kl = new double[] { 0.060, 0.040 };
         double[] xf = new double[] { 1.000, 0.500 };
         double[] Thickness = new double[] { 100, 600 };

         S.SetVariable("x Thickness (mm)", Thickness);
         S.SetVariable("x LL (mm/mm)", ll);
         S.SetVariable("x KL (/day)", kl);
         S.SetVariable("x XF (0-1)", xf);

         Assert.IsTrue(MathUtility.AreEqual(S.Variable("x ll(mm/mm)"), new double[] {0.28667, 0.280, 0.280, 0.280, 0.280}));
         Assert.IsTrue(MathUtility.AreEqual(S.Variable("x kl(/day)"), new double[] {0.05333, 0.040, 0.040, 0.040, 0.040 }));
         Assert.IsTrue(MathUtility.AreEqual(S.Variable("x xf(0-1)"), new double[] {0.83333, 0.500, 0.500, 0.500, 0.500 }));
         }

      /// <summary>
      /// Make sure we can get the values of predicted crop values.
      /// </summary>
      [Test]
      public void PredictedCrops()
         {
         S.SetProperty("SoilType", "Black Vertosol");
         Assert.IsTrue(MathUtility.AreEqual(S.Variable("Wheat LL (mm)"),
                                            new double[] { 43.5, 43.5, 87, 87, 90 }));
         Assert.IsTrue(MathUtility.AreEqual(S.Variable("Wheat KL (/day)"),
                                            new double[] { 0.06, 0.06, 0.06, 0.04, 0.04 }));
         Assert.IsTrue(MathUtility.AreEqual(S.Variable("Wheat XF (0-1)"),
                                            new double[] { 1.0, 1.0, 1.0, 1.0, 1.0 }));
         }

      /// <summary>
      /// Make sure that a soil can get the PAWC by layer relative to LL15 
      /// and also to a crop LL.
      /// </summary>
      [Test]
      public void PAWC()
         {
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(S.Variable("PAWC(mm)")), 289.5));
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(S.Variable("Chickpea PAWC(mm)")), 51.6666667));
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(S.Variable("PAW(mm)")), 289.5));
         }

      /// <summary>
      /// Make sure that a soil get the value of SW when there is an
      /// <initwater> element in the soil.
      /// </summary>
      [Test]
      public void SWFromInitWater()
         {
         Assert.IsTrue(MathUtility.AreEqual(S.Variable("SW (mm/mm)"),
                                            new double[] { 0.540, 0.530, 0.540, 0.540, 0.520 }));
         }

      /// <summary>
      /// Make sure the soil class can read variables and properties
      /// from a data table.
      /// </summary>
      [Test]
      public void DataTableRead()
         {
         // Create a data table with 2 soils in it.
         DataTable Table = new DataTable();
         DataTableUtility.AddColumn(Table, "Name",                 new string[] { "S1",        "S1",        "S1",        "S2",     "S2",    "S2" });
         DataTableUtility.AddColumn(Table, "Region",               new string[] { "Toowoomba", "Toowoomba", "Toowoomba", "Dalby",  "Dalby", "Dalby" });
         DataTableUtility.AddColumn(Table, "Thickness (mm)",       new double[] {     100,          300,        300,       150,     200,     200 });
         DataTableUtility.AddColumn(Table, "BD (g/cc)",            new double[] {     1.1,          1.2,        1.3,       1.4,     1.5,     1.6 });
         DataTableUtility.AddColumn(Table, "OC (Walkley Black %)", new double[] { 4.6, 3.6, 2.6, 1.6, 0.6, 0.1 });
         DataTableUtility.AddColumn(Table, "Wheat LL(mm/mm)",      new double[] {     20,           19,         18,        17,      16,      15 });
         DataTableUtility.AddColumn(Table, "Wheat KL(mm/mm)",      new double[] {     1.0,          0.9,        0.8,       0.7,     0.6,     0.5 });

         Soil S1 = Soil.Create("Soil1");
         S1.Read(Table, 0);

         Soil S2 = Soil.Create("Soil2");
         S2.Read(Table, 3);

         Assert.AreEqual(S1.Name, "S1");
         Assert.AreEqual(S1.Property("Region"), "Toowoomba");
         Assert.AreEqual(S1.Variable("Thickness(mm)"), new double[] {100, 300, 300} );
         Assert.AreEqual(S1.Variable("BD (g/cc)"), new double[] { 1.1, 1.2, 1.3 });
         Assert.AreEqual(S1.Variable("OC (Walkley Black %)"), new double[] { 4.6, 3.6, 2.6 });
         Assert.AreEqual(S1.Variable("Wheat LL(mm/mm)"), new double[] { 20, 19, 18 });
         Assert.AreEqual(S1.Variable("Wheat KL(mm/mm)"), new double[] { 1.0, 0.9, 0.8 });

         Assert.AreEqual(S2.Name, "S2");
         Assert.AreEqual(S2.Property("Region"), "Dalby");
         Assert.AreEqual(S2.Variable("Thickness(mm)"), new double[] { 150, 200, 200 });
         Assert.AreEqual(S2.Variable("BD (g/cc)"), new double[] { 1.4, 1.5, 1.6 });
         Assert.AreEqual(S2.Variable("OC (Walkley Black %)"), new double[] { 1.6, 0.6, 0.1 });
         Assert.AreEqual(S2.Variable("Wheat LL(mm/mm)"), new double[] { 17, 16, 15 });
         Assert.AreEqual(S2.Variable("Wheat KL(mm/mm)"), new double[] { 0.7, 0.6, 0.5 });

         }


      /// <summary>
      /// Make sure the soil can read from a column in a datatable.
      /// </summary>
      [Test]
      public void ReadFromTableColumn()
         {
         // Create a data table with 2 soils in it.
         DataTable Table = new DataTable();
         DataTableUtility.AddColumn(Table, "Name", new string[] { "S1", "S1", "S1", "S2", "S2", "S2" });
         DataTableUtility.AddColumn(Table, "Region", new string[] { "Toowoomba", "Toowoomba", "Toowoomba", "Dalby", "Dalby", "Dalby" });
         DataTableUtility.AddColumn(Table, "Thickness (mm)", new double[] { 100, 300, 300, 150, 200, 200 });
         DataTableUtility.AddColumn(Table, "BD (g/cc)", new double[] { 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 });

         S.Read(Table, "BD (g/cc)", "Water");
         Assert.AreEqual(S.Variable("BD (g/cc)"), new double[] { 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 });

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
         VariableNames.Add("MWCON (0-1)");

         DataTable Table = new DataTable();

         S.WriteUnMapped(Table, VariableNames);

         // Check that we have variables.
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Thickness (mm)"),
                                            new double[] { 100, 100, 200 }));
         string[] Depths = DataTableUtility.GetColumnAsStrings(Table, "Depth (cm)");
         Assert.AreEqual(Depths[0], "0-10");
         Assert.AreEqual(Depths[1], "10-20");
         Assert.AreEqual(Depths[2], "20-40");
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "SWCON (0-1)"),
                                            new double[] { 0.3, 0.3, 0.3 }));

         // Make sure there is an MWCON column and that it doesn't have any values.
         Assert.IsFalse(MathUtility.ValuesInArray(DataTableUtility.GetColumnAsDoubles(Table, "MWCON (0-1)")));
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

         S.Write(Table, VariableNames);

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
                                   new double[] { 0.29, 0.28, 0.28, 0.28, 0.28 }));
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley PAWC (mm)"),
                                   new double[] { 37.5, 37.5, 37.5, 37.5, 37.5 }));
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

         S.WriteUnMapped(Table, VariableNames);

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
      /// Make sure that a soil can be exported to a spreadsheet and then reimported.
      /// </summary>
      [Test]
      public void SpreadsheetReadWrite()
         {
         string TestFileName = Directory.GetCurrentDirectory() + "\\test.xls";

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml("<dummy>" + S.XML + "</dummy>");

         StringCollection Paths = new StringCollection();
         Paths.Add("Test Soil");
         SoilSpreadsheet.Export(TestFileName, Doc.DocumentElement, Paths);
         Assert.IsTrue(File.Exists(TestFileName));
         Soil NewSoil = Soil.CreateFromXML(SoilSpreadsheet.Import(TestFileName));

         Assert.AreEqual(NewSoil.Name, "Test Soil");
         Assert.IsTrue(MathUtility.AreEqual(S.Variable("Thickness (mm)"),
                                            new double[] { 150.000,   150.000,   300.000,   300.000,   300.000 }));
         Assert.IsTrue(MathUtility.AreEqual(S.Variable("BD (g/cc)"),
                                            new double[] { 1.020,     1.030,     1.020,     1.020,     1.060 }));

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
         S.Write(Table, VariableNames);

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
                                            new double[] {0.025, 0.02167, 0.02, 0.02, 0.02})); 

         // Now check that we have the crops.
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley LL (mm/mm)"),
                                            new double[] { 0.29, 0.28, 0.28, 0.28, 0.28 }));
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Barley KL (/day)"),
                                            new double[] { 0.1, 0.1, 0.1, 0.1, 0.1 }));
         Assert.IsTrue(MathUtility.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Chickpea XF (0-1)"),
                                            new double[] { 1.0, 1.0, 1.0, 0.33333, 0.0 })); 
         }

      /// <summary>
      /// Ensure that a soil can return a list of valid variable names for a specified profile node 
      /// eg. Water, SoilWat.
      /// </summary>
      [Test]
      public void ValidVariableNames()
         {
         List<string> Variables = S.ValidVariablesForProfileNode("Water", "");
         Assert.IsTrue(Variables.Contains("Thickness (mm)"));
         Assert.IsTrue(Variables.Contains("BD (g/cc)"));
         Assert.IsTrue(Variables.Contains("KS (mm/day)"));
         Assert.IsTrue(Variables.Contains("AirDry (mm/mm)"));
         Assert.IsTrue(Variables.Contains("LL15 (mm/mm)"));
         Assert.IsTrue(Variables.Contains("DUL (mm/mm)"));
         Assert.IsTrue(Variables.Contains("SAT (mm/mm)"));

         Variables = S.ValidVariablesForProfileNode("SoilWat", "");
         Assert.IsTrue(Variables.Contains("SWCON (0-1)"));
         Assert.IsTrue(Variables.Contains("MWCON (0-1)"));

         Variables = S.ValidVariablesForProfileNode("SoilOrganicMatter", "");
         Assert.IsTrue(Variables.Contains("OC (Walkley Black %)"));
         Assert.IsTrue(Variables.Contains("FBiom (0-1)"));
         Assert.IsTrue(Variables.Contains("FInert (0-1)"));

         Variables = S.ValidVariablesForProfileNode("SoilCrop", "Barley");
         Assert.IsTrue(Variables.Contains("Barley LL (mm/mm)"));
         Assert.IsTrue(Variables.Contains("Barley KL (/day)"));
         Assert.IsTrue(Variables.Contains("Barley XF (0-1)"));

         }

      /// <summary>
      /// Test that the soil's TargetThickness can be modified. 
      /// </summary>
      [Test]
      public void SetTargetThickness()
         {
         double[] TargetThickness =  {100, 500};

         S.TargetThickness = TargetThickness;
         Assert.IsTrue(MathUtility.AreEqual(S.Variable("ll15 (mm)"), new double[] { 29.0, 145.0 }));

         }

      /// <summary>
      /// Make sure the caller can delete values from a soil variable using the 
      /// MathUtility.MissingValue.
      /// </summary>
      [Test]
      public void DeleteValues()
         {
         double[] NewValues = { 0.30, 0.31, 999999.0, 0.32 };
         S.SetVariable("Chickpea LL (mm/mm)", NewValues);
         Assert.IsTrue(MathUtility.AreEqual(S.VariableUnMapped("Chickpea LL (mm/mm)"),
                                   new double[] { 0.30, 0.31, 999999.0, 0.32 }));
         DataTable Table = new DataTable();
         List<string> VariableNames = new List<string>();
         VariableNames.Add("Chickpea LL (mm/mm)");
         S.WriteUnMapped(Table, VariableNames);
         Assert.AreEqual(Table.Rows[0][0], 0.30);
         Assert.AreEqual(Table.Rows[1][0], 0.31);
         Assert.AreEqual(Table.Rows[2][0], DBNull.Value);
         Assert.AreEqual(Table.Rows[3][0], 0.32);
         }

      /// <summary>
      /// Make sure that depth and thickness are interchangable i.e. that you can
      /// get and set both at will.
      /// </summary>
      [Test]
      public void InterchangableDepthThickness()
         {
         // Make sure we can change depth strings.
         string[] NewDepthValues = { "0-15", "15-30", "", "60-90", "90-120" };
         DataTable DepthTable = new DataTable();
         DataTableUtility.AddColumn(DepthTable, "Depth (cm)", NewDepthValues);
         S.Read(DepthTable, "Depth (cm)", "Water");

         DataTable NewDepthTable = new DataTable();
         List<string> VariableNames = new List<string>();
         VariableNames.Clear();
         VariableNames.Add("Depth (cm)");
         VariableNames.Add("Thickness (cm)");
         S.Write(NewDepthTable, VariableNames);

         string[] NewDepthValues2 = DataTableUtility.GetColumnAsStrings(NewDepthTable, "Depth (cm)");
         Assert.AreEqual(NewDepthValues2.Length, 5);
         Assert.AreEqual(NewDepthValues2[0], "0-15");
         Assert.AreEqual(NewDepthValues2[1], "15-30");
         Assert.AreEqual(NewDepthValues2[2], "");
         Assert.AreEqual(NewDepthValues2[3], "30-60");
         Assert.AreEqual(NewDepthValues2[4], "60-90");

         string[] NewThicknessValues = DataTableUtility.GetColumnAsStrings(NewDepthTable, "Thickness (cm)");
         Assert.AreEqual(NewThicknessValues.Length, 5);
         Assert.AreEqual(NewThicknessValues[0], "15");
         Assert.AreEqual(NewThicknessValues[1], "15");
         Assert.AreEqual(NewThicknessValues[2], "");
         Assert.AreEqual(NewThicknessValues[3], "30");
         Assert.AreEqual(NewThicknessValues[4], "30");
         
         }

      /// <summary>
      /// Make sure we can get soil organic matter calculated variables that are NOT mapped.
      /// </summary>
      [Test]
      public void GetSoilOrganicMatterVariables()
         {
         double[] InertC = S.VariableUnMapped("InertC (kg/ha)");         
         Assert.IsTrue(MathUtility.FloatsAreEqual(MathUtility.Sum(InertC), 25290.46));
         Assert.AreEqual(InertC.Length, 2);

         double[] BiomC = S.VariableUnMapped("BiomC (kg/ha)");         
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

         S.WriteUnMapped(Table, VariableNames);

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
         List<string> VariableNames = new List<string>();
         VariableNames.Add("Barley LLCode");
         VariableNames.Add("Barley PAWCCode (mm)");
         DataTable Table = new DataTable();
         S.WriteUnMapped(Table, VariableNames);
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table, "Barley LLCode")[0], "Field measured");
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table, "Barley LLCode")[1], "Laboratory measured");
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table, "Barley PAWCCode (mm)")[0], "Calculated");

         // Check codes on soil organic matter variables.

         List<string> VariableNames2 = new List<string>();
         VariableNames2.Add("InertCCode (kg/ha)");
         VariableNames2.Add("BiomCCode (kg/ha)");
         VariableNames2.Add("HumCCode (kg/ha)");
         DataTable Table2 = new DataTable();
         S.WriteUnMapped(Table2, VariableNames2);
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table2, "InertCCode (kg/ha)")[0], "Calculated");
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table2, "BiomCCode (kg/ha)")[0], "Calculated");
         Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table2, "HumCCode (kg/ha)")[0], "Calculated");
         }

      /// <summary>
      /// Make sure we can delete depths from a node by specifying a reduced number 
      /// of values for Depth(cm). This is what ProfileUI does.
      /// </summary>
      [Test]
      public void DeleteDepths()
         {
         string[] NewValues = { "0-10" };
         S.SetVariable("Barley Depth (cm)", NewValues);
         Assert.IsTrue(MathUtility.AreEqual(S.VariableUnMapped("Barley LL (mm/mm)"),
                                   new double[] { 0.29 }));
         }

      /// <summary>
      /// Make sure we can get a list of allowable units for a particular variable.
      /// </summary>
      [Test]
      public void GetAllowableUnits()
         {
         List<string> OkUnits = S.ValidUnits("no3");
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
         // Create a data table with 2 soils in it.
         DataTable Table = new DataTable();
         DataTableUtility.AddColumn(Table, "NO3 (kg/ha)", new double[] {1, 2, 3 });

         S.Read(Table, "NO3 (kg/ha)", "Initial Nitrogen");

         double[] NewValues = S.VariableUnMapped("NO3 (kg/ha)", "Initial Nitrogen");

         Assert.AreEqual(NewValues, new double[] {1, 2, 3 });
         }

      /// <summary>
      /// Make sure we can write a sample to a datatable.
      /// </summary>
      [Test]
      public void WriteSampleToTable()
         {
         List<string> VariableNames = new List<string>();
         VariableNames.Add("Depth (cm)");
         VariableNames.Add("NO3 (ppm)");
         VariableNames.Add("SW (mm/mm)");

         DataTable Table = new DataTable();
         S.WriteUnMapped(Table, VariableNames, "Initial nitrogen2");

         // Check that we have variables.
         string[] Depths = DataTableUtility.GetColumnAsStrings(Table, "Depth (cm)");
         double[] NO3 = DataTableUtility.GetColumnAsDoubles(Table, "NO3 (ppm)");
         double[] SW = DataTableUtility.GetColumnAsDoubles(Table, "SW (mm/mm)");

         Assert.AreEqual(Depths, new string[] { "0-15", "15-30", "30-60", "60-90" });

         Assert.AreEqual(NO3, new double[] { 10, 11, 12, 13 });
         Assert.IsFalse(MathUtility.ValuesInArray(SW));
         }

      /// <summary>
      /// Make sure we can set codes for our soil variables.
      /// </summary>
      [Test]
      public void SetCodes()
         {
         string[] Codes = { "U", "E" }; 
         S.SetVariable("Barley LLCode", Codes);

         Codes = S.VariableAsStrings("Barley LLCode");
         Assert.AreEqual(Codes.Length, 2);
         Assert.AreEqual(Codes[0], "U");
         Assert.AreEqual(Codes[1], "E");

         }

      /// <summary>
      /// Some soil variables have special "Method" variables e.g. OCMethod, PHMethod.
      /// Make sure we can read and write these. Neal D's spreadsheet has these in them.
      /// </summary>
      [Test]
      public void SetMethodVariableNames()
         {
         // Create a data table with 2 soils in it.
         DataTable Table = new DataTable();
         DataTableUtility.AddColumn(Table, "Name", new string[] { "X", "X" });
         DataTableUtility.AddColumn(Table, "OC", new double[] { 1.0, 0.5 });
         DataTableUtility.AddColumn(Table, "OCMethod", new string[] { "Total %", "Total %" });

         S.Read(Table, 0);

         double[] OC = S.VariableUnMapped("OC (Walkley Black %)");
         Assert.AreEqual(OC.Length, 2);
         Assert.IsTrue(MathUtility.AreEqual(OC, new double[] {0.7692301, 0.384615} ));
         }


      }
   }
