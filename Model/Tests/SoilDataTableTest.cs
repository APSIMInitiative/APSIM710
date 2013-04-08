using System;
using System.Text;
using NUnit.Framework;
using ApsimFile;
using System.IO;
using CSGeneral;
using System.Xml;
using System.Data;


namespace APSIM.Tests
{
    [TestFixture]
    public class SoilDataTableTest
    {

        [Test]
        public void ConvertDataTableToSoil()
        {
            double NaN = double.NaN;


            DataTable Table = new DataTable();
            DataTableUtility.AddColumn(Table, "Name",              new string[] {"soil1", "soil1", "soil1", "soil1", "soil2", "soil2", "soil2", "soil2", "soil2", "soil2"});
            DataTableUtility.AddColumn(Table, "Country",           new string[] {"Aust",  "Aust",   "Aust",  "Aust",    "NZ",    "NZ",    "NZ",    "NZ",    "NZ",    "NZ"});
            DataTableUtility.AddColumn(Table, "Thickness (mm)",  new double[] { 150.00,    150.00,  300.00,     NaN,  50.00 ,   50.00,  150.00,  300.00,  300.00, 300.00 });
            DataTableUtility.AddColumn(Table, "BD (g/cc)",         new double[] { 1.02,      1.03,    1.02,     NaN,   1.06 ,    1.02,    1.03,    1.02,    1.02,   1.06 });
            DataTableUtility.AddColumn(Table, "BDCode",            new string[] { "FM",      "LM","C_grav",     "E",     "" ,      "",      "",      "",      "",     "" });

            DataTableUtility.AddColumn(Table, "ThicknessChem (mm)",new double[] { 150.00,    150.00,  300.00,  300.00,   50.00,   50.00,  150.00,  300.00,  300.00, 300.00 });
            DataTableUtility.AddColumn(Table, "OC",                new double[] { 0.25,      1.20,     1.60,    1.80,    0.10,    0.20,    0.30,    0.40,    0.50,  00.60 });
            DataTableUtility.AddColumn(Table, "OCCode",            new string[] { "M (Walkley Black %)", "E (Walkley Black %)", "E (Walkley Black %)", "E (Walkley Black %)", "", "", "", "", "", "" });
            
            DataTableUtility.AddColumn(Table, "AirDry (mm/mm)",    new double[] { 0.15,      0.26,    0.29,    0.29,   0.30 ,  0.15,      0.26,    0.29,    0.29,   0.30 });
            DataTableUtility.AddColumn(Table, "Wheat LL (mm/mm)",  new double[] { 0.29,      0.25,    0.24,    0.23,   0.22 ,  0.29,      0.25,    0.24,    0.23,   0.22 });
            DataTableUtility.AddColumn(Table, "Wheat KL (/day)",   new double[] { 0.10,      0.08,    0.06,    0.04,   0.02 ,  0.10,      0.08,    0.06,    0.04,   0.02 });
            DataTableUtility.AddColumn(Table, "Wheat XF (0-1)" ,   new double[] { 1.00,      0.80,    0.80,    0.60,   0.60 ,  1.00,      0.80,    0.80,    0.60,   0.60 });
            DataTableUtility.AddColumn(Table, "Barley LL (mm/mm)", new double[] { 0.25,      0.24,    0.23,    0.22,   0.21 ,  0.25,      0.24,    0.23,    0.22,   0.21 });
                        
            Soil[] Soils = SoilDataTable.TableToSoils(Table);

            Assert.AreEqual(Soils.Length, 2);
            Assert.AreEqual(Soils[0].Name, "soil1");
            Assert.AreEqual(Soils[1].Name, "soil2");
            Assert.AreEqual(Soils[0].Water.BD, new double[] { 1.02, 1.03, 1.02 }, 0.01);

            Assert.IsTrue(MathUtility.AreEqual(Soils[0].Water.BDMetadata,
                                               new string[] { "Field measured and checked for sensibility", 
                                                              "Laboratory measured", 
                                                              "Calculated from gravimetric moisture when profile wet but drained" }));

            Assert.AreEqual(Soils[1].Water.BD, new double[] { 1.06, 1.02, 1.03, 1.02, 1.02, 1.06 }, 0.01);
            Assert.AreEqual(Soils[0].SoilOrganicMatter.OC, new double[] { 0.25, 1.20, 1.60, 1.80 });
            Assert.AreEqual(Soils[0].Crop("wheat").LL,  new double[] { 0.29, 0.25, 0.24 });
            Assert.AreEqual(Soils[0].Crop("wheat").KL,  new double[] { 0.10, 0.08, 0.06 });

        }


        [Test]
        public void ConvertSoilToDataTable()
        {
            Soil Soil = new Soil();
            Soil.ApsoilNumber = "100";
            Soil.Water.Thickness = new double[] { 150.00, 150.00, 300.00, 300.00, 300.00 };
            Soil.Water.BD = new double[] { 1.02, 1.03, 1.02, 1.02, 1.06 };
            Soil.Water.BDMetadata = new string[] { "Field measured and checked for sensibility", "Laboratory measured", "", "", "" };
            Soil.Water.AirDry = new double[] { 0.15, 0.26, 0.29, 0.29, 0.30 };
            Soil.Water.LL15 = new double[] { 0.29, 0.29, 0.29, 0.29, 0.30 };
            Soil.Water.DUL = new double[] { 0.54, 0.53, 0.54, 0.54, 0.52 };
            Soil.Water.SAT = new double[] { 0.59, 0.58, 0.59, 0.58, 0.57 };

            Soil.Water.Crops.Add(new SoilCrop()
            {
                Name = "Barley",
                Thickness = new double[] { 100.00, 100.00 },
                LL = new double[] { 0.29, 0.25 },
                KL = new double[] { 0.10, 0.08 },
                XF = new double[] { 1.00, 1.00 }
            });
            Soil.Water.Crops.Add(new SoilCrop()
            {
                Name = "Chickpea",
                Thickness = new double[] { 100.00, 100.00, 100.00, 100.00, 100.00, 100.00, 100.00, 100.00 },
                LL = new double[] { 0.29, 0.29, 0.36, 0.43, 0.51, 0.50, 0.50, 0.48 },
                KL = new double[] { 0.10, 0.10, 0.08, 0.06, 0.04, 0.02, 0.01, 0.00 },
                XF = new double[] { 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00 }
            });

            Soil.SoilOrganicMatter.Thickness = new double[] { 200.000, 200.00 };
            Soil.SoilOrganicMatter.OC = new double[] { 1.040, 0.89 };
            Soil.SoilOrganicMatter.FBiom = new double[] { 0.025, 0.02 };
            Soil.SoilOrganicMatter.FInert = new double[] { 0.400, 0.89 };

            Soil.InitialWater = new InitialWater();
            Soil.InitialWater.SetSW(0.5, InitialWater.PercentMethodEnum.FilledFromTop);
            Soil.SoilWater = new SoilWater();
            Soil.SoilWater.SummerCona = 3.5;
            Soil.SoilWater.WinterCona = 3.5;
            Soil.Analysis.Thickness = new double[] { 100, 100, 100 };
            Soil.Analysis.EC = new double[] { 1.7, double.NaN, double.NaN };

            DataTable Table = new DataTable();
            SoilDataTable.SoilToTable(Soil, Table);
            Assert.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Thickness (mm)"),
                            Soil.Water.Thickness, 1);
            Assert.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "BD (g/cc)"),
                            Soil.Water.BD, 0.01);
            Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table, "BDCode")[0], "FM");
            Assert.AreEqual(DataTableUtility.GetColumnAsStrings(Table, "BDCode")[1], "LM");


            Assert.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Chickpea ll (mm/mm)"),
                            new double[] { 0.29, 0.29, 0.36, 0.43, 0.51, 0.50, 0.50, 0.48 }, 0.01);
            Assert.AreEqual(DataTableUtility.GetColumnAsDoubles(Table, "Chickpea xf (0-1)"),
                            new double[] { 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.00 }, 0.01);
        }
    }
}
