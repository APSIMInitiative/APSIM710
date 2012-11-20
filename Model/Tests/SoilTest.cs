using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;
using System.IO;
using System.Xml;
using NUnit.Framework;
using CSGeneral;
using ApsimFile;

namespace APSIM.Tests
{
    class Assert : NUnit.Framework.Assert
    {
        public static void AreEqual(double[] expected, double[] actual, double delta)
        {
            expected = MathUtility.RemoveMissingValuesFromBottom(expected);
            actual = MathUtility.RemoveMissingValuesFromBottom(actual);
            AreEqual(expected.Length, actual.Length);
            for (int i = 0; i < expected.Length; i++)
            {
                AreEqual(expected[i], actual[i], delta);
            }
        }
    }

    [TestFixture]
    class TestSoil
    {
        Soil Soil;

        [SetUp]
        public void Setup()
        {
            Soil = new Soil();
            Soil.ApsoilNumber = "100";
            Soil.Water.Thickness = new double[] { 150.00, 150.00, 300.00, 300.00, 300.00 };
            Soil.Water.BD = new double[] { 1.02, 1.03, 1.02, 1.02, 1.06 };
            Soil.Water.BDMetadata = new string[] { "Field measured", "A really good point", "", "", "" };
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


        }

        [Test]
        public void SerialiseSoil()
        {
            StreamWriter Out = new StreamWriter("..\\..\\TestOut.xml");
            Out.Write(Soil.ToXml());
            Out.Close();
        }

        [Test]
        public void DeserialiseSoil()
        {
            StreamReader F = new StreamReader("..\\..\\Test.xml");
            string Xml = F.ReadToEnd();
            F.Close();
            Soil.Create(Xml);
        }

        [Test]
        public void PAWC()
        {
            Assert.AreEqual(MathUtility.Sum(Soil.PAWCmm), 289.5, 0.1);
        }

        [Test]
        public void PAW()
        {
            Assert.AreEqual(MathUtility.Sum(Soil.PAWmm("Barley")), 151.25, 0.1);
            Assert.AreEqual(MathUtility.Sum(Soil.PAWmm("chickpea")), 80.75, 0.1);
        }

        [Test]
        public void SW()
        {
            // Filled from top.
            Soil.InitialWater.SetSW(0.5, InitialWater.PercentMethodEnum.FilledFromTop);
            Assert.AreEqual(Soil.SW, new double[] { 0.540, 0.530, 0.527, 0.290, 0.300 }, 0.01);

            // Evenly distributed.
            Soil.InitialWater.SetSW(0.5, InitialWater.PercentMethodEnum.EvenlyDistributed);
            Assert.AreEqual(Soil.SW, new double[] { 0.415, 0.410, 0.415, 0.415, 0.410 }, 0.01);

            // Depth wet soil
            Soil.InitialWater.SetSW(100);
            Assert.AreEqual(Soil.SW, new double[] { 0.457, 0.290, 0.290, 0.290, 0.300 }, 0.01);
        }

        [Test]
        public void Map()
        {
            Assert.AreEqual(Soil.LL("barley"), new double[] { 0.28, 0.26, 0.29, 0.29, 0.30 }, 0.01);
        }

        [Test]
        public void OC()
        {
            Assert.AreEqual(Soil.OC, new double[] { 1.040, 0.94, 0.89, 0.89, 0.89 }, 0.01);
            Soil.Samples.Add(new Sample()
                                {
                                    Thickness = new double[] { 100 },
                                    OC = new double[] { 2.6 }
                                });
            Assert.AreEqual(Soil.OC, new double[] { 2.08, 0.94, 0.89, 0.89, 0.89 }, 0.01);
        }

        [Test]
        public void ReorderCrops()
        {
            Soil.CropNames = new string[] { "Wheat", "chickpea", "Barley" };

            Assert.AreEqual(Soil.CropNames, new string[] { "Wheat", "chickpea", "Barley" });
        }

        [Test]
        public void MissingData()
        {
            // KS should be null as it hasn't been assigned.
            Assert.AreEqual(Soil.Water.KS, null);

            // EC should have a missing value in the middle of its array.
            Assert.AreEqual(Soil.Analysis.EC, new double[] { 1.7, double.NaN, double.NaN });

        }

        [Test]
        public void Metadata()
        {
            // Test that we can change metadata and get it back.
            Soil.Water.BDMetadata = new string[] { "Field measured", "A really good point", "Test", "", "" };
            Assert.AreEqual(Soil.Water.BDMetadata, new string[] { "Field measured", "A really good point", "Test", "", "" });
        }

        [Test]
        public void ChangeUnits()
        {
            // OC units weren't specified but should default to Total.
            Soil.SoilOrganicMatter.OCUnitsSet(SoilOrganicMatter.OCUnitsEnum.WalkleyBlack);
            Assert.AreEqual(Soil.SoilOrganicMatter.OC, new double[] { 0.80, 0.68 }, 0.01);

            // OCTotal should still give us OC in total %
            Assert.AreEqual(Soil.SoilOrganicMatter.OCTotal, new double[] { 1.04, 0.89 }, 0.01);

        }


    }
}