using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Data;
using System.Collections.Generic;

namespace SoilArbitratorUnitTests
{


    /// <summary>
    ///This is a test class for SoilArbitratorTest and is intended
    ///to contain all SoilArbitratorTest Unit Tests
    ///</summary>
    [TestClass()]
    public class SoilArbitratorTest
    {


        private TestContext testContextInstance;

        /// <summary>
        ///Gets or sets the test context which provides
        ///information about and functionality for the current test run.
        ///</summary>
        public TestContext TestContext
        {
            get
            {
                return testContextInstance;
            }
            set
            {
                testContextInstance = value;
            }
        }

        #region Additional test attributes
        // 
        //You can use the following additional attributes as you write your tests:
        //
        //Use ClassInitialize to run code before running the first test in the class
        //[ClassInitialize()]
        //public static void MyClassInitialize(TestContext testContext)
        //{
        //}
        //
        //Use ClassCleanup to run code after all tests in a class have run
        //[ClassCleanup()]
        //public static void MyClassCleanup()
        //{
        //}
        //
        //Use TestInitialize to run code before running each test
        //[TestInitialize()]
        //public void MyTestInitialize()
        //{
        //}
        //
        //Use TestCleanup to run code after each test has run
        //[TestCleanup()]
        //public void MyTestCleanup()
        //{
        //}
        //
        #endregion


        /// <summary>
        ///A test for CalcMaxRootLayer
        ///</summary>
        [TestMethod()]
        [DeploymentItem("SoilArbitrator.dll")]
        public void CalcMaxRootLayerTest()
        {
            SoilArbitrator_Accessor target = new SoilArbitrator_Accessor();
            double root_depth = 100;
            double[] dlayer = new double[] { 50, 100, 200 };
            int expected = 1;
            int actual;
            actual = target.CalcMaxRootLayer(root_depth, dlayer);
            Assert.AreEqual(expected, actual);
        }

        /// <summary>
        ///A test for RootProportion
        ///</summary>
        [TestMethod()]
        [DeploymentItem("SoilArbitrator.dll")]
        public void RootProportionTest()
        {
            SoilArbitrator_Accessor target = new SoilArbitrator_Accessor();
            int layer = 1;
            double root_depth = 100;
            double[] dlayer = new double[] { 50, 100, 200 };
            double expected = 0.5;
            double actual;
            actual = target.RootProportion(layer, root_depth, dlayer);
            Assert.AreEqual(expected, actual);
        }

        /// <summary>
        ///A test for CalcCropSWLayerUptake
        ///</summary>
        [TestMethod()]
        [DeploymentItem("SoilArbitrator.dll")]
        public void CalcCropSWLayerUptakeTest()
        {
            DataTable AllRootSystems = new DataTable();
            AllRootSystems.Columns.Add("ZoneName", typeof(string));
            AllRootSystems.Columns.Add("CropType", typeof(string));
            AllRootSystems.Columns.Add("SWDemand", typeof(double));
            AllRootSystems.Columns.Add("ZoneStrength", typeof(Dictionary<string, double>)); //KVP of zones and relative strengths for water extraction
            AllRootSystems.Columns.Add("RootSystemZone", typeof(RootSystemZoneType));
            AllRootSystems.Columns.Add("HasRootSystem", typeof(bool));
            Dictionary<string, double> ZoneStrength = new Dictionary<string, double>();
            ZoneStrength.Add("Testfield1", 0.5);
            ZoneStrength.Add("Testfield2", 0.5);
            RootSystemType rsz = new RootSystemType();
            rsz.SWDemand = 5;
            rsz.Zone = new RootSystemZoneType[2];
            rsz.Zone[0] = new RootSystemZoneType();
            rsz.Zone[1] = new RootSystemZoneType();
            rsz.Zone[0].dlayer = new double[] { 50, 100, 200 };
            rsz.Zone[0].kl = new double[] { 0.05, 0.05, 0.05 };
            rsz.Zone[0].ll = new double[] { 0.15, 0.15, 0.15 };
            rsz.Zone[0].RootDepth = 100;
            rsz.Zone[1].dlayer = new double[] { 50, 100, 200 };
            rsz.Zone[1].kl = new double[] { 0.05, 0.05, 0.05 };
            rsz.Zone[1].ll = new double[] { 0.15, 0.15, 0.15 };
            rsz.Zone[1].RootDepth = 100;
            AllRootSystems.Rows.Add("Testfield1", "Crop1", 2, ZoneStrength, rsz.Zone[0], true);
            AllRootSystems.Rows.Add("Testfield2", "Crop2", 4, ZoneStrength, rsz.Zone[1], true);

            SoilArbitrator_Accessor target = new SoilArbitrator_Accessor();
            double[] CropSWDemand = new double[] { 2, 4 };
            double[,] RelSWLayerStrength = new double[,] { { 0.5, 0.5, 0 }, { 0.5, 0.5, 0 } };
            int NumLayers = 3;
            IEnumerable<DataRow> RootZones = AllRootSystems.AsEnumerable();
            double[] dlayer = new double[] { 50, 100, 200 }; ;
            double[,] expected = new double[,] { { 1, .5, 0 }, { 2, 1, 0 } };
            double[,] actual;
            actual = target.CalcCropSWLayerUptake(CropSWDemand, RelSWLayerStrength, NumLayers, RootZones, dlayer);
            Assert.AreEqual(Compare2DArrays(actual,expected), true);
        }

        /// <summary>
        ///A test for CalcRelKLStrength
        ///</summary>
        [TestMethod()]
        [DeploymentItem("SoilArbitrator.dll")]
        public void CalcRelKLStrengthTest()
        {
            double[] CropSWDemand = new double[] { 2, 4 };
            DataTable AllRootSystems = new DataTable();
            AllRootSystems.Columns.Add("ZoneName", typeof(string));
            AllRootSystems.Columns.Add("CropType", typeof(string));
            AllRootSystems.Columns.Add("SWDemand", typeof(double));
            AllRootSystems.Columns.Add("ZoneStrength", typeof(Dictionary<string, double>)); //KVP of zones and relative strengths for water extraction
            AllRootSystems.Columns.Add("RootSystemZone", typeof(RootSystemZoneType));
            AllRootSystems.Columns.Add("HasRootSystem", typeof(bool));
            Dictionary<string, double> ZoneStrength = new Dictionary<string, double>();
            ZoneStrength.Add("Testfield1", 0.5);
            ZoneStrength.Add("Testfield2", 0.5);
            RootSystemType rsz = new RootSystemType();
            rsz.SWDemand = 5;
            rsz.Zone = new RootSystemZoneType[2];
            rsz.Zone[0] = new RootSystemZoneType();
            rsz.Zone[1] = new RootSystemZoneType();
            rsz.Zone[0].dlayer = new double[] { 50, 100, 200 };
            rsz.Zone[0].kl = new double[] { 0.05, 0.05, 0.05 };
            rsz.Zone[0].ll = new double[] { 0.15, 0.15, 0.15 };
            rsz.Zone[0].RootDepth = 100;
            rsz.Zone[1].dlayer = new double[] { 50, 100, 200 };
            rsz.Zone[1].kl = new double[] { 0.05, 0.05, 0.05 };
            rsz.Zone[1].ll = new double[] { 0.15, 0.15, 0.15 };
            rsz.Zone[1].RootDepth = 100;
            AllRootSystems.Rows.Add("Testfield1", "Crop1", 2, ZoneStrength, rsz.Zone[0], true);
            AllRootSystems.Rows.Add("Testfield2", "Crop2", 4, ZoneStrength, rsz.Zone[1], true);

            SoilArbitrator_Accessor target = new SoilArbitrator_Accessor();
            IEnumerable<DataRow> RootZones = AllRootSystems.AsEnumerable();
            double[,] expected = new double[,] { { 0.5, 0.5 }, { 0.5, 0.5 }, { 0.0, 0.0 } };
            double[,] actual;
            actual = target.CalcRelKLStrength(RootZones, CropSWDemand);
            Assert.AreEqual(Compare2DArrays(actual, expected), true);
        }

        /// <summary>
        ///A test for CalcRelSWLayerStrength
        ///</summary>
        [TestMethod()]
        [DeploymentItem("SoilArbitrator.dll")]
        public void CalcRelSWLayerStrengthTest()
        {
            DataTable AllRootSystems = new DataTable();
            AllRootSystems.Columns.Add("ZoneName", typeof(string));
            AllRootSystems.Columns.Add("CropType", typeof(string));
            AllRootSystems.Columns.Add("SWDemand", typeof(double));
            AllRootSystems.Columns.Add("ZoneStrength", typeof(Dictionary<string, double>)); //KVP of zones and relative strengths for water extraction
            AllRootSystems.Columns.Add("RootSystemZone", typeof(RootSystemZoneType));
            AllRootSystems.Columns.Add("HasRootSystem", typeof(bool));
            Dictionary<string, double> ZoneStrength = new Dictionary<string, double>();
            ZoneStrength.Add("Testfield1", 0.5);
            ZoneStrength.Add("Testfield2", 0.5);
            RootSystemType rsz = new RootSystemType();
            rsz.SWDemand = 5;
            rsz.Zone = new RootSystemZoneType[2];
            rsz.Zone[0] = new RootSystemZoneType();
            rsz.Zone[1] = new RootSystemZoneType();
            rsz.Zone[0].dlayer = new double[] { 50, 100, 200 };
            rsz.Zone[0].kl = new double[] { 0.05, 0.05, 0.05 };
            rsz.Zone[0].ll = new double[] { 0.15, 0.15, 0.15 };
            rsz.Zone[0].RootDepth = 100;
            rsz.Zone[1].dlayer = new double[] { 50, 100, 200 };
            rsz.Zone[1].kl = new double[] { 0.05, 0.05, 0.05 };
            rsz.Zone[1].ll = new double[] { 0.15, 0.15, 0.15 };
            rsz.Zone[1].RootDepth = 100;
            AllRootSystems.Rows.Add("Testfield1", "Crop1", 2, ZoneStrength, rsz.Zone[0], true);
            AllRootSystems.Rows.Add("Testfield2", "Crop2", 4, ZoneStrength, rsz.Zone[1], true);

            SoilArbitrator_Accessor target = new SoilArbitrator_Accessor();
            IEnumerable<DataRow> RootZones = AllRootSystems.AsEnumerable();
            double[] SWDep = new double[] { 50, 50, 50, };
            int NumLayers = 3;
            double[,] expected = new double[,] { { 0.5, 0.5, 0 }, { 0.5, 0.5, 0 } };
            double[,] actual;
            actual = target.CalcRelSWLayerStrength(RootZones, SWDep, NumLayers);
            Assert.AreEqual(Compare2DArrays(actual, expected), true);
        }

        /// <summary>
        ///A test for CalcSWSupply
        ///</summary>
        [TestMethod()]
        [DeploymentItem("SoilArbitrator.dll")]
        public void CalcSWSupplyTest()
        {
            DataTable AllRootSystems = new DataTable();
            AllRootSystems.Columns.Add("ZoneName", typeof(string));
            AllRootSystems.Columns.Add("CropType", typeof(string));
            AllRootSystems.Columns.Add("SWDemand", typeof(double));
            AllRootSystems.Columns.Add("ZoneStrength", typeof(Dictionary<string, double>)); //KVP of zones and relative strengths for water extraction
            AllRootSystems.Columns.Add("RootSystemZone", typeof(RootSystemZoneType));
            AllRootSystems.Columns.Add("HasRootSystem", typeof(bool));
            Dictionary<string, double> ZoneStrength = new Dictionary<string, double>();
            ZoneStrength.Add("Testfield1", 0.5);
            ZoneStrength.Add("Testfield2", 0.5);
            RootSystemType rsz = new RootSystemType();
            rsz.SWDemand = 5;
            rsz.Zone = new RootSystemZoneType[2];
            rsz.Zone[0] = new RootSystemZoneType();
            rsz.Zone[1] = new RootSystemZoneType();
            rsz.Zone[0].dlayer = new double[] { 50, 100, 200 };
            rsz.Zone[0].kl = new double[] { 0.05, 0.05, 0.05 };
            rsz.Zone[0].ll = new double[] { 0.15, 0.15, 0.15 };
            rsz.Zone[0].RootDepth = 100;
            rsz.Zone[1].dlayer = new double[] { 50, 100, 200 };
            rsz.Zone[1].kl = new double[] { 0.05, 0.05, 0.05 };
            rsz.Zone[1].ll = new double[] { 0.15, 0.15, 0.15 };
            rsz.Zone[1].RootDepth = 100;
            AllRootSystems.Rows.Add("Testfield1", "Crop1", 2, ZoneStrength, rsz.Zone[0], true);
            AllRootSystems.Rows.Add("Testfield2", "Crop2", 4, ZoneStrength, rsz.Zone[1], true);

            SoilArbitrator_Accessor target = new SoilArbitrator_Accessor();
            IEnumerable<DataRow> RootZones = AllRootSystems.AsEnumerable();
            double[] SWDep = new double[] { 50, 50, 50 };
            int NumLayers = 3;
            double[,] expected = new double[,] { { 2.125, 1.75, 1 }, { 2.125, 1.75, 1 } };
            double[,] actual;
            actual = target.CalcSWSupply(RootZones, SWDep, NumLayers);
            Assert.AreEqual(Compare2DArrays(actual, expected), true);
        }

        private bool Compare2DArrays(double[,] actual, double[,] expected)
        {
            if (actual.Length != expected.Length)
                return false;

            for (int i = 0; i < actual.GetLength(0); i++)
                for (int j = 0; j < actual.GetLength(1); j++)
                {
                    if (actual[i, j] != expected[i, j])
                    {
                        return false;
                    }
                }
            return true;
        }
    }
}
