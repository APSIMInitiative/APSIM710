using System;
using System.IO;
using System.Data;
using System.Collections;
using CSGeneral;
using NUnit.Framework;
using ApsimFile;

namespace APSIM.Tests
{
    /// <summary>
    /// Test class for ApsimInputFile.
    /// </summary>
    [TestFixture]
    public class ApsimInputFileTest
    {

        // -------------------------------------
        // Write a dummy met file for testing.
        // -------------------------------------
        void WriteDummyMetFile()
        {
            const string MetContents =
                "[weather.met.weather]\r\n" +
                "Latitude =  -27.11 (deg min) ! my latitude\r\n" +

                "! TAV and AMP\r\n" +
                "tav =  19.57 (oC)     ! annual average ambient temperature\r\n" +
                "amp =  10.40 (oC)     ! annual amplitude in mean monthly temperature\r\n" +

                "site Year   day  radn   maxt   mint   rain    evap\r\n" +
                "()   ()    () (MJ/m2) (oC)   (oC)   (mm)    (mm)\r\n" +
                "DALB 1988     1 20.74   33.0   17.4    0.2    7.41\r\n" +
                "DALB 1988     2 23.43   33.8   23.0    0.0    7.41\r\n" +
                "DALB 1988     3 23.79   32.5   21.0    0.0    7.41\r\n" +
                "DALB 1988     4 19.14   30.8   19.7   34.0    7.41\r\n";

            StreamWriter Out = new StreamWriter("test.met");
            Out.Write(MetContents);
            Out.Close();
        }

        // ----------------------------------------
        // Delete the dummy met file.
        // ----------------------------------------
        void DeleteDummyMetFile()
        {
            File.Delete("test.met");
        }


        // -----------------------------------------
        // Make sure the input class can read and 
        // return temporal data.
        // -----------------------------------------
        [Test]
        public void TestReadData()
        {
            WriteDummyMetFile();
            APSIMInputFile InputFile = new APSIMInputFile();
            DataTable MetData = new DataTable();
            InputFile.ReadFromFile("test.met", MetData);

            int FirstRow = 0;
            Assert.AreEqual(MetData.Rows[FirstRow]["Site"], "DALB");
            Assert.AreEqual(MetData.Rows[FirstRow]["radn"], (float)20.74);
            Assert.AreEqual(MetData.Rows[FirstRow]["maxt"], (float)33.0);
            Assert.AreEqual(MetData.Rows[FirstRow]["mint"], (float)17.4);
            Assert.AreEqual(MetData.Rows[FirstRow]["rain"], (float)0.2);
            Assert.AreEqual(MetData.Rows[FirstRow]["evap"], (float)7.41);

            int LastRow = MetData.Rows.Count - 1;
            Assert.AreEqual(LastRow, 3);

            DeleteDummyMetFile();
        }


        // -----------------------------------------
        // Make sure the input class can read and 
        // return constants
        // -----------------------------------------
        [Test]
        public void TestReadConstants()
        {
            WriteDummyMetFile();
            APSIMInputFile InputFile = new APSIMInputFile();
            DataTable MetData = new DataTable();
            InputFile.ReadFromFile("test.met", MetData);

            ArrayList Constants = InputFile.Constants;
            Assert.AreEqual(Constants.Count, 4);
            APSIMConstant Latitude = (APSIMConstant)Constants[0];
            Assert.AreEqual(Latitude.Name, "Latitude");
            Assert.AreEqual(Latitude.Value, "-27.11");
            Assert.AreEqual(Latitude.Units, "deg min");
            Assert.AreEqual(Latitude.Comment, "my latitude");


            DeleteDummyMetFile();
        }

        // -----------------------------------------
        // Make sure the input class can read a CSV
        // file.
        // -----------------------------------------
        [Test]
        public void TestReadCSV()
        {
            const string CSVContents =
                "[weather.met.weather]\r\n" +
                "site,Year,day,radn,maxt,mint,rain,evap\r\n" +
                "(),(),(),(MJ/m2),(oC),(oC),(mm),(mm)\r\n" +
                "DALB,1988,1,20.74,33.0,17.4,0.2,7.41\r\n" +
                "DALB,1988,2,23.43,33.8,23.0,0.0,7.41\r\n" +
                "DALB,1988,3,23.79,32.5,21.0,0.0,7.41\r\n" +
                "DALB,1988,4,19.14,30.8,19.7,34.0,7.41\r\n";

            StreamWriter Out = new StreamWriter("test.csv");
            Out.Write(CSVContents);
            Out.Close();

            APSIMInputFile InputFile = new APSIMInputFile();
            DataTable MetData = new DataTable();
            InputFile.ReadFromFile("test.csv", MetData);

            Assert.AreEqual(MetData.Rows[0]["Site"], "DALB");
            Assert.AreEqual(MetData.Rows[0]["radn"], (float)20.74);
            Assert.AreEqual(MetData.Rows[0]["maxt"], (float)33.0);
            Assert.AreEqual(MetData.Rows[0]["mint"], (float)17.4);
            Assert.AreEqual(MetData.Rows[0]["rain"], (float)0.2);
            Assert.AreEqual(MetData.Rows[0]["evap"], (float)7.41);

            File.Delete("text.csv");
        }






    }
}
