using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using NUnit.Framework;
using System.Data;

[TestFixture]
public class TestApsimFileReader
   {
   [SetUp] public void Setup()
      {
      StreamWriter Out = new StreamWriter("test1.out");
      Out.WriteLine("Title = Test file1");
      Out.WriteLine("col1 col2 col3");
      Out.WriteLine("  ()   ()   ()");
      Out.WriteLine("  10    ?   20");
      Out.WriteLine("  11    5   21");
      Out.WriteLine("  ?     6   22");
      Out.Close();

      Out = new StreamWriter("test2.out");
      Out.WriteLine("Title = Test file2");
      Out.WriteLine("year col2 col3 col4");
      Out.WriteLine("  ()   ()   ()   ()");
      Out.WriteLine(" 1989 215 320  400");
      Out.WriteLine(" 1990 216 321  410");
      Out.WriteLine(" 1991 214 322  420");
      Out.Close();

      Out = new StreamWriter("test3.out");
      Out.WriteLine("Title = Test file3");
      Out.WriteLine("date      dlayer(1) dlayer(2) dlayer(3) sw(1) sw(2) sw(3)");
      Out.WriteLine("  ()         ()         ()        ()      ()    ()     ()");
      Out.WriteLine("1/1/2000    100        300       300      10    15     20");
      Out.WriteLine("2/1/2000    100        300       300      11    16     21");
      Out.WriteLine("3/1/2000    100        300       300      12    17     22");
      Out.Close();

      }

   [TearDown] public void TearDown()
      {
      File.Delete("test1.out");
      File.Delete("test2.out");
      File.Delete("test3.out");
      }

   [Test] public void TestFileReader()
      {
      // --------------------------------------------------------------------
      // Make sure we can read in 2 output files where the output files
      // have different columns.
      // --------------------------------------------------------------------
      const string Xml = "<GDApsimFileReader>" +
                         "   <FileName>test1.out</FileName>" +
                         "   <FileName>test2.out</FileName>" +
                         "</GDApsimFileReader>";

      DataProcessor Processor = new DataProcessor();
      DataTable Data = Processor.Go(Xml);
      Assert.AreEqual(Data.Columns.Count, 6);
      Assert.AreEqual(Data.Rows.Count, 6);

      // First row
      Assert.AreEqual(Convert.ToDouble(Data.Rows[0][0]), 10);            // col 1
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[0][1]));                  // col 2          
      Assert.AreEqual(Convert.ToDouble(Data.Rows[0][2]), 20);            // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[0][3]), "Test file1");  // title
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[1][4]));                  // year          
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[1][5]));                  // col 4          

      // Second row
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][0]), 11);            // col 1
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][1]), 5);             // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][2]), 21);            // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[1][3]), "Test file1");  // title
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[1][4]));                  // year          
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[1][5]));                  // col 4          

      // Third row
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[2][0]));                  // col 1
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][1]), 6);             // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][2]), 22);            // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[2][3]), "Test file1");  // title
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[2][4]));                  // year          
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[2][5]));                  // col 4          

      // 4th row
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[3][0]));                  // col 1
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][1]), 215);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][2]), 320);           // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[3][3]), "Test file2");  // title
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][4]), 1989);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][5]), 400);           // col 4

      // 5th row
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[4][0]));                  // col 1
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][1]), 216);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][2]), 321);           // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[4][3]), "Test file2");  // title
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][4]), 1990);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][5]), 410);           // col 4

      // 6th row
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[5][0]));                  // col 1
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][1]), 214);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][2]), 322);           // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[5][3]), "Test file2");  // title
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][4]), 1991);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][5]), 420);           // col 4

      // Make sure second row is of type single.
      Assert.AreEqual(Data.Columns[1].DataType.ToString(), "System.Single");
      }
   [Test] public void TestProbability()
      {
      // --------------------------------------------------------------------
      // Make sure we can do a probability distribution on 2 output files.
      // --------------------------------------------------------------------
      const string Xml = "<GDProbability>" +
                         "   <GDApsimFileReader>" +
                         "      <FileName>test1.out</FileName>" +
                         "      <FileName>test2.out</FileName>" +
                         "   </GDApsimFileReader>" +
                         "</GDProbability>";

      DataProcessor Processor = new DataProcessor();
      DataTable Data = Processor.Go(Xml);
      Assert.AreEqual(Data.Columns.Count, 7);
      Assert.AreEqual(Data.Rows.Count, 6);

      // First row
      Assert.AreEqual(Convert.ToInt32(Data.Rows[0][0]), 17);             // probability
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[0][1]));                  // col 1
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[0][2]));                  // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[0][3]), 20);            // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[0][4]), "Test file1");  // title
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[0][5]));                  // year
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[0][6]));                  // col 4

      // Second row
      Assert.AreEqual(Convert.ToInt32(Data.Rows[1][0]), 50);             // probability
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][1]), 10);            // col 1
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][2]), 5);             // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][3]), 21);            // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[1][4]), "Test file1");  // title
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[1][5]));                  // year
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[1][6]));                  // col 4

      // Third row
      Assert.AreEqual(Convert.ToInt32(Data.Rows[2][0]), 83);             // probability
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][1]), 11);            // col 1
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][2]), 6);             // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][3]), 22);            // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[2][4]), "Test file1");  // title
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[2][5]));                  // year
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[2][6]));                  // col 4

      // 4th row
      Assert.AreEqual(Convert.ToInt32(Data.Rows[3][0]), 17);             // probability
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[3][1]));                  // col 1
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][2]), 214);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][3]), 320);           // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[3][4]), "Test file2");  // title
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][5]), 1989);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][6]), 400);           // col 4

      // 5th row
      Assert.AreEqual(Convert.ToInt32(Data.Rows[4][0]), 50);             // probability
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[4][1]));                  // col 1
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][2]), 215);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][3]), 321);           // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[4][4]), "Test file2");  // title
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][5]), 1990);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][6]), 410);           // col 4

      // 6th row
      Assert.AreEqual(Convert.ToInt32(Data.Rows[5][0]), 83);             // probability
      Assert.IsTrue(Convert.IsDBNull(Data.Rows[5][1]));                  // col 1
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][2]), 216);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][3]), 322);           // col 3
      Assert.AreEqual(Convert.ToString(Data.Rows[5][4]), "Test file2");  // title
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][5]), 1991);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][6]), 420);           // col 4
      }
   [Test] public void TestSOI()
      {
      // --------------------------------------------------------------------
      // Make sure we can add an SOI column to a dataset.
      // --------------------------------------------------------------------
      const string Xml = "<GDSOI>" +
                         "   <Month>2</Month>" +
                         "   <GDApsimFileReader>" +
                         "      <FileName>test2.out</FileName>" +
                         "   </GDApsimFileReader>" +
                         "</GDSOI>";

      DataProcessor Processor = new DataProcessor();
      DataTable Data = Processor.Go(Xml);
      Assert.AreEqual(Data.Columns.Count, 6);
      Assert.AreEqual(Data.Rows.Count, 6);

      // 1st row
      Assert.AreEqual(Convert.ToDouble(Data.Rows[0][0]), 1989);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[0][1]), 215);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[0][2]), 320);           // col 3
      Assert.AreEqual(Convert.ToDouble(Data.Rows[0][3]), 400);           // col 4
      Assert.AreEqual(Convert.ToString(Data.Rows[0][4]), "Test file2, Positive");  // title
      Assert.AreEqual(Convert.ToString(Data.Rows[0][5]), "Positive");    // soiphase

      // 2nd row
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][0]), 1990);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][1]), 216);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][2]), 321);           // col 3
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][3]), 410);           // col 4
      Assert.AreEqual(Convert.ToString(Data.Rows[1][4]), "Test file2, Falling");  // title
      Assert.AreEqual(Convert.ToString(Data.Rows[1][5]), "Falling");    // soiphase

      // 3rd row
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][0]), 1991);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][1]), 214);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][2]), 322);           // col 3
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][3]), 420);           // col 4
      Assert.AreEqual(Convert.ToString(Data.Rows[2][4]), "Test file2, Zero");  // title
      Assert.AreEqual(Convert.ToString(Data.Rows[2][5]), "Zero");    // soiphase

      // 4th row
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][0]), 1989);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][1]), 215);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][2]), 320);           // col 3
      Assert.AreEqual(Convert.ToDouble(Data.Rows[3][3]), 400);           // col 4
      Assert.AreEqual(Convert.ToString(Data.Rows[3][4]), "Test file2, AllYears");  // title
      Assert.AreEqual(Convert.ToString(Data.Rows[3][5]), "AllYears");    // soiphase

      // 5th row
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][0]), 1990);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][1]), 216);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][2]), 321);           // col 3
      Assert.AreEqual(Convert.ToDouble(Data.Rows[4][3]), 410);           // col 4
      Assert.AreEqual(Convert.ToString(Data.Rows[4][4]), "Test file2, AllYears");  // title
      Assert.AreEqual(Convert.ToString(Data.Rows[4][5]), "AllYears");    // soiphase

      // 6th row
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][0]), 1991);          // year
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][1]), 214);           // col 2
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][2]), 322);           // col 3
      Assert.AreEqual(Convert.ToDouble(Data.Rows[5][3]), 420);           // col 4
      Assert.AreEqual(Convert.ToString(Data.Rows[5][4]), "Test file2, AllYears");  // title
      Assert.AreEqual(Convert.ToString(Data.Rows[5][5]), "AllYears");    // soiphase

      }
   [Test] public void TestDepth()
      {
      // --------------------------------------------------------------------
      // Make sure we can read in 2 output files where the output files
      // have different columns.
      // --------------------------------------------------------------------
      const string Xml = "<GDDepth>" +
                         "  <GDApsimFileReader>" +
                         "   <FileName>test3.out</FileName>" +
                         "  </GDApsimFileReader>" +
                         "  <Date>2/1/2000</Date>" +
                         "</GDDepth>";

      DataProcessor Processor = new DataProcessor();
      DataTable Data = Processor.Go(Xml);
      Assert.AreEqual(Data.Columns.Count, 4);
      Assert.AreEqual(Data.Rows.Count, 3);

      // First row
      Assert.AreEqual(Convert.ToString(Data.Rows[0][0]), "2/01/2000 12:00:00 AM");  // date
      Assert.AreEqual(Convert.ToDouble(Data.Rows[0][1]), 50);          // depth
      Assert.AreEqual(Convert.ToDouble(Data.Rows[0][2]), 11);          // sw
      Assert.AreEqual(Convert.ToString(Data.Rows[0][3]), "Test file3, 2/01/2000");  // title

      Assert.AreEqual(Convert.ToString(Data.Rows[1][0]), "2/01/2000 12:00:00 AM");  // date
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][1]), 250);       // depth
      Assert.AreEqual(Convert.ToDouble(Data.Rows[1][2]), 16);        // sw
      Assert.AreEqual(Convert.ToString(Data.Rows[1][3]), "Test file3, 2/01/2000");  // title

      Assert.AreEqual(Convert.ToString(Data.Rows[2][0]), "2/01/2000 12:00:00 AM");  // date
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][1]), 550);       // depth
      Assert.AreEqual(Convert.ToDouble(Data.Rows[2][2]), 21);        // sw
      Assert.AreEqual(Convert.ToString(Data.Rows[2][3]), "Test file3, 2/01/2000");  // title
      }

   }
