
using System;
using System.Collections.Specialized;
using System.Data;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;
using System.Windows.Forms;

//using Controllers;
using CSGeneral;


namespace UIUtility
   {

   public class DataProcessor
      {
      // -----------------------------------------------------------------------
      // The APSIMData passed into this class' constructor should look like:
      //    <Data>
      //       <ApsimFileReader>
      //          <FileName type="filenames">continuous wheat simulation.out</FileName>
      //          <ParseTitle type="yesno">no</ParseTitle>
      //       </ApsimFileReader>
      //    </Data>
      // For those methods that require a path it should NOT include the root node name
      //    e.g. ApsimFileReader and NOT Data\ApsimFileReader
      // -----------------------------------------------------------------------

      private StringBuilder contents = new StringBuilder(500000);
      private UInt32 DataContainer = 0;
      private bool WeCreatedDataContainer = false;

      #region Imports from external DLL's

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern UInt32 Create();

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void Destroy(UInt32 DataContainer);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void Setup(UInt32 DataContainer, string Xml);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void Set(UInt32 DataContainer, string Xml);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void SetWithNoRefresh(UInt32 DataContainer, string Xml);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void Add(UInt32 DataContainer, string Xml);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void Erase(UInt32 DataContainer, string Name);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void Rename(UInt32 DataContainer, string OldName, string NewName);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void ErrorMessage(UInt32 DataContainer, StringBuilder ErrorMessage);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void FieldNames(UInt32 DataContainer, string Name, StringBuilder FieldNames);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern UInt32 CreateDataForm(IntPtr ParentHandle);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void DeleteDataForm(UInt32 Handle);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern int GetHandleOfDataForm(UInt32 FormHandle);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void FillDataFormWithData(UInt32 FormHandle, UInt32 DataContainer, string Name);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void GetData(UInt32 DataContainer, string Name, string x, byte[] Data);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void Xml(UInt32 DataContainer, StringBuilder Xml);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void REMSExperimentNames(UInt32 DataContainer, String FileName, StringBuilder ExperimentNames);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void REMSTreatmentNames(UInt32 DataContainer, String FileName, String ExperimentName, StringBuilder TreatmentNames);

      [DllImport("segreport.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern void DataSetNames(UInt32 DataContainer, StringBuilder Names);

      [DllImport("user32.dll", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.StdCall)]
      private static extern bool MoveWindow(
          int hWnd,	// handle of window
          int X,	// horizontal position
          int Y,	// vertical position
          int nWidth,	// width
          int nHeight,	// height
          bool bRepaint 	// repaint flag
         );
      #endregion

      #region Constructor / destructor / setup
      public DataProcessor()
         {
         // --------------------------------------
         // constructor
         // --------------------------------------
         WeCreatedDataContainer = true;
         DataContainer = Create();
         }
      public DataProcessor(UInt32 HandleOfExisting)
         {
         // --------------------------------------
         // constructor
         // --------------------------------------
         WeCreatedDataContainer = false;
         DataContainer = HandleOfExisting;
         }
      public void Shutdown()
         {
         if (WeCreatedDataContainer)
            Destroy(DataContainer);
         }
      #endregion

      #region Data methods

      public bool FromApsimReport
         {
         get { return !WeCreatedDataContainer; }
         }
      public string ErrorMessage()
         {
         // -----------------------------------------------------
         // Return any error message for the data component as
         // specified by full path.
         // -----------------------------------------------------
         ErrorMessage(DataContainer, contents);
         return contents.ToString();
         }
      public void Set(string XML)
         {
         // -----------------------------------------------------
         // Set the properties for the data component as specified
         // by path.
         // -----------------------------------------------------
         Set(DataContainer, XML);
         }
      public void SetNoRefresh(string XML)
         {
         // -----------------------------------------------------
         // Set the properties for the data component as specified
         // by path.
         // -----------------------------------------------------
         SetWithNoRefresh(DataContainer, XML);
         }
      public void Add(string XML)
         {
         // -----------------------------------------------------
         // Add a new component with the specified properties 
         // -----------------------------------------------------
         Add(DataContainer, XML);
         }
      public void Erase(string Name)
         {
         // -----------------------------------------------------
         // erase a component
         // -----------------------------------------------------
         Erase(DataContainer, Name);
         }
      public void Rename(string OldName, string NewName)
         {
         // -----------------------------------------------------
         // Rename a component
         // -----------------------------------------------------
         Rename(DataContainer, OldName, NewName);
         }
      public string XML()
         {
         Xml(DataContainer, contents);
         return contents.ToString();
         }
      public string[] DataSetNames()
         {
         DataSetNames(DataContainer, contents);
         return contents.ToString().Split("\t".ToCharArray());
         }
      public string[] GetFieldNamesForDataSet(string Name)
         {
         // ------------------------------------------------------
         // Return a list of all field names for specified dataset
         // ------------------------------------------------------
         FieldNames(DataContainer, Name, contents);
         if (contents.Length == 0)
            return new string[0];
         else
            return contents.ToString().Split("\t".ToCharArray());
         }
      public void GetData(string Name, string ColumnName, DataTable Data)
         {
         // ------------------------------------------------------
         // Add a column of data to the specified datatable
         // ------------------------------------------------------
         byte[] ByteStream = new byte[5000000];
         GetData(DataContainer, Name, ColumnName, ByteStream);

         MemoryStream Mem = new MemoryStream(ByteStream);
         BinaryReader In = new BinaryReader(Mem);

         FillDataTable(Data, ColumnName, In);
         }
      private void FillDataTable(DataTable Data, string ColumnName, BinaryReader In)
         {
         // ----------------------------------------------------------
         // Internal method for extracting a column of numbers/strings
         // from a byte stream.
         // ----------------------------------------------------------
         int DataType = In.ReadInt32();
         if (DataType == 1)
            Data.Columns.Add(ColumnName, typeof(float));
         else if (DataType == 2)
            Data.Columns.Add(ColumnName, typeof(DateTime));
         else
            Data.Columns.Add(ColumnName, typeof(string));
         int NumValues = In.ReadInt32();

         // Make sure there are enough values in the table.
         while (Data.Rows.Count < NumValues)
            Data.Rows.Add(Data.NewRow());

         for (int Row = 0; Row != NumValues; Row++)
            {
            if (DataType == 1)
               {
               float Value = In.ReadSingle();
               if (Value != MathUtility.MissingValue)
                  Data.Rows[Row][ColumnName] = Value;
               }
            else if (DataType == 2)
               {
               Int16 Year = In.ReadInt16();
               Int16 Month = In.ReadInt16();
               Int16 Day = In.ReadInt16();
               Data.Rows[Row][ColumnName] = new DateTime(Year, Month, Day);
               }
            else
               {
               string Value = In.ReadString();
               Data.Rows[Row][ColumnName] = Value;
               }
            }
         }
      #endregion

      #region Data window methods
      public UInt32 CreateDataWindow(IntPtr ParentHandle)
         {
         // ------------------------------------------------
         // Create an empty data window control parented to
         // the specified handle.
         // ------------------------------------------------
         return CreateDataForm(ParentHandle);
         }
      public void DeleteDataWindow(UInt32 DataWindow)
         {
         // ------------------------------------------------
         // Delete the specified data window control
         // ------------------------------------------------

         DeleteDataForm(DataWindow);
         }
      public void SizeDataWindow(UInt32 DataWindow, int Left, int Right, int Width, int Height)
         {
         // ------------------------------------------------
         // Tell windows to resize the specified data window
         // ------------------------------------------------
         MoveWindow(GetHandleOfDataForm(DataWindow), Left, Right, Width, Height, true);
         }
      public void RefreshDataWindow(UInt32 DataWindow, string Name)
         {
         // ------------------------------------------------
         // Refresh the data in the specified data window.
         // ------------------------------------------------
         FillDataFormWithData(DataWindow, DataContainer, Name);
         }
      #endregion

      #region REMS
      public string[] GetExperimentNames(string FileName)
         {
         REMSExperimentNames(DataContainer, FileName, contents);
         return contents.ToString().Split("\t".ToCharArray());
         }
      public string[] GetTreatmentNames(string FileName, string ExperimentName)
         {
         REMSTreatmentNames(DataContainer, FileName, ExperimentName, contents);
         return contents.ToString().Split("\t".ToCharArray());
         }
      #endregion
      }
   }

