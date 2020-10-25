using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using CMPServices;

namespace outputComp
{
    
    // ============================================================================
    /// <summary>
    /// Text reporter
    /// </summary>
    // ============================================================================
    public class TTextReporter : TGenericReporter
    {
        private const int MAXCOLWIDTH = 20;
        private StreamWriter FOutFile;  //Results file 
        private int FInitColumns;       //Initial number of output columns      
        private int FLastColumnCount;   //Number of columns in the last write
        private List<int> FWidths;      //width of each column for apsim format
        private StringBuilder FHeaderfmt; //format string for apsim format

        /// <summary>
        /// Create the text reporter
        /// </summary>
        public TTextReporter()
        {
            this.FWidths = new List<int>();
            this.FHeaderfmt = new StringBuilder();
            FLastColumnCount = 0;
        }
        protected String MissingText 
        {
            get
            {
                if (ApsimFMT)
                {
                    return "?";
                }
                else
                {
                    return "#n/a";
                }
            }
        }
        // ============================================================================
        /// <summary>
        /// Write an 'empty' line to any existing file.
        /// </summary>
        public void ClearOutFile()
        {
            if ((this.FileName.Length > 0) && File.Exists(this.FileName))
            {
                this.FOutFile = new StreamWriter(this.FileName, false);
                this.FOutFile.WriteLine("- no data -");
                this.FOutFile.Close();
            }
        }

        // ============================================================================
        /// <summary>
        /// Writes the header rows to the output file. Must be done after the            
        /// FColumns structure is populated, which can only happen once variable values  
        /// are returned.
        /// </summary>
        // ============================================================================
        private void WriteHeaders()
        {
            int Idx;

            if (ApsimFMT)
            {
                RecalcApsimHdrFmt();

                String[] colArray = new String[FColumns.Count + 1];
                String[] unitsArray = new String[FColumns.Count + 1];

                colArray[0] = "Date";
                if (DateFMT.Length > 0)
                {
                    unitsArray[0] = "(" + DateFMT + ")";
                }
                else
                {
                    unitsArray[0] = "(dd/mm/yyyy)";
                }

                for (Idx = 0; Idx <= FColumns.Count - 1; Idx++)
                {
                    colArray[Idx + 1] = FColumns[Idx].Name;
                    unitsArray[Idx + 1] = "(" + FColumns[Idx].Units + ")";
                }

                FOutFile.WriteLine(FHeaderfmt.ToString(), colArray);
                FOutFile.Write(FHeaderfmt.ToString(), unitsArray);
            }
            else 
            {
                //ausfarm standard
                FOutFile.Write("Date");
                for (Idx = 0; Idx <= FColumns.Count - 1; Idx++)
                {
                    FOutFile.Write("\t" + FColumns[Idx].Name);
                }
                FOutFile.WriteLine();
                for (Idx = 0; Idx <= FColumns.Count - 1; Idx++)
                {
                    FOutFile.Write("\t" + FColumns[Idx].Units);
                }
            }
            FOutFile.WriteLine();
        }

        /// <summary>
        /// Recalculate the APSIM header format for the number of colummns in the file
        /// </summary>
        private void RecalcApsimHdrFmt()
        {
            int Idx;
            int maxwidth;

            FWidths.Clear();
            FHeaderfmt.Clear();
            FWidths.Add(MAXCOLWIDTH);
            maxwidth = MAXCOLWIDTH;
            FHeaderfmt.Append("{0," + maxwidth.ToString() + "}");
            for (Idx = 0; Idx <= FColumns.Count - 1; Idx++)
            {
                maxwidth = Math.Max(FColumns[Idx].Name.Length + 1, FColumns[Idx].Units.Length + 1);
                maxwidth = Math.Max(MAXCOLWIDTH, maxwidth);
                FWidths.Add(maxwidth);
                FHeaderfmt.Append("{" + Convert.ToString(Idx + 1) + "," + maxwidth.ToString() + "}");
            }
        }
        
        ////============================================================================
        /// <summary>
        /// Open the output file
        /// </summary>
        ////============================================================================
        public override void beginWriting()
        {
            if (!FWriting)
            {
                FileStream stream = new FileStream(FileName, FileMode.Create, FileAccess.Write, FileShare.Read);
                FOutFile = new StreamWriter(stream);
                try
                {
                    if (Title.Length > 0)
                    {
                        FOutFile.WriteLine(Title);
                    }
                    base.beginWriting();
                }
                catch (Exception excep)
                {
                    throw new ApplicationException("Cannot open output file. " + excep.Message);
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Close the output file
        /// </summary>
        //============================================================================
        public override void endWriting()
        {
            if (FWriting)
            {
                base.endWriting();
                FOutFile.Close();
                if (FColumns.Count != FInitColumns)
                {
                    SortColumns();
                }
            }
        }
        ////============================================================================
        /// <summary>
        /// Output one line of the results file 
        /// </summary>
        ////============================================================================
        public override void writeValues()
        {
            String sDateStr = "";
            String strVal;
            int Idx;
            TOutputScalar scalarItem;
            String[] colArray = new String[FColumns.Count + 1];

            if (FFirstTime)
            {
                WriteHeaders();
                FInitColumns = FColumns.Count;
                FLastColumnCount = FInitColumns;    //avoid recalc apsim format string
                FFirstTime = false;
            }
            //Write the line of output values       
            if (IntervalUnit <= TTimeValue.SEC) // Dates formatted according to ISO 8601 
            {
                sDateStr = FCurrOutputTime.asISODateTimeStr();
            }
            else
            {
                if (IntervalUnit <= TTimeValue.HR)
                {
                    sDateStr = FCurrOutputTime.asISODateTimeStr();
                }
                else
                {
                    if (ApsimFMT)
                    {
                        if (DateFMT.Length > 0)
                        {
                            sDateStr = FCurrOutputTime.asDateStrFMT(DateFMT);
                        }
                        else
                        {
                            sDateStr = FCurrOutputTime.asDateStr();
                        }
                    }
                    else
                    {
                        if (DateFMT.Length > 0)
                        {
                            sDateStr = FCurrOutputTime.asDateStrFMT(DateFMT);
                        }
                        else
                        {
                            sDateStr = FCurrOutputTime.asISODateStr();
                        }
                    }
                }
            }

            if (ApsimFMT)
            {
                colArray[0] = sDateStr;
            }
            else
                FOutFile.Write(sDateStr);
            for (Idx = 0; Idx <= FColumns.Count - 1; Idx++)
            {
                scalarItem = FColumns[Idx];
                if (scalarItem.AggregCount == 0)        // It is possible that no value has been 
                {                                       // recorded for this column (an array may have shrunk)
                    if (ApsimFMT)
                        colArray[Idx + 1] = MissingText;
                    else
                        FOutFile.Write("\t", MissingText);
                }
                else
                {
                    strVal = "";
                    // format the output value
                    switch (scalarItem.baseType)
                    {
                        case TTypedValue.TBaseType.ITYPE_INT1:
                        case TTypedValue.TBaseType.ITYPE_INT2:
                        case TTypedValue.TBaseType.ITYPE_INT4:
                        case TTypedValue.TBaseType.ITYPE_INT8: strVal = Convert.ToInt32(scalarItem.dVal).ToString();
                            break;
                        case TTypedValue.TBaseType.ITYPE_SINGLE:
                        case TTypedValue.TBaseType.ITYPE_DOUBLE:
                            {
                                if (Math.Log10(Math.Abs(scalarItem.dVal)) > 10)
                                {
                                    strVal = String.Format(CultureInfo.InvariantCulture, "{0:e" + scalarItem.decPl + "}", scalarItem.dVal);
                                }
                                else
                                {
                                    strVal = String.Format(CultureInfo.InvariantCulture, "{0:f" + scalarItem.decPl + "}", scalarItem.dVal);
                                }
                            }
                            break;
                        case TTypedValue.TBaseType.ITYPE_CHAR: strVal = scalarItem.sVal;
                            break;
                        case TTypedValue.TBaseType.ITYPE_STR: strVal = scalarItem.sVal;
                            break;
                        case TTypedValue.TBaseType.ITYPE_BOOL: strVal = scalarItem.dVal.ToString();
                            break;
                    }
                    if (ApsimFMT)
                    {
                        colArray[Idx + 1] = strVal;
                    }
                    else
                    {
                        FOutFile.Write("\t" + strVal);  // Write the current value of the column }
                    }
                }

                FColumns[Idx].Clear();      // Clear the current column values       
            }
            if (ApsimFMT)
            {
                if (FLastColumnCount != FColumns.Count)
                {
                    RecalcApsimHdrFmt(); //update the header format based on the FColumns
                    FLastColumnCount = FColumns.Count;
                }
                FOutFile.Write(FHeaderfmt.ToString(), colArray);
            }
            FOutFile.WriteLine();
        }
        ////============================================================================
        /// <summary>
        /// Get the column
        /// </summary>
        /// <param name="sLine"></param>
        /// <returns></returns>
        ////============================================================================
        private String GetColumn(ref String sLine)
        {
            string result = "";
            int iTabPos;

            iTabPos = sLine.IndexOf('\t');
            if (iTabPos >= 0)
            {
                result = sLine.Substring(0, iTabPos - 1);
                sLine = sLine.Remove(0, iTabPos + 1);
            }
            else
            {
                result = sLine;
                sLine = "";
            }
            return result;
        }

        ////============================================================================
        /// <summary>
        /// Rewrite the results file, ordering the columns in the expected manner. This  
        /// has to be done when the length of array variables increases during the       
        /// course of storing values.  
        /// </summary>
        ////============================================================================
        private void SortColumns()
        {
            List<int> iSortOrder = new List<int>();
            int iColI = 0;

            String sTempName;
            StreamReader OrigFile;
            StreamWriter FinalFile;

            if (FWriting)
            {
                throw new ApplicationException("TTextReporter: Cannot sort columns while results file is open");
            }
            else
            {
                
                for (int i = 0; i <= FOutputs.Count - 1; i++)
                {
                    AddSortedIndexes(FOutputs[i].valTree, ref iSortOrder, ref iColI);
                }

                OrigFile = new StreamReader(FileName);          //Open the original (unsorted) file and 
                sTempName = Path.GetDirectoryName(FileName) + "_" + Path.GetFileName(FileName); //Open the final (sorted) file and      
                FinalFile = new StreamWriter(sTempName);                                        //write the title and headers         

                if (!ApsimFMT)              //temporary - until sortcolumns supports apsim format ! {TODO}
                {
                    RewriteAusFarmFile(iSortOrder, OrigFile, FinalFile);
                }
                else
                {
                    RewriteAPSIMFile(iSortOrder, OrigFile, FinalFile);
                }
                OrigFile.Close();                                               // Close the files                       
                FinalFile.Close();
                File.Delete(FileName);                                          //Replace the unsorted file with the    
                File.Move(sTempName, FileName);                                 //sorted one                          
            }
        }

        ////============================================================================
        /// <summary>
        /// Read the source file and apply column sorting. Write to the destination file.
        /// </summary>
        /// <param name="iSortOrder">The order of the array items</param>
        /// <param name="OrigFile">Source file</param>
        /// <param name="FinalFile">Destination file</param>
        ////============================================================================
        private void RewriteAPSIMFile(List<int> iSortOrder, StreamReader OrigFile, StreamWriter FinalFile)
        {
            int i;
            String sLine;

            //read over the title & header lines  
            if (Title.Length > 0)
            {                                               // Assumes that Title has not changed... }
                OrigFile.ReadLine();    //apsim ver
                OrigFile.ReadLine();    //title
            }
            OrigFile.ReadLine();        //heading
            OrigFile.ReadLine();        //units

            if (Title.Length > 0)
            {
                FinalFile.WriteLine(Title);
            }

            //write the column headings
            String[] colArray = new String[FColumns.Count + 1];
            String[] unitsArray = new String[FColumns.Count + 1];

            colArray[0] = "Date";
            if (DateFMT.Length > 0)
            {
                unitsArray[0] = "(" + DateFMT + ")";
            }
            else
            {
                unitsArray[0] = "(dd/mm/yyyy)";
            }

            //write the units
            for (i = 0; i <= FColumns.Count - 1; i++)
            {
                colArray[i + 1] = FColumns[iSortOrder[i]].Name;
                unitsArray[i + 1] = "(" + FColumns[iSortOrder[i]].Units + ")";
            }

            //update the header format based on the FColumns using the iSortOrder[]
            //see RecalcApsimHdrFmt()
            int Idx;
            int maxwidth;

            FWidths.Clear();
            FHeaderfmt.Clear();
            FWidths.Add(MAXCOLWIDTH);
            maxwidth = MAXCOLWIDTH;
            FHeaderfmt.Append("{0," + maxwidth.ToString() + "}");
            for (Idx = 0; Idx <= FColumns.Count - 1; Idx++)
            {
                maxwidth = Math.Max(FColumns[iSortOrder[Idx]].Name.Length + 1, FColumns[iSortOrder[Idx]].Units.Length + 1);
                maxwidth = Math.Max(MAXCOLWIDTH, maxwidth);
                FWidths.Add(maxwidth);
                FHeaderfmt.Append("{" + Convert.ToString(Idx + 1) + "," + maxwidth.ToString() + "}");
            }

            FinalFile.WriteLine(FHeaderfmt.ToString(), colArray);
            FinalFile.Write(FHeaderfmt.ToString(), unitsArray);

            FinalFile.WriteLine();

            while (!OrigFile.EndOfStream)
            {                                                               // Work through the results file...      
                sLine = OrigFile.ReadLine();                                // ... read in each line of results...   

                string[] cols = sLine.Trim().Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);

                colArray[0] = cols[0];
                for (i = 0; i <= FColumns.Count - 1; i++)
                {
                    if (cols.Length > iSortOrder[i] + 1)
                        colArray[i + 1] = cols[iSortOrder[i] + 1];
                    else
                        colArray[i + 1] = MissingText;
                }
                FinalFile.WriteLine(FHeaderfmt.ToString(), colArray);
            }
        }

        ////============================================================================
        /// <summary>
        /// Read the source file and apply column sorting. Write to the destination file.
        /// </summary>
        /// <param name="iSortOrder">The order of the array items</param>
        /// <param name="OrigFile">Source file</param>
        /// <param name="FinalFile">Destination file</param>
        ////============================================================================
        private void RewriteAusFarmFile(List<int> iSortOrder, StreamReader OrigFile, StreamWriter FinalFile)
        {
            int i;
            List<String> ColTexts;
            String sLine;
            String sDateStr;


            //read over the title & header lines  
            if (Title.Length > 0)
            {                                               // Assumes that Title has not changed... }
                OrigFile.ReadLine();
            }
            OrigFile.ReadLine();
            OrigFile.ReadLine();

            if (Title.Length > 0)
            {
                FinalFile.WriteLine(Title);
            }

            RecalcApsimHdrFmt(); //update the header format based on the FColumns
           // FOutFile.Write(FHeaderfmt.ToString(), colArray);

            FinalFile.Write("Date");    //Column names                          
            for (i = 0; i <= FColumns.Count - 1; i++)
            {
                FinalFile.Write("\t" + FColumns[iSortOrder[i]].Name);
            }

            FinalFile.WriteLine();

            for (i = 0; i <= FColumns.Count - 1; i++)
            {                       // Column units                          
                FinalFile.Write("\t" + FColumns[iSortOrder[i]].Units);
            }

            FinalFile.WriteLine();

            ColTexts = new List<String>();                                  // Set up a list with the correct number 
            for (i = 0; i <= FColumns.Count - 1; i++)                       // of strings                           
            {
                ColTexts.Add(MissingText);
            }

            while (!OrigFile.EndOfStream)
            {                                                               // Work through the results file...      
                for (i = 0; i <= FColumns.Count - 1; i++)
                {
                    ColTexts[i] = MissingText;
                }

                sLine = OrigFile.ReadLine();                                // ... read in each line of results...   
                sDateStr = GetColumn(ref sLine);
                for (i = 0; i <= FColumns.Count - 1; i++)
                {
                    if (sLine.Length > 0)
                    {
                        ColTexts[i] = GetColumn(ref sLine);
                    }
                }

                FinalFile.Write(sDateStr);                                  // ... and write it back in sorted order }
                for (i = 0; i <= FColumns.Count - 1; i++)
                {
                    FinalFile.Write("\t" + ColTexts[iSortOrder[i]]);
                }

                FinalFile.WriteLine();
            }
        }
        ////============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="valTree"></param>
        /// <param name="sortOrder"></param>
        /// <param name="colI"></param>
        ////============================================================================
        private void AddSortedIndexes(TValTree valTree, ref List<int> sortOrder, ref int colI)
        {
            if (valTree.colNumber >= 0)
            {
                sortOrder.Add(valTree.colNumber);
                colI++;
            }
            for (int i = 0; i <= valTree.subTrees.Count - 1; i++)
            {
                AddSortedIndexes(valTree.subTrees[i], ref sortOrder, ref colI);
            }
        }
    }
}
