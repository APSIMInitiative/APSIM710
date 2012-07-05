using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using CMPServices;

namespace outputComp
{
    //============================================================================
    /// <summary>
    /// 
    /// </summary>
    //============================================================================
    public class TTextReporter : TGenericReporter
    {
        static String MISSINGTEXT = "#n/a";

        private StreamWriter FOutFile;  //Results file 
        private int FInitColumns;       //Initial number of output columns      
        private List<int> FWidths;      //width of each column for apsim format
        private StringBuilder FHeaderfmt; //format string for apsim format


        public TTextReporter()
        {
            FWidths = new List<int>();
            FHeaderfmt = new StringBuilder();
        }
        //============================================================================
        /// <summary>
        /// Writes the header rows to the output file. Must be done after the            
        /// FColumns structure is populated, which can only happen once variable values  
        /// are returned.
        /// </summary>
        //============================================================================
        private void WriteHeaders()
        {
            int Idx;
            int maxwidth;
            String[] colArray = new String[FColumns.Count + 1];
            String[] unitsArray = new String[FColumns.Count + 1];

            if (ApsimFMT)
            {
                maxwidth = 15;
                FWidths.Add(maxwidth);
                FHeaderfmt.Append("{0,15}");
                colArray[0] = "Date";
                unitsArray[0] = "(dd/mm/yyyy)";
                for (Idx = 0; Idx <= FColumns.Count - 1; Idx++)
                {
                    maxwidth = Math.Max(FColumns[Idx].Name.Length + 1, FColumns[Idx].Units.Length + 1);
                    maxwidth = Math.Max(15, maxwidth);
                    FWidths.Add(maxwidth);
                    FHeaderfmt.Append("{" + Convert.ToString(Idx + 1) + "," + maxwidth.ToString() + "}");
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
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        //============================================================================
        public override void beginWriting()
        {
            if (!FWriting)
            {
                FOutFile = new StreamWriter(FileName);
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
                    throw (new ApplicationException("Cannot open output file. " + excep.Message));
                }
            }
        }
        //============================================================================
        /// <summary>
        /// 
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
                    if (!ApsimFMT)              //temporary - until sortcolumns supports apsim format ! {TODO}
                        SortColumns();
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Output one line of the results file 
        /// </summary>
        //============================================================================
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
                FFirstTime = false;
            }
            //Write the line of output values       
            if (IntervalUnit <= TTimeValue.SEC) //Dates formatted according to ISO 8601 
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
                        sDateStr = FCurrOutputTime.asDateStr();
                    else
                        sDateStr = FCurrOutputTime.asISODateStr();
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
                if (scalarItem.AggregCount == 0)        //It is possible that no value has been 
                {                                       //recorded for this column (an array may have shrunk)
                    if (ApsimFMT)
                        colArray[Idx + 1] = MISSINGTEXT;
                    else
                        FOutFile.Write("\t", MISSINGTEXT);
                }
                else
                {
                    strVal = "";
                    //format the output value
                    switch (scalarItem.baseType)
                    {
                        case TTypedValue.TBaseType.ITYPE_INT1:
                        case TTypedValue.TBaseType.ITYPE_INT2:
                        case TTypedValue.TBaseType.ITYPE_INT4:
                        case TTypedValue.TBaseType.ITYPE_INT8: strVal = Convert.ToInt32(scalarItem.dVal).ToString();
                            break;
                        case TTypedValue.TBaseType.ITYPE_SINGLE:
                        case TTypedValue.TBaseType.ITYPE_DOUBLE: strVal = String.Format("{0:f" + scalarItem.decPl + "}", scalarItem.dVal);
                            break;
                        case TTypedValue.TBaseType.ITYPE_CHAR: strVal = scalarItem.sVal;
                            break;
                        case TTypedValue.TBaseType.ITYPE_STR: strVal = scalarItem.sVal;
                            break;
                        case TTypedValue.TBaseType.ITYPE_BOOL: strVal = scalarItem.dVal.ToString();
                            break;
                    }
                    if (ApsimFMT)
                        colArray[Idx + 1] = strVal;
                    else
                        FOutFile.Write("\t" + strVal);  //Write the current value of the column }
                }

                FColumns[Idx].Clear();      //Clear the current column values       
            }
            if (ApsimFMT)
                FOutFile.Write(FHeaderfmt.ToString(), colArray);
            FOutFile.WriteLine();
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sLine"></param>
        /// <returns></returns>
        //============================================================================
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
        //============================================================================
        /// <summary>
        /// Rewrite the results file, ordering the columns in the expected manner. This  
        /// has to be done when the length of array variables increases during the       
        /// course of storing values.  
        /// </summary>
        //============================================================================
        private void SortColumns()
        {
            List<int> iSortOrder = new List<int>();
            int iColI = 0;

            String sTempName;
            StreamReader OrigFile;
            StreamWriter FinalFile;
            List<String> ColTexts;
            String sLine;
            String sDateStr;

            int i;

            if (FWriting)
            {
                throw (new ApplicationException("TTextReporter: Cannot sort columns while results file is open"));
            }
            else
            {
                for (i = 0; i <= FOutputs.Count - 1; i++)
                {
                    AddSortedIndexes(FOutputs[i].valTree, ref iSortOrder, ref iColI);
                }
                
                OrigFile = new StreamReader(FileName);          //Open the original (unsorted) file and 
                                                                //read over the title & header lines  
                if (Title.Length > 0)
                {                                               // Assumes that Title has not changed... }
                    OrigFile.ReadLine();
                }
                OrigFile.ReadLine();
                OrigFile.ReadLine();

                sTempName = Path.GetDirectoryName(FileName) + "_" + Path.GetFileName(FileName); //Open the final (sorted) file and      
                FinalFile = new StreamWriter(sTempName);                                        //write the title and headers         

                if (Title.Length > 0)
                {
                    FinalFile.WriteLine(Title);
                }

                FinalFile.Write("Date");    //Column names                          
                for (i = 0; i <= FColumns.Count - 1; i++)
                {
                    FinalFile.Write("\t" + FColumns[iSortOrder[i]].Name);
                }
                FinalFile.WriteLine();

                for (i = 0; i <= FColumns.Count-1; i++) {                       //Column units                          
                  FinalFile.Write("\t" + FColumns[ iSortOrder[i] ].Units);
                }
                FinalFile.WriteLine();

                ColTexts = new List<String>();                                  //Set up a list with the correct number 
                for (i = 0; i <= FColumns.Count - 1; i++)                       //of strings                           
                {                                     
                    ColTexts.Add(MISSINGTEXT);
                }

                while (!OrigFile.EndOfStream)
                {                                                               //Work through the results file...      
                    for (i = 0; i <= FColumns.Count - 1; i++)
                    {
                        ColTexts[i] = MISSINGTEXT;
                    }

                    sLine = OrigFile.ReadLine();                                //... read in each line of results...   
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

                OrigFile.Close();                                               // Close the files                       }
                FinalFile.Close();
                File.Delete(FileName);                                          //Replace the unsorted file with the    }
                File.Move(sTempName, FileName);                                 //sorted one                          }
            }
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="valTree"></param>
        /// <param name="iSortOrder"></param>
        /// <param name="iColI"></param>
        //============================================================================
        private void AddSortedIndexes(TValTree valTree, ref List<int> iSortOrder, ref int iColI)
        {
            if (valTree.colNumber >= 0)
            {
                iSortOrder[iColI] = valTree.colNumber;
                iColI++;
            }
            for (int i = 0; i <= valTree.subTrees.Count - 1; i++)
            {
                AddSortedIndexes(valTree.subTrees[i], ref iSortOrder, ref iColI);
            }

        }
    }
}
