using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using CMPServices;

namespace outputComp
{
    //============================================================================
    /// <summary>
    /// The wrapper class for the TextOut component.
    /// </summary>
    //============================================================================
    [ComVisible(true)]
    public class TextOutInstance : TBaseComp
    {
        public const string typeSUMMARYWRITE = "<type name=\"SummaryFileWrite\">"
                                             + "<field name=\"componentName\" kind=\"string\"/>"
                                             + "<field name=\"lines\" kind=\"string\"/>"
                                             + "</type>";
        //property ID's
        public const int drvTIME = 1;
        
        public const int prpFILE        =  PROP_START_INDEX;
        public const int prpINTV        =  PROP_START_INDEX + 1;
        public const int prpUNIT        =  PROP_START_INDEX + 2;
        public const int prpOUTS        =  PROP_START_INDEX + 3;
        public const int prpSUMMARYFILE =  PROP_START_INDEX + 4;
        public const int drvOUT_OFS     =  PROP_START_INDEX + 5;
        public const int prpAPSIMFMT    =  PROP_START_INDEX + 6;
        public const int prpTITLE       =  PROP_START_INDEX + 7;
        public const int prpOUTFREQ     =  PROP_START_INDEX + 8;
        public const int prpDATEFMT      = PROP_START_INDEX + 9; 
        
        //event ID's
        public const int evtEXEC    = 1;
        public const int stateREQS  = 1;
        public const int stateQUERY = 2;
        public const int stateSTORE = 3;

        public const int evtSUMMARYWRITE = 2;
        public const int stateDOWRITE = 1;

        //starting index for extra write events
        private const int evtEXTRAWRITE = 3;

        private static String _STYPE = "TextOut";
        private static String _SVERSION = "1.0";
        private static String _SAUTHOR = "CSIRO Plant Industry";

        protected TGenericReporter FReporter;
        protected List<String>FRequests;
        protected TTimeStep FTimeStep;
        protected Boolean FFirstTime;
        protected StreamWriter FSummaryStream;
        protected List<int> WriteEventIds;
        private Dictionary<int, String> RegExtraEvents; //extra event handlers

        protected static bool isWaterCastSim = false;

        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="compID"></param>
        /// <param name="parentCompID"></param>
        /// <param name="messageCallback"></param>
        //============================================================================
        public TextOutInstance(uint compID, uint parentCompID, MessageFromLogic messageCallback)
            : base(compID, parentCompID, messageCallback, _STYPE, _SVERSION, _SAUTHOR)
        {
            if (isWaterCastSim)
            {
                FReporter = new TStaticReporter();
            }
            else
            {
                FReporter = new TTextReporter();
            }
            FRequests = new List<string>();
            FTimeStep = new TTimeStep();
            FFirstTime = true;
            FSummaryStream = null;
            WriteEventIds = new List<int>();
            RegExtraEvents = new Dictionary<int, string>();

            TInitValue newProperty;

            //addDriver(sName,  ID,      lMinConn, lMaxConn, sUnit, bIsArray, sType)
            addDriver("time", drvTIME, 1, 1, "-", false, TTimeStep.typeTIMESTEP, "", "", 0);

            //addProperty(              sName,         ID,             bRead,bWrite,bInit, sUnit,bIsArray,sType)
            addProperty("title", prpTITLE, true, true, true, "-", false, TTypedValue.STYPE_STR, "Title of the file", "");
            addProperty("filename", prpFILE, true, true, true, "-", false, TTypedValue.STYPE_STR, "Output file", "Full path of the output file");
            addProperty("interval", prpINTV, true, false, true, "-", false, TTypedValue.STYPE_INT4, "Logging interval", "");
            setPropertyRange(prpINTV, 1, 1, 365);

            newProperty = addProperty("intervalunit", prpUNIT, true, false, true, "-", false, TTypedValue.STYPE_STR, "", "");
            newProperty.setDefault("day");

            newProperty = addProperty("outputs", prpOUTS, true, false, true, "-", false, TGenericReporter.typeOUTPUTS, "", "");
            newProperty.item(0).member("decplaces").setValue(2);

            addProperty("summary_file", prpSUMMARYFILE, true, true, true, "-", false, TTypedValue.STYPE_STR, "Summary file name", "");
            addProperty("apsim_format", prpAPSIMFMT, true, true, true, "-", false, TTypedValue.STYPE_BOOL, "Use an APSIM output format", "");
            addProperty("outputfrequency", prpOUTFREQ, true, true, true, "-", true, TTypedValue.STYPE_STR, "Report when these events are received", "");
            addProperty("dateformat", prpDATEFMT, true, true, true, "-", false, TTypedValue.STYPE_STR, "Date format string", "Use: dd/mm/yyyy etc. Ignored if empty string."); 

            addWritingEvent(evtEXEC, "update_outputs");
        }
        //============================================================================
        /// <summary>
        /// Add a standard event handler for writing a line to the output.
        /// This functionality is in this function so that more than one event
        /// handler can be registered and uses the same state logic.
        /// </summary>
        /// <param name="eventId"></param>
        /// <param name="eventName"></param>
        //============================================================================
        private void addWritingEvent(int eventId, String eventName)
        {
            // Transition diagram is:
            //   REQS ->QUERY -> STORE
            //addEvent( sName,   ID,      iKind,          sType, destID)
            addEvent(eventName, eventId, TypeSpec.KIND_SUBSCRIBEDEVENT, TypeSpec.TYPEEMPTY, "", "", 0);
            defineEventState(eventId, stateREQS, TStateMachine.LOGIC);
            defineEventState(eventId, stateQUERY, TStateMachine.NONLOGIC);
            defineEventState(eventId, stateSTORE, TStateMachine.LOGIC);
            defineEventTransition(eventId, TStateMachine.IDLE, 0, stateREQS, true);
            defineEventTransition(eventId, stateREQS, 0, stateQUERY, false);
            defineEventTransition(eventId, stateQUERY, 0, stateSTORE, false);
            defineEventTransition(eventId, stateSTORE, 0, TStateMachine.DONE, false);
            defineEventTransition(eventId, stateREQS, 1, TStateMachine.DONE, false); // For aborting when no file was specified
        }
        //============================================================================
        /// <summary>
        /// Run a WaterCast Simulation
        /// </summary>
        public static void SetWaterCastModel()
        {
            isWaterCastSim = true;
        }

        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="iStage"></param>
        //============================================================================
        public override void initialise(int iStage)
        {
            if (iStage == 1)
            { }
            if (iStage == 2)
            {
                //ensure that the file has no data carried over from last run
                if (FReporter.GetType() == typeof(TTextReporter))
                    ((TTextReporter)FReporter).ClearOutFile();
            }
        }
        //==========================================================================
        /// <summary>
        /// 
        /// </summary>
        //==========================================================================
        public override void terminate()
        {
            FReporter.EndWriting();
            base.terminate();
        }
        //==========================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="propertyID"></param>
        /// <param name="aValue"></param>
        //==========================================================================
        public override void initProperty(int propertyID, TTypedValue aValue)
        {
            String sText;
            uint Idx;

            if (!(FReporter is TStaticReporter) && (isWaterCastSim))
            {
                FReporter = null;
                
                FReporter = new TStaticReporter();
            }

            switch (propertyID)
            {
                case prpFILE:
                    {
                        FReporter.FileName = aValue.asString();
                    } break;
                case prpINTV:
                    {
                        FReporter.ReportInterval = aValue.asInteger();
                    } break;
                case prpUNIT:
                    {
                        sText = aValue.asString().ToLower().Trim();
                        for (int i = TTimeValue.SEC; i <= TTimeValue.YR; i++)
                        {
                            if (sText == TTimeValue.TIMETEXTS[i - 1])
                            {
                                FReporter.IntervalUnit = i;
                            }
                        }
                    } break;
                case prpOUTS:
                    {
                        for (Idx = 1; Idx <= aValue.count(); Idx++)
                        {
                            FReporter.addVariable(aValue.item(Idx).member("varname").asString(),
                                                   aValue.item(Idx).member("alias").asString(),
                                                   aValue.item(Idx).member("decplaces").asInteger(),
                                                   FReporter.parseAggreg(aValue.item(Idx).member("aggreg").asString()));
                        }
                    } break;
                case prpSUMMARYFILE:
                    {
                        if (aValue.asString().Length == 0)
                        {
                            FSummaryStream = null;
                        }
                        else
                        {
                            FSummaryStream = new StreamWriter(aValue.asString());
                            addEvent("summaryFileWrite", evtSUMMARYWRITE, TypeSpec.KIND_SUBSCRIBEDEVENT, typeSUMMARYWRITE, "", "", 0);
                            defineEventState(evtSUMMARYWRITE, stateDOWRITE, TStateMachine.NONLOGIC);
                            defineEventTransition(evtSUMMARYWRITE, stateDOWRITE, 0, TStateMachine.DONE, false);
                        }
                    } break;
                case prpAPSIMFMT:
                    {
                        FReporter.ApsimFMT = aValue.asBool();
                    } break;
                case prpTITLE:
                    {
                        FReporter.Title = aValue.asStr();
                    } break;
                case prpOUTFREQ:
                    {
                        uint count = aValue.count();
                        for (uint i = 1; i <= count; i++)
                        {
                            //for each new event; add it to the list and also register it
                            String eventName = aValue.item(i).asStr().ToLower();
                            if (!RegExtraEvents.ContainsValue(eventName))
                            {
                                int eventId = evtEXTRAWRITE + RegExtraEvents.Count;
                                addWritingEvent(eventId, eventName);    
                                RegExtraEvents.Add(eventId, eventName);
                            }
                        }
                    } break;
                case prpDATEFMT:
                    {
                        FReporter.DateFMT = aValue.asStr();
                    }
                    break;
                default: throw (new ApplicationException("Invalid ID code in initProperty()"));
            }

            if ((propertyID == prpFILE) || (propertyID == prpINTV)    //Store the initial value for later reading
               || (propertyID == prpUNIT) || (propertyID == prpOUTS)
                || (propertyID == prpSUMMARYFILE) )
            {
                propertyList[propertyID].setValue(aValue);
            }
        }
        //==========================================================================
        /// <summary>
        /// Returns the current value of owned properties
        /// </summary>
        //==========================================================================
        public override void readProperty(int propertyID, uint requestorID, ref TPropertyInfo aValue)
        {

            TMainOutputSpecifier outputItem;
            uint i;

            switch (propertyID)
            {
                case prpFILE:
                    {
                        aValue.setValue(FReporter.FileName);
                    } break;
                case prpINTV:
                    {
                        aValue.setValue(FReporter.ReportInterval);
                    } break;
                case prpUNIT:
                    {
                        aValue.setValue(TTimeValue.TIMETEXTS[FReporter.IntervalUnit-1]);
                    } break;
                case prpOUTS:
                    {
                        i = 0;
                        aValue.setElementCount(0);
                        outputItem = FReporter.getVariable(0);
                        while (outputItem != null)
                        {
                            i++;
                            aValue.setElementCount(i);
                            aValue.item(i).member("varname").setValue(outputItem.sName);
                            aValue.item(i).member("alias").setValue(outputItem.sAlias);
                            aValue.item(i).member("decplaces").setValue(outputItem.iDecPl);
                            aValue.item(i).member("aggreg").setValue(TGenericReporter.sAggregText[(int)outputItem.Aggreg]);
                            outputItem = FReporter.getVariable(i);
                        }
                    } break;
                case prpSUMMARYFILE:
                    {
                        if (FSummaryStream != null)
                        {
                            aValue.setValue(propertyList[prpSUMMARYFILE].asStr());
                        }
                        else
                            aValue.setValue("");
                    } break;
                case prpAPSIMFMT:
                    {
                        aValue.setValue(FReporter.ApsimFMT);
                    } break;
                case prpTITLE:
                    {
                        aValue.setValue(FReporter.Title);
                    } break;
                case prpOUTFREQ:
                    {
                        aValue.setElementCount((uint)RegExtraEvents.Count);
                        uint idx = 1;
                        foreach (KeyValuePair<int, String> pair in RegExtraEvents)
                        {
                            aValue.item(idx).setValue(pair.Value);
                            idx++;
                        }
                    } break;
                case prpDATEFMT:
                    {
                        aValue.setValue(FReporter.DateFMT);
                    }
                    break;
            }
        }
        //==========================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="propertyID"></param>
        /// <param name="aValue"></param>
        /// <returns></returns>
        //==========================================================================
        public override bool writeProperty(int propertyID, TTypedValue aValue)
        {
            bool result = true;
            switch (propertyID)
            {
                case prpFILE:
                    {
                        if (FFirstTime)
                        {
                            initProperty(prpFILE, aValue);
                        }
                    } break;
                case prpSUMMARYFILE:
                    {
                        if (FFirstTime)
                        {
                            FSummaryStream = null;
                            initProperty(prpSUMMARYFILE, aValue);
                        }
                    } break;
                default: result = false; break;
            }

            if (!result)
            {
                result = base.writeProperty(propertyID, aValue);
            }
            return result;
        }
        //==========================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="eventID"></param>
        /// <param name="iState"></param>
        /// <param name="publisherID"></param>
        /// <param name="aParams"></param>
        /// <returns></returns>
        //==========================================================================
        public override int processEventState(int eventID, int iState, uint publisherID, TTypedValue aParams)
        {
            int iCondition = 0;
            int Idx;
            Boolean bError;
            Boolean isWriteEvent = false;
           
            //Handle any request for writing from extra events.
            //Because of the way TGenericReporter.writeVariables() is configured
            //there will only be one record written per day even if more events
            //are handled per day. This could be modified if required.
            if ( (eventID == evtEXEC) || RegExtraEvents.ContainsKey(eventID) )
            {
                isWriteEvent = true;
                if ((eventID == evtEXEC) && RegExtraEvents.ContainsValue("post"))
                    isWriteEvent = false;     //ignore the update_outputs event when post is registered
            }

            if ((isWriteEvent) && (iState == stateREQS))                  // "Requests" state                      }
            {
                if (FReporter.FileName.Length == 0)
                {
                    iCondition = 1;
                }
                else
                {
                    if (FFirstTime)                                             // Only actually do anything in the      
                    {                                                           //   first time step 
                        FReporter.getRequestNames(ref FRequests);
                        for (Idx = 0; Idx <= FRequests.Count - 1; Idx++)
                        {
                            sendQueryInfo(FRequests[Idx], TypeSpec.KIND_OWNED, eventID); // Handled by processEntityInfo         
                        }
                    }
                    iCondition = 0;
                }
            }
            else
            {
                if ((isWriteEvent) && (iState == stateQUERY))             // "Query" state                         
                {
                    bError = false;
                    if (FFirstTime)
                    {
                        for (Idx = 0; Idx <= FRequests.Count - 1; Idx++)
                        {
                            if (FRequests[Idx].Length > 0)
                            {
                                sendError("Requested variable \"" + FRequests[Idx] + "\" not found in simulation", true);
                                bError = true;
                            }
                        }
                        if (!bError)
                        {
                            FReporter.BeginWriting();                           // Open the output file                  
                        }
                        FFirstTime = false;
                    }
                    if (!bError)
                    {
                        sendDriverRequest(drvTIME, eventID);                    // Request "time" plus the properties    
                        for (Idx = 0; Idx <= FRequests.Count - 1; Idx++)        //   identified as required by the       
                        {
                            sendDriverRequest(drvOUT_OFS + Idx, eventID);
                        }
                        iCondition = 0;
                    }
                }
                else
                {
                    if ((isWriteEvent) && (iState == stateSTORE))         // "Store" state                         
                    {
                        try
                        {
                            FReporter.writeVariables(FTimeStep.getStart());     // Aggregate the current time step's     
                        }
                        catch (Exception excep)                                 //   values and store outputs            
                        {
                            String sNow = FTimeStep.getStart().asDateTimeStr();
                            sendError("Output error for " + sNow + ": " + excep.Message, true);
                        }
                        iCondition = 0;
                    }
                    else
                    {
                        if ((eventID == evtSUMMARYWRITE) && (iState == stateDOWRITE))
                        {
                            if (FSummaryStream != null)
                            {
                                String sMessage = aParams.member("lines").asString() + "\r\n";
                                FSummaryStream.Write(sMessage);
                            }
                            iCondition = 0;
                        }
                        else
                        {
                            iCondition = base.processEventState(eventID, iState, publisherID, aParams);
                        }
                    }
                }
            }
            return iCondition;
        }
        //==========================================================================
        /// <summary>
        /// QueryInfo calls are used to obtain the type (as DDML) of the various         
        /// requested properties. This routine handles the responses to the queryInfo    
        /// messages, setting up driving properties.                                     
        /// * Note how FRequests is emptied as each request is returned. A result of     
        ///   this is that, if the variable name is ambiguous, the name will not be in   
        ///   FRequests when the second returnInfo message arrives and we can trap the   
        ///   error.                                                                     
        /// </summary>
        /// <param name="sReqName"></param>
        /// <param name="sReturnName"></param>
        /// <param name="ownerID"></param>
        /// <param name="entityID"></param>
        /// <param name="iKind"></param>
        /// <param name="sDDML"></param>
        //==========================================================================
        public override void processEntityInfo(string sReqName, string sReturnName, uint ownerID,
                                              uint entityID, int iKind, string sDDML)
        {
            int Idx;

            if ((iKind == TypeSpec.KIND_OWNED_R) || (iKind == TypeSpec.KIND_OWNED_RW)) //Only consider readable properties    
            {
                Idx = FRequests.IndexOf(sReqName);
                if (Idx >= 0)
                {
                    addDriver(sReqName, drvOUT_OFS + Idx, 1, 1, "", false, sDDML, "", "", ownerID);
                    FRequests[Idx] = "";                                               // Blank out the entry rather than       
                }                                                                      //   delete so that Idx values stay      
                else                                                                   //   unique and we keep FRequests.Count  
                    sendError("Ambiguous output name \"" + sReqName + "\"", true);     //   for use in the QUERY state          
            }
        }
        //==========================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="driverID"></param>
        /// <param name="providerID"></param>
        /// <param name="aValue"></param>
        //==========================================================================
        public override void assignDriver(int driverID, uint providerID, TTypedValue aValue)
        {
            if (driverID == drvTIME) 
            {
                FTimeStep.Set(aValue);
            }
            else
                FReporter.storeVariable( aValue );

            //base.assignDriver(driverID, providerID, aValue);
        }
        //==========================================================================
        /// <summary>
        /// Edit the SDML using a dialog within this class library.
        /// This function editInits() is required by the PI calling wrapper if
        /// you want to provide a dialog for editing init values in the 
        /// SDML init section.
        /// </summary>
        /// <param name="sSDML">SDML to be initialised.</param>
        /// <returns>True if the </returns>
        //==========================================================================
        public bool editInits(ref string sSDML)
        {
            MessageBox.Show(sSDML, "Not implemented");
            return false;
            /*
            EditInitForm editDlg = new EditInitForm();

            editDlg.init(sSDML, description(""));   //init with the initsection and comp description
            DialogResult result = editDlg.ShowDialog();
            if (result == DialogResult.OK)
            {
                sSDML = editDlg.getSDML();
                return true;
            }
            else
            {
                return false;
            }
             */ 
        }
    }
}
