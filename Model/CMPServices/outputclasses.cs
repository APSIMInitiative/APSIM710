using System;
using System.Collections.Generic;
using System.Text;
using CMPServices;

//============================================================================
// In reading this code, it is important to distinguish three kinds of          
// variables:                                                                   
// 1. "Output" variables are those specified by the user for output.            
// 2. "Requested" variables are the variables for which the reporter acquires   
//    values. The set of requested variables is obtained by taking the          
//    unqualified parts of the output variables, without duplication. For       
//    example, if the user specifies "x[1]", "x[3]", "x[5]" and "z:field",      
//    there wil be two requested variables, namely "x" and "z".                 
// 3. "Column" variables are the scalar sub-values within the output variables. 
//    There will be at least as many column variables as output variables.      
//    The column variables are found by recursing through each output variable, 
//    storing the current position in the tree of variables in a TValTree       
//    structure.                                                                
//                                                                              
// * The output variables are stored as a list of TMainOutputSpecifier objects. 
// * The requested variables are stored as a list of strings; each requested    
//   variable has a TTypedValue associated with it in which the current value   
//   data is stored.                                                            
// * The column variables are stored as a list of TOutputScalar objects. These  
//   objects also handle the aggregation of values within reporting intervals. 
//============================================================================

namespace outputComp
{
    //============================================================================
    /// <summary>
    /// The TValTree class tells us how to locate a column variable. We can't do     
    /// this by pointing directly at TTypedValues because of the way the item()      
    /// function of TTypedValue works 
    /// </summary>
    //============================================================================
    public class TValTree {
        /// <summary>
        /// Id no. returned from defineVariable
        /// </summary>
        public int varId;           
        /// <summary>
        /// index in FColumns list
        /// </summary>
        public int colNo;           
        /// <summary>
        /// index within current structure
        /// </summary>
        public uint index;           
        /// <summary>
        /// 
        /// </summary>
        public TMainOutputSpecifier specifier;
        /// <summary>
        /// actually a "sub" name
        /// </summary>
        public string columnName;   
        /// <summary>
        /// pointer to parent; null for root
        /// </summary>
        public TValTree parent;     
        /// <summary>
        /// 
        /// </summary>
        public List<TValTree> subTrees;
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="aParent"></param>
        /// <param name="aSpecifier"></param>
        //============================================================================
        public TValTree(TValTree aParent, TMainOutputSpecifier aSpecifier)
        {
            parent = aParent;
            specifier = aSpecifier;
            varId = -1;
            colNo = -1;
            index = 0;
            subTrees = new List<TValTree>();
        }
        //============================================================================
        /// <summary>
        /// Iterates up through the tree to form the full name of the coolumn.
        /// </summary>
        /// <returns></returns>
        //============================================================================
        public string FullColumnName()
        {
            TValTree valTree;

            string result = "";
            valTree = this;
            do
            {
                result = valTree.columnName + result;
                valTree = valTree.parent;
            } while (valTree != null);

            return result;
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        //============================================================================
        public int colNumber {
            get {return colNo;}
        }
    }
    //============================================================================
    /// <summary>
    /// TOutputScalar represents a scalar attribute to be stored in an output file.  
    /// It handles aggregation of values during each reporting interval.             
    /// </summary>
    //============================================================================
    public class TOutputScalar 
    {
        /// <summary>
        /// Decimal places
        /// </summary>
        public int decPl;
        /// <summary>
        /// Aggregation type
        /// </summary>
        public TGenericReporter.AggregType Aggreg;
        /// <summary>
        /// 
        /// </summary>
        public int AggregCount;
        /// <summary>
        /// 
        /// </summary>
        public TTypedValue.TBaseType baseType;
        /// <summary>
        /// 
        /// </summary>
        public string Units;
        /// <summary>
        /// 
        /// </summary>
        public string Name;
        /// <summary>
        /// 
        /// </summary>
        public double dVal;
        /// <summary>
        /// 
        /// </summary>
        public string sVal;
        /// <summary>
        /// 
        /// </summary>
        public TTypedValue.TBaseType[,] COLUMNTYPES;
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="baseValue"></param>
        /// <param name="Agg"></param>
        /// <param name="decplaces"></param>
        //============================================================================
        public TOutputScalar(TTypedValue baseValue, TGenericReporter.AggregType Agg, int decplaces)
        {
                                                    // None                  Sum                    Mean                Max                 Min      
            COLUMNTYPES = new TTypedValue.TBaseType[12, 5]  { { TTypedValue.TBaseType.ITYPE_EMPTY, TTypedValue.TBaseType.ITYPE_EMPTY, TTypedValue.TBaseType.ITYPE_EMPTY, TTypedValue.TBaseType.ITYPE_EMPTY, TTypedValue.TBaseType.ITYPE_EMPTY },
                                            { TTypedValue.TBaseType.ITYPE_INT1, TTypedValue.TBaseType.ITYPE_INT4, TTypedValue.TBaseType.ITYPE_DOUBLE, TTypedValue.TBaseType.ITYPE_INT1, TTypedValue.TBaseType.ITYPE_INT1 },
                                            { TTypedValue.TBaseType.ITYPE_INT2, TTypedValue.TBaseType.ITYPE_INT4, TTypedValue.TBaseType.ITYPE_DOUBLE, TTypedValue.TBaseType.ITYPE_INT2, TTypedValue.TBaseType.ITYPE_INT2 },
                                            { TTypedValue.TBaseType.ITYPE_INT4, TTypedValue.TBaseType.ITYPE_INT4, TTypedValue.TBaseType.ITYPE_DOUBLE, TTypedValue.TBaseType.ITYPE_INT4, TTypedValue.TBaseType.ITYPE_INT4 },
                                            { TTypedValue.TBaseType.ITYPE_INT8, TTypedValue.TBaseType.ITYPE_INT8, TTypedValue.TBaseType.ITYPE_DOUBLE, TTypedValue.TBaseType.ITYPE_INT8, TTypedValue.TBaseType.ITYPE_INT8 },
                                            { TTypedValue.TBaseType.ITYPE_SINGLE, TTypedValue.TBaseType.ITYPE_SINGLE, TTypedValue.TBaseType.ITYPE_SINGLE, TTypedValue.TBaseType.ITYPE_SINGLE, TTypedValue.TBaseType.ITYPE_SINGLE },
                                            { TTypedValue.TBaseType.ITYPE_DOUBLE, TTypedValue.TBaseType.ITYPE_DOUBLE, TTypedValue.TBaseType.ITYPE_DOUBLE, TTypedValue.TBaseType.ITYPE_DOUBLE, TTypedValue.TBaseType.ITYPE_DOUBLE },
                                            { TTypedValue.TBaseType.ITYPE_CHAR, TTypedValue.TBaseType.ITYPE_CHAR, TTypedValue.TBaseType.ITYPE_CHAR, TTypedValue.TBaseType.ITYPE_CHAR, TTypedValue.TBaseType.ITYPE_CHAR },   //char     
                                            { TTypedValue.TBaseType.ITYPE_BOOL, TTypedValue.TBaseType.ITYPE_INT4, TTypedValue.TBaseType.ITYPE_DOUBLE, TTypedValue.TBaseType.ITYPE_BOOL, TTypedValue.TBaseType.ITYPE_BOOL }, //boolean  
                                            { TTypedValue.TBaseType.ITYPE_CHAR, TTypedValue.TBaseType.ITYPE_CHAR, TTypedValue.TBaseType.ITYPE_CHAR, TTypedValue.TBaseType.ITYPE_CHAR, TTypedValue.TBaseType.ITYPE_CHAR },   //wchar    
                                            { TTypedValue.TBaseType.ITYPE_STR, TTypedValue.TBaseType.ITYPE_STR, TTypedValue.TBaseType.ITYPE_STR, TTypedValue.TBaseType.ITYPE_STR, TTypedValue.TBaseType.ITYPE_STR },        //string   
                                            { TTypedValue.TBaseType.ITYPE_STR, TTypedValue.TBaseType.ITYPE_STR, TTypedValue.TBaseType.ITYPE_STR, TTypedValue.TBaseType.ITYPE_STR, TTypedValue.TBaseType.ITYPE_STR } };      //wstring  
           

           if (!baseValue.isScalar())
               throw (new ApplicationException("Cannot create non-scalar output columns"));

           baseType = COLUMNTYPES[(int)baseValue.baseType(), (int)Agg]; //Note that the type of the output can depend on the aggregation           
           Units = baseValue.units();
           Aggreg = Agg;
           decPl = decplaces;
           Clear(); //init values
        }

        //============================================================================
        /// <summary>
        /// Updates the current value of the variable in the output column. Handles      
        /// aggregation within reporting intervals                                       
        /// Assumes:                                                                     
        ///  * aValue is a scalar of the correct type 
        /// </summary>
        /// <param name="aValue"></param>
        //============================================================================
        public void Update(TTypedValue aValue)
        {
            switch (baseType)
            {
                case TTypedValue.TBaseType.ITYPE_INT1:
                case TTypedValue.TBaseType.ITYPE_INT2:
                case TTypedValue.TBaseType.ITYPE_INT4:
                case TTypedValue.TBaseType.ITYPE_INT8:
                    {
                        if (AggregCount == 0)
                            dVal = aValue.asInt();
                        else
                        {
                            switch (Aggreg)
                            {
                                case TGenericReporter.AggregType.aggSum: dVal = dVal + aValue.asInt();
                                    break;
                                case TGenericReporter.AggregType.aggMax: dVal = Math.Max(dVal, aValue.asInt());
                                    break;
                                case TGenericReporter.AggregType.aggMin: dVal = Math.Min(dVal, aValue.asInt());
                                    break;
                            }
                        }
                    }
                    break;
                case TTypedValue.TBaseType.ITYPE_SINGLE:
                    {
                        if (AggregCount == 0)
                            dVal = aValue.asSingle();
                        else
                        {
                            switch (Aggreg)
                            {
                                case TGenericReporter.AggregType.aggSum: dVal = dVal + aValue.asSingle();
                                    break;
                                case TGenericReporter.AggregType.aggAve: dVal = (dVal * AggregCount + aValue.asSingle()) / (AggregCount + 1);
                                    break;
                                case TGenericReporter.AggregType.aggMax: dVal = Math.Max(dVal, aValue.asSingle());
                                    break;
                                case TGenericReporter.AggregType.aggMin: dVal = Math.Min(dVal, aValue.asSingle());
                                    break;
                            }
                        }
                    }
                    break;
                case TTypedValue.TBaseType.ITYPE_DOUBLE:
                    {
                        if (AggregCount == 0)
                            dVal = aValue.asDouble();
                        else
                            switch (Aggreg)
                            {
                                case TGenericReporter.AggregType.aggSum: dVal = dVal + aValue.asDouble();
                                    break;
                                case TGenericReporter.AggregType.aggAve: dVal = (dVal * AggregCount + aValue.asDouble()) / (AggregCount + 1);
                                    break;
                                case TGenericReporter.AggregType.aggMax: dVal = Math.Max(dVal, aValue.asDouble());
                                    break;
                                case TGenericReporter.AggregType.aggMin: dVal = Math.Min(dVal, aValue.asDouble());
                                    break;
                            }
                    }
                    break;
                case TTypedValue.TBaseType.ITYPE_BOOL:
                    {
                        if (AggregCount == 0)
                            sVal = aValue.asBool().ToString();
                        else
                            switch (Aggreg)
                            {
                                case TGenericReporter.AggregType.aggMax:
                                    {
                                        bool bVal = Convert.ToBoolean(sVal) || aValue.asBool();
                                        sVal = bVal.ToString();
                                    }
                                    break;
                                case TGenericReporter.AggregType.aggMin:
                                    {
                                        bool bVal = Convert.ToBoolean(sVal) && aValue.asBool();
                                        sVal = bVal.ToString();
                                    }
                                    break;
                            }
                    }
                    break;
                case TTypedValue.TBaseType.ITYPE_CHAR:
                case TTypedValue.TBaseType.ITYPE_STR:
                    {
                        if (AggregCount == 0)
                            sVal = aValue.asStr();
                    }
                    break;
            }
            AggregCount++;
        }
        //============================================================================
        /// <summary>
        /// Invalidates the current value in the output column 
        /// </summary>
        //============================================================================
        public void Clear()
        {
            sVal = "";
            dVal = 0;

            AggregCount = 0;
        }
        
    }
    //============================================================================
    /// <summary>
    /// An "output specifier" is simply a decomposition of a property name.          
    /// It allows us to navigate quickly to a nominated sub-value within a           
    /// TTypedValue.                                                                 
    /// </summary>
    //============================================================================
    public class TOutputSpecifier
    {
        /// <summary>
        /// Name
        /// </summary>
        public string sName;
        /// <summary>
        /// True if this is a field
        /// </summary>
        public bool bIsField;
        /// <summary>
        /// True if this is an element
        /// </summary>
        public bool bIsElement;
        /// <summary>
        /// 
        /// </summary>
        public int iArrayIndex;
        /// <summary>
        /// 
        /// </summary>
        public TOutputSpecifier SubSpecifier;

        //============================================================================
        /// <summary>
        /// Parses the variable name and adds specifiers for each child item.
        /// </summary>
        /// <param name="sVarName">Variable name string.</param>
        //============================================================================
        public TOutputSpecifier(string sVarName)
        {
            int iClosePosn;
            int iFieldPosn;
            int iElemPosn;

            if (sVarName.IndexOf(':') == 0)
            {
                bIsField = true;
                sVarName = sVarName.Remove(0, 1);
            }
            else
            {
                if (sVarName.IndexOf('[') == 0)                                         //if this is an array item
                {
                    bIsElement = true;
                    iClosePosn = sVarName.IndexOf(']');
                    iArrayIndex = Convert.ToInt32(sVarName.Substring(1, iClosePosn-1));
                    sVarName = sVarName.Remove(0, iClosePosn+1);                        //removes [x] from [x]...
                }
            }

            iElemPosn = sVarName.IndexOf('[');
            iFieldPosn = sVarName.IndexOf(':');


            if ((iElemPosn < 0) && (iFieldPosn < 0))                                    // No further qualifiers                 }
                sName = sVarName;
            else
            {
                if ((iFieldPosn >= 0) && ((iElemPosn < 0) || (iElemPosn > iFieldPosn))) //This part names a collection          }
                {
                    sName = sVarName.Substring(0, iFieldPosn );
                    sVarName = sVarName.Remove(0, sName.Length);
                    SubSpecifier = new TOutputSpecifier(sVarName);
                }
                else                                                                    // else this part names an array 'name[1]...'
                {
                    sName = sVarName.Substring(0, iElemPosn);
                    sVarName = sVarName.Remove(0, sName.Length);                        //remove the name part of 'name[]...' 
                    SubSpecifier = new TOutputSpecifier(sVarName);                      //create a specifier for '[]...'
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Finds the sub-value of a TTypedValue corresponding to this specifier 
        /// </summary>
        /// <param name="aValue"></param>
        /// <returns></returns>
        //============================================================================
        public TDDMLValue FindValue(TTypedValue aValue)
        {
            TDDMLValue result;
            if ((SubSpecifier == null) || (aValue == null))
                result = (TDDMLValue)aValue;
            else
            {
                if (SubSpecifier.bIsField)
                    result = (TDDMLValue)(SubSpecifier.FindValue(aValue.member(SubSpecifier.sName)));
                else
                {
                    if (SubSpecifier.bIsElement)
                        result = (SubSpecifier.FindValue(aValue.item((uint)SubSpecifier.iArrayIndex)));
                    else
                        result = null;
                }
            }
            return result;
        }
    }
    //============================================================================
    /// <summary>
    /// 
    /// </summary>
    //============================================================================
    public class TMainOutputSpecifier : TOutputSpecifier
    {
        /// <summary>
        /// Aliased name
        /// </summary>
        public string sAlias;                                
        /// <summary>
        /// Decimal places (not used yet)
        /// </summary>
        public int iDecPl;                                   
        /// <summary>
        /// Aggregation                           
        /// </summary>
        public TGenericReporter.AggregType Aggreg;           
        /// <summary>
        /// Index into the list of requested vars 
        /// </summary>
        public int iReqIdx;    
        /// <summary>
        /// 
        /// </summary>
        public TValTree valTree;
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sVarName"></param>
        //============================================================================
        public TMainOutputSpecifier(string sVarName)
            : base(sVarName)
        {
        }
    }
    //============================================================================
    /// <summary>
    /// TGenericReporter is the ancestor class for TTextReporter and TDBReporter.    
    /// </summary>
    //============================================================================
    public class TGenericReporter
    {
        /// <summary>
        /// Various kinds of aggregation of data     
        /// </summary>
        public enum AggregType
        {
            /// <summary>
            /// No aggregation
            /// </summary>
            aggNone = 0,    
            /// <summary>
            /// Total
            /// </summary>
            aggSum,         
            /// <summary>
            /// Arithmetic mean
            /// </summary>
            aggAve,         
            /// <summary>
            /// Maximum
            /// </summary>
            aggMax,         
            /// <summary>
            /// Minimum
            /// </summary>
            aggMin,         
            /// <summary>
            /// Standard deviation
            /// </summary>
            aggStdDev,      
            /// <summary>
            /// Median
            /// </summary>
            aggMedian,      
            /// <summary>
            /// Number of non-missing values
            /// </summary>
            aggCount           
        };
        /// <summary>
        /// String representation of the aggregation type
        /// </summary>
        public static string[] sAggregText = { "none", "sum", "ave", "max", "min", "stddev", "median", "count" };

        private string FTitle;
        private string FFileName;
        private int FReportInterval;
        private int FIntervalUnit;

        /// <summary>
        /// 
        /// </summary>
        public const string typeOUTPUTS = "<type array=\"T\">" +
                                            "<element>" +
                                                "<field name=\"varname\" kind=\"string\"/>" +
                                                "<field name=\"alias\" kind=\"string\"/>" +
                                                "<field name=\"aggreg\" kind=\"string\"/>" +
                                                "<field name=\"decplaces\" kind=\"integer4\"/>" +
                                            "</element>" +
                                          "</type>";

        /// <summary>
        /// 
        /// </summary>
        protected List<TMainOutputSpecifier> FOutputs;    //Lists of the three different kinds of variables   
        private List<string> FRequestNames;             //Names list parallel to FRequests                        
        /// <summary>
        /// 
        /// </summary>
        protected List<TDDMLValue> FRequests;             //list of refs to driving properties  
        /// <summary>
        /// 
        /// </summary>
        protected List<TOutputScalar> FColumns;
        /// <summary>
        /// Is the results file open?
        /// </summary>
        protected bool FWriting;                                                
        /// <summary>
        /// TRUE if we haven't yet written output 
        /// </summary>
        protected bool FFirstTime; 
        /// <summary>
        /// 
        /// </summary>
        protected TTimeValue FCurrOutputTime;                                   //Start of current reporting interval   
        /// <summary>
        /// 
        /// </summary>
        protected TTimeValue FNextOutputTime;                                   //Start of next reporting interval      
        /// <summary>
        /// Produce a file in the Normal APSIM format
        /// </summary>
        public Boolean ApsimFMT;
        /// <summary>
        /// The date format string. Ignored if empty.
        /// </summary>
        public String DateFMT;
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        //============================================================================
        public TGenericReporter()
        {
            FOutputs = new List<TMainOutputSpecifier>();
            FRequestNames = new List<string>();
            FRequests = new List<TDDMLValue>();
            FColumns = new List<TOutputScalar>();

            FCurrOutputTime = new TTimeValue();                                 //Start of current reporting interval   
            FNextOutputTime = new TTimeValue();                                 //Start of next reporting interval      

            FReportInterval = 1;
            FIntervalUnit = TTimeValue.DAY;
            FWriting = false;
            FFirstTime = true;
            FTitle = new String(' ', 0);
            ApsimFMT = false;
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        public string Title
        {
            get { return FTitle; }
            set { FTitle = value; }
        }
        /// <summary>
        /// 
        /// </summary>
        public string FileName
        {
            get { return FFileName; }
            set { FFileName = value; }
        }
        /// <summary>
        /// 
        /// </summary>
        public int ReportInterval
        {
            get { return FReportInterval; }
            set { FReportInterval = value; }
        }
        /// <summary>
        /// 
        /// </summary>
        public int IntervalUnit
        {
            get { return FIntervalUnit; }
            set { FIntervalUnit = value; }
        }
        /// <summary>
        /// 
        /// </summary>
        public int OutputsCount
        {
            get { return FOutputs.Count; }
        }
        //============================================================================
        /// <summary>
        /// Adds an item to the list of user-specified outputs
        /// </summary>
        /// <param name="sName"></param>
        /// <param name="sAlias"></param>
        /// <param name="iDecPl"></param>
        /// <param name="Aggreg"></param>
        //============================================================================
        public void addVariable(string sName, string sAlias, int iDecPl, TGenericReporter.AggregType Aggreg)
        {
            if (sName.Length > 0)
            {
                TMainOutputSpecifier Specifier = new TMainOutputSpecifier(sName);                    // Make up the specifier                 
                Specifier.iDecPl = iDecPl;
                Specifier.Aggreg = Aggreg;
                if (sAlias.Length > 0)
                    Specifier.sAlias = sAlias;
                else
                    Specifier.sAlias = sName;

                //add the name of the variable to the FRequests list
                Specifier.iReqIdx = FRequestNames.IndexOf(Specifier.sName);
                if (Specifier.iReqIdx == -1)
                {                                                                       // If the main variable name is new,     
                    Specifier.iReqIdx = FRequestNames.Count;                            //   add it to the list of variables to request 
                    FRequestNames.Add(Specifier.sName);
                    FRequests.Add(null);                                                //keep an item parallel to it
                }

                Specifier.valTree = new TValTree(null, Specifier);
                Specifier.valTree.columnName = Specifier.sAlias;

                FOutputs.Add(Specifier);                           // Store the output specifier in FOutputs
            }
        }
        //==============================================================================
        /// <summary>
        /// Return a ptr to a TMainOutputSpecifier object found at index in the
        /// FOutputs list of objects. Returns nil if not found.
        /// </summary>
        /// <param name="index">index = 0->n</param>
        /// <returns></returns>
        //==============================================================================
        public TMainOutputSpecifier getVariable(uint index)
        {
            TMainOutputSpecifier result = null;
            if ((index >= 0) && (index < FOutputs.Count))
            {
                result = FOutputs[(int)index];
            }
            return result;
        }
        //==============================================================================
        /// <summary>
        /// Populates requests with the names of each property that needs to be known to
        /// provide the value of each output property
        /// </summary>
        /// <param name="requests"></param>
        //==============================================================================
        public void getRequestNames(ref List<string> requests)
        {
            requests.Clear();
            for (int i = 0; i < FRequestNames.Count; i++)
                requests.Add(FRequestNames[i]);
        }
        //==============================================================================
        /// <summary>
        /// Stores the current value of a requested variable.
        /// </summary>
        /// <param name="aValue"></param>
        //==============================================================================
        public void storeVariable(TTypedValue aValue)
        {
            int iReqIdx;
            TDDMLValue reqValue;

            iReqIdx = FRequestNames.IndexOf(aValue.Name);
            if (iReqIdx < 0)
                throw (new ApplicationException("storeVariable() - Attempt to store invalid variable: " + aValue.Name));

            reqValue = FRequests[iReqIdx];
            if (reqValue == null)
            {
                reqValue = new TDDMLValue(aValue);           // First time: make a copy of the TTypedValue          
                FRequests[iReqIdx] = reqValue;
            }
            reqValue.setValue(aValue);
        }
        //==============================================================================
        /// <summary>
        /// Called once all output variables have been defined and before storage of     
        /// results begins.                                                              
        /// When overridden, this inherited routine should be called. 
        /// </summary>
        //==============================================================================
        public virtual void BeginWriting()
        {
            FCurrOutputTime.Set(0, 0, 0);
            FWriting = true;
            FFirstTime = true;
        }
        //==============================================================================
        /// <summary>
        /// Called once all outputs have been stored.                                    
        /// When overridden, this inherited routine should be called *first up*.
        /// </summary>
        //==============================================================================
        public virtual void EndWriting()
        {
            if (FWriting)
            {
                writeVariables(FNextOutputTime);     //Forces output                         
            }
            FWriting = false;
        }
        //==============================================================================
        /// <summary>
        /// Output the aggregated values to the persistent storage
        /// </summary>
        //==============================================================================
        public virtual void writeValues()
        {
        }
        //==============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="currTime"></param>
        //==============================================================================
        public void writeVariables(TTimeValue currTime)
        {
            if (FCurrOutputTime.getDay() == 0)   //First time      
            {
                FCurrOutputTime.Set(currTime);
                FNextOutputTime.Set(FCurrOutputTime);
                FNextOutputTime.advTime(ReportInterval, IntervalUnit);
            }
            else
            {
                if (FNextOutputTime <= currTime)   //Store outputs from previous period
                {
                    writeValues();
                    FCurrOutputTime.Set(currTime);
                    FNextOutputTime.Set(FCurrOutputTime);
                    FNextOutputTime.advTime(ReportInterval, IntervalUnit);
                }
            }
            aggregateValues();
        }
        //==============================================================================
        /// <summary>
        /// 
        /// </summary>
        //==============================================================================
        private void aggregateValues()
        {
            TMainOutputSpecifier Specifier;
            TDDMLValue requestValue;
            TDDMLValue outputValue;

            for (int i = 0; i < FOutputs.Count; i++)
            {
                Specifier = (TMainOutputSpecifier)FOutputs[i];
                requestValue = FRequests[Specifier.iReqIdx];
                outputValue = Specifier.FindValue(requestValue);
                if (outputValue != null)
                    aggregateValue(outputValue, Specifier.valTree)  ;
                else
                    throw (new ApplicationException("Output value not found: " + Specifier.sName));
            }
        }
        //==============================================================================
        /// <summary>
        /// Recursive logic for the AggregateValues routine                              
        /// For a scalar value, finds the corresponding output column and updates it.
        /// For an array or collection, recurses into each element or member.          
        /// </summary>
        /// <param name="aValue"></param>
        /// <param name="aTree"></param>
        //==============================================================================
        private void aggregateValue(TTypedValue aValue, TValTree aTree)
        {
            int iColNo;
            int i;
            int oldLen;
            int parentId;

            if (aTree.varId < 0)
            { // Hasn't been defined yet...
                if (aTree.parent != null)
                    parentId = aTree.parent.varId;
                else
                    parentId = -1;
                aTree.varId = defineVariable(aTree.FullColumnName(), (TDDMLValue)aValue, parentId, (int)aTree.index, aTree.specifier);
            }

            if (aValue.isScalar())                                  // Scalar - we store or summarise the    
            {                                                       //  data                                 
                if (aTree.colNo < 0)   // Doesn't have a column number yet...
                {
                    aTree.colNo = FColumns.Count;
                    TOutputScalar aScalar = new TOutputScalar(aValue, aTree.specifier.Aggreg, aTree.specifier.iDecPl);
                    aScalar.Name = aTree.FullColumnName();
                    FColumns.Add(aScalar);
                }
                iColNo = aTree.colNo;
                FColumns[iColNo].Update(aValue);
            }
            else       // Array or collection - recurse         
            {
                if (aValue.count() > aTree.subTrees.Count)
                {  // Need to add new subTrees
                    oldLen = aTree.subTrees.Count;
                    //SetLength(aTree.subTrees, aValue.count());
                    for (i = oldLen; i < aValue.count(); i++)
                    //for (i = oldLen; i <= aTree.subTrees.Count - 1; i++)
                    {
                        aTree.subTrees.Add(new TValTree(aTree, aTree.specifier));
                        aTree.subTrees[i].index = (uint)i + 1;
                        if (aValue.isArray())
                            aTree.subTrees[i].columnName = "[" + aTree.subTrees[i].index.ToString() + "]";
                        else
                        {
                            if (aValue.isRecord())
                                aTree.subTrees[i].columnName = ":" + aValue.item((uint)aTree.subTrees[i].index).Name;
                        }
                        aTree.subTrees[i].varId = defineVariable(aTree.subTrees[i].FullColumnName(),
                                                                 (TDDMLValue)(aValue.item(aTree.subTrees[i].index)),
                                                                 aTree.varId, (int)aTree.subTrees[i].index,
                                                                 aTree.subTrees[i].specifier);
                    }
                }
                for (i = 1; i <= aValue.count(); i++)
                    aggregateValue(aValue.item((uint)i), aTree.subTrees[i - 1]);
            }
        }
        //==============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sVariableName"></param>
        /// <param name="aValue"></param>
        /// <param name="iParentID"></param>
        /// <param name="iIndex"></param>
        /// <param name="Aggreg"></param>
        /// <param name="iDecPlaces"></param>
        /// <returns></returns>
        //==============================================================================
        protected virtual int defineVariable(string sVariableName, TDDMLValue aValue,
                              int iParentID, int iIndex, AggregType Aggreg, int iDecPlaces)
        {
            int result;
            if (FColumns.Count < 1)
                result = -1;
            else
                result = FColumns.Count;
            return result;
        }
        //==============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sVariableName"></param>
        /// <param name="aValue"></param>
        /// <param name="iParentID"></param>
        /// <param name="iIndex"></param>
        /// <param name="Specifier"></param>
        /// <returns></returns>
        //==============================================================================
        protected virtual int defineVariable(string sVariableName, TDDMLValue aValue, int iParentID,
                            int iIndex, TMainOutputSpecifier Specifier)
        {
            int result;
            if (FColumns.Count < 1)
                result = -1;
            else
                result = FColumns.Count;
            return result;
        }
        //==========================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sAggreg"></param>
        /// <returns></returns>
        //==========================================================================
        public AggregType parseAggreg(string sAggreg)
        {
            AggregType result = AggregType.aggNone;
            sAggreg = sAggreg.ToLower();
            if (sAggreg == "stddev")
                result = AggregType.aggStdDev;
            if (sAggreg == "median")
                result = AggregType.aggMedian;
            if ((sAggreg == "total") || (sAggreg == "sum"))
                result = AggregType.aggSum;
            if ((sAggreg == "ave") || (sAggreg == "average") || (sAggreg == "mean"))
                result = AggregType.aggAve;
            if ((sAggreg == "max") || (sAggreg == "maximum"))
                result = AggregType.aggMax;
            if ((sAggreg == "min") || (sAggreg == "minimum"))
                result = AggregType.aggMin;

            return result;
        }
    }
}
