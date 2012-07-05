using System;
using System.Collections.Generic;
using System.Text;
using CMPServices;

namespace outputComp
{

    public class TStaticReporter : TGenericReporter
    {

        private static Dictionary<string, TStaticData> _currentValues = new Dictionary<string, TStaticData>();
        private static Dictionary<DateTime, Dictionary<string, TStaticData>> _timeseries = new Dictionary<DateTime, Dictionary<string, TStaticData>>();
        public TStaticReporter()
        {
            
        }

        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        //============================================================================
        public override void beginWriting()
        {
            base.beginWriting();
        }




        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        //============================================================================
        public override void endWriting()
        {
            base.endWriting();

            
        }




        //============================================================================
        /// <summary>
        /// Output one line of the results file 
        /// </summary>
        //============================================================================
        public override void writeValues()
        {
            int Idx;
            TOutputScalar scalarItem;

        
            DateTime dtCurrent = FCurrOutputTime.asDateTime();

            TStaticData objStaticData = new TStaticData("dd/mm/yyyy", "Time", 0, "", dtCurrent);

            if (_currentValues.ContainsKey(getCurrentKey(objStaticData.Name)))
            {
                _currentValues[getCurrentKey(objStaticData.Name)] = objStaticData;
            }
            else
            {
                _currentValues.Add(getCurrentKey(objStaticData.Name), objStaticData);
            }

            //addTimeSeries(dtCurrent, objStaticData);
            

            for (Idx = 0; Idx <= FColumns.Count - 1; Idx++)
            {
                scalarItem = FColumns[Idx];
                objStaticData = new TStaticData(scalarItem.Units, scalarItem.Name, scalarItem.dVal, scalarItem.sVal, DateTime.Now);

                if (_currentValues.ContainsKey(getCurrentKey(objStaticData.Name)))
                {
                    _currentValues[getCurrentKey(objStaticData.Name)] = objStaticData;
                }
                else
                {
                    _currentValues.Add(getCurrentKey(objStaticData.Name), objStaticData);
                }
                //addTimeSeries(dtCurrent, objStaticData);
                ////_currentValues.Add(FColumns[Idx].Name.ToLower(), new TStaticData(scalarItem.Units, scalarItem.Name, scalarItem.dVal, scalarItem.sVal, DateTime.Now));
                
                FColumns[Idx].Clear();      //Clear the current column values       
            }
        }

        private void addTimeSeries(DateTime ipdtCurrent, TStaticData ipobjStatic)
        {
            Dictionary<string, TStaticData> currentStatic = null;

            if (_timeseries.ContainsKey(getTimeSeriesKey(ipdtCurrent)))
            {
                currentStatic = _timeseries[getTimeSeriesKey(ipdtCurrent)];
            }
            else
            {
                currentStatic = new Dictionary<string, TStaticData>();
                _timeseries.Add( getTimeSeriesKey(ipdtCurrent), currentStatic);
            }

            if (!currentStatic.ContainsKey(getCurrentKey(ipobjStatic.Name)))
            {
                currentStatic.Add(getCurrentKey(ipobjStatic.Name), ipobjStatic);
            }
            
        }

        private static string getCurrentKey(string ipsName)
        {
            return ipsName.ToLower();
        }

        private static DateTime getTimeSeriesKey(DateTime ipdtKey)
        {
            return ipdtKey.Date;
        }



        public static void clearValues()
        {
            _currentValues.Clear();
        }


        public static double getCurrentValueDouble(string ipsName)
        {


            if (_currentValues != null)
            {
                if (_currentValues.ContainsKey(getCurrentKey( ipsName )))
                {
                    return _currentValues[getCurrentKey( ipsName)].dValue;
                }
            }

            return 0;
        }



        public static string getCurrentValueString(string ipsName)
        {


            if (_currentValues != null)
            {
                if (_currentValues.ContainsKey(getCurrentKey( ipsName)))
                {
                    return _currentValues[getCurrentKey( ipsName)].sValue;
                }
            }

            return "";
        }

        public static DateTime getCurrentValueDateTime(string ipsName)
        {


            if (_currentValues != null)
            {
                if (_currentValues.ContainsKey(getCurrentKey( ipsName )))
                {
                    return _currentValues[getCurrentKey( ipsName )].dtValue;
                }
            }

            return DateTime.MinValue;
        }
    }

}

