using System;
using System.Collections.Generic;
using System.Text;

namespace outputComp
{
    public class TStaticData
    {

        /// <summary>
        /// 
        /// </summary>
        private string _Units;
        /// <summary>
        /// 
        /// </summary>
        private string _Name;
        /// <summary>
        /// 
        /// </summary>
        private double _dVal;
        /// <summary>
        /// 
        /// </summary>
        private string _sVal;

        /// <summary>
        /// 
        /// </summary>
        private DateTime _dtVal;

        public TStaticData(string units, string name, double dVal, string sVal, DateTime dtVal)
        {
            _Units = units;
            _Name = name;
            _dVal = dVal;
            _sVal = sVal;
            _dtVal = dtVal;
        }

        public DateTime dtValue
        {
            get { return _dtVal; }
            set { _dtVal = value; }
        }

        public string sValue
        {
            get { return _sVal; }
            set { _sVal = value; }
        }

        public double dValue
        {
            get { return _dVal; }
            set { _dVal = value; }
        }

        public string Name
        {
            get { return _Name; }
            set { _Name = value; }
        }

        public string Units
        {
            get { return _Units; }
            set { _Units = value; }
        }
    }

}
