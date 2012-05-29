using System;
using System.Collections.Generic;
using System.Text;

using CMPServices;

namespace CPIUserInterface
{
    //=====================================================================
    /// <summary>
    /// contains the data used by the simulation tree node control
    /// </summary>
    public class TAFTreeViewColumnTag
    {
        private TTypedValue _typedValue;
        //private string[] _sColumnData;
        private string _sVariable;
        private string _sValue;
        private string _sType;
        private string _sUnit;
        private string _sDefault;
        private string _sMin;
        private string _sMax;

        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="typedValue"></param>
        public TAFTreeViewColumnTag(TTypedValue typedValue)
        {
            _typedValue = typedValue;
            //_sColumnData = ipsColumnData;

            _sVariable = typedValue.Name;
            _sValue = typedValue.asString();
            _sType = typedValue.typeName();

            if (_sType.ToLower().Equals("defined") || (TypedValue.isRecord() == true))
            {
                _sType = "record";
            }

            if (TypedValue.isArray())
            {
                _sType = "array";
            }
            _sUnit = typedValue.units();

            if (typedValue is TInitValue)
            {
                TInitValue initValue = typedValue as TInitValue;

                if (initValue.getDefault() != null)
                {
                    _sDefault = initValue.getDefault().asString();
                }
                else
                {
                    _sDefault = "";
                }

                if (initValue.getMin() != null)
                {
                    _sMin = initValue.getMin().asString();
                }
                else
                {
                    _sMin = "";
                }

                if (initValue.getMax() != null)
                {
                    //_sMax = Math.Round( initValue.getMax().asDouble(),3).ToString();
                    _sMax = initValue.getMax().asString();
                    //initValue.getMax().typeName()

                    //double dMaxValue = double.MaxValue;
                    //switch (initValue.baseType())
                    //{
                    //    case TTypedValue.TBaseType.ITYPE_INT1:
                    //        {
                    //            //FMax.setValue(+VERYLARGE_I1);
                    //            //FMin.setValue(-VERYLARGE_I1);
                    //        } break;
                    //    case TTypedValue.TBaseType.ITYPE_INT2:
                    //        {
                    //            //FMax.setValue(+VERYLARGE_I2);
                    //            //FMin.setValue(-VERYLARGE_I2);
                    //        } break;
                    //    case TTypedValue.TBaseType.ITYPE_INT4:
                    //        {
                    //            //FMax.setValue(+VERYLARGE_I4);
                    //            //FMin.setValue(-VERYLARGE_I4);
                    //        } break;
                    //    case TTypedValue.TBaseType.ITYPE_INT8:
                    //        {
                    //            //FMax.setValue(+VERYLARGE_I8);
                    //            //FMin.setValue(-VERYLARGE_I8);
                    //        } break;
                    //    case TTypedValue.TBaseType.ITYPE_SINGLE:
                    //        {
                    //            //FMax.setValue(+1.0 * VERYLARGE_S);
                    //            //FMin.setValue(-1.0 * VERYLARGE_S);
                    //        } break;
                    //    case TTypedValue.TBaseType.ITYPE_DOUBLE:
                    //        {
                    //            dMaxValue = TInitValue.VERYLARGE_D_POS;
                    //            //FMin.setValue(VERYLARGE_D_NEG);
                    //        } break;
                    //}

                    //if (initValue.getMax().asDouble() == dMaxValue)
                    //{
                    //    Console.WriteLine("equal");
                    //    _sMax = "";
                    //}
                }
                else
                {
                    _sMax = "";
                }
            }
        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public string updateValue(string value)
        {
            string tmp = value.Replace("\n", "");

            if (tmp.Equals(""))
            {
                value = DefaultValue;
            }

            //when this is a numeric value check it's range
            if ((_typedValue.isScalar()) && (!_typedValue.isTextType()) && (_typedValue.baseType() != TTypedValue.TBaseType.ITYPE_BOOL))
            {
                try
                {
                    double dValue = Convert.ToDouble(value);
                    if (TypedValue is TInitValue)
                    {
                        TInitValue initValue = TypedValue as TInitValue;

                        if (dValue > initValue.getMax().asDouble())
                        {
                            //value = initValue.getMax().asString();
                            value = DefaultValue;
                        }
                        if (dValue < initValue.getMin().asDouble())
                        {
                            //value = initValue.getMin().asString();
                            value = DefaultValue;
                        }
                    }
                }
                catch
                {
                    value = Value;
                }
            }
            _typedValue.setValue(value);

            return value;
        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="i"></param>
        /// <returns></returns>
        public string getValueAtColumn(int i)
        {
            if (i == 0)
            {
                return Variable;
            }
            else if (i == 1)
            {
                return Value;
            }
            else if (i == 2)
            {
                return Type;
            }
            else if (i == 3)
            {
                return Unit;
            }
            else if (i == 4)
            {
                return DefaultValue;
            }
            else if (i == 5)
            {
                return Min;
            }
            else if (i == 6)
            {
                return Max;
            }
            else 
            {
                return "";
            }
        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        public TTypedValue TypedValue
        {
            get { return _typedValue; }
        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        public string Variable
        {
            get { return _sVariable; }
        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        public string Value
        {
            get { return _sValue; }
            set { _sValue = value; }

        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        public string Type
        {
            get { return _sType; }
        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        public string Unit
        {
            get { return _sUnit; }
        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        public string DefaultValue
        {
            get { return _sDefault; }
        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        public string Min
        {
            get { return _sMin; }
        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        public string Max
        {
            get { return _sMax; }
        }
    }
}
