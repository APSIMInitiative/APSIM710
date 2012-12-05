using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SoilCN
{
    /// <summary>
    /// List of approaches available for computing urea hydrolysis
    /// </summary>
    public enum UreaHydrolysisApproach { APSIMdefault, RCichota };

    /// <summary>
    /// List of approaches available for computing nitrification
    /// </summary>
    public enum NitrificationApproach { APSIMdefault, RCichota };

    /// <summary>
    /// List of approaches available for computing denitrification
    /// </summary>
    public enum DenitrificationApproach { APSIMdefault, RCichota };
    
    
    
    /// <summary>
    /// BendingStick function data - exponential line hanging in two points
    /// </summary>
    public struct BendingStickData
    {
        // parameters defining a bending stick function, a exponential line haning on two points. used to compute the temperature factor
        /// <summary>
        /// The value of x at which y is optimum
        /// </summary>
        public double[] xValueAtOptimum;
        /// <summary>
        /// The value of y when x is zero
        /// </summary>
        public double[] yValueAtZero;
        /// <summary>
        /// The curve exponent
        /// </summary>
        public double[] CurveExponent;
    }

    /// <summary>
    /// BrokenStick function data - collection of straight lines defined by several points
    /// </summary>
    public struct BrokenStickData
    {
        // lists with value of x and y defining a broken stick function. Used to describe certain factors (water factor, for ex.)
        /// <summary>
        /// List of values on the x-axis
        /// </summary>
        public double[] xVals;
        /// <summary>
        /// List of values on y-axis
        /// </summary>
        public double[] yVals;
    }

    class VariablesAndTypes
    {

        //public const double EPSILON = Math.Pow(2, -24);

    }

    public class g
    {

        [Param()]
        public static double WarningThreshold = 0.0;
        
        [Param()]
        public static double FatalThreshold = 0.0;


        public const double EPSILON = 2/100; //Math.Pow(2, -24);
        private double test1 = 0.0;
        public double test2 = 1.0 * 2.0;
        public double test3
        { get { return 3.0; } }
    }

    public class Params
    {
        static public double Potential = 0.0;
    }

}
