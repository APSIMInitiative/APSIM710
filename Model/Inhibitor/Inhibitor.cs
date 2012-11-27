using System;
using System.Reflection;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
using CSGeneral;

namespace Inhibitor
{
    /// <summary>
    /// A generic inhibitor for a soil process
    /// </summary>
    public class Inhibitor
    {
        // This module calculates the nitrification inhibiton factor due to presence of DCD.  It also accounts
        //  for DCD degradation as function of environmental factors.
        // DCD effect function is given in the xml file (or user interface).
        // DCD degradation was set to agree with general biochemical processes in the soil, values were adapted
        //  from the mineralisation routine in SoilN, the temperature effect was calibrated based on some 
        //  literature data (decrease over high temp). Similar for the carbon effect. These two were set to 
        //  values corresponding to the ones published by Di & Cameron (various) and J. Sigh Thesis (2006)
        //  => The curves for the effect of environmental factors are also given in the xml file.

        //used in SetModuleSolutes()
        [Link]
        Paddock MyPaddock = null;

        #region "Parameters used for initialisation"

        #region The DCD effect function

        private BrokenStickData dcdEffectData = new BrokenStickData();

        // x-axis values - concentration of dcd in the soil (ppm)
        [Param()]
        private double[] dcd_x
        {
            set
            {
                dcdEffectData.xVals = new double[value.Length];
                for (int i = 0; i < value.Length; i++)
                    dcdEffectData.xVals[i] = value[i];
            }
        }

        // y-axis values - inhibition effect of dcd corresponding to the values given in dcd_x (0.0-1.0)
        [Param()]
        private double[] dcd_y
        {
            set
            {
                dcdEffectData.yVals = new double[value.Length];
                for (int i = 0; i < value.Length; i++)
                    dcdEffectData.yVals[i] = value[i];
            }
        }

        #endregion

        #region The DCD degradation

        [Param()]
        private double dcd_degrad_pot;        // Potential degradation (%/day)

        // SWC degradation factor (0.0-1.0)
        private BrokenStickData dcdSWEffectData = new BrokenStickData();

        // The values of x of the function describing the effect of soil water on dcd degradation
        [Param()]
        private double[] sw_dcd_x
        {
            set
            {
                dcdSWEffectData.xVals = new double[value.Length];
                for (int i = 0; i < value.Length; i++)
                    dcdSWEffectData.xVals[i] = value[i];
            }
        }

        // The values of y of the function describing the effect of soil water on dcd degradation
        [Param()]
        private double[] sw_dcd_y
        {
            set
            {
                dcdSWEffectData.yVals = new double[value.Length];
                for (int i = 0; i < value.Length; i++)
                    dcdSWEffectData.yVals[i] = value[i];
            }
        }

        // Soil temperature effect values
        private BrokenStickData dcdTempEffectData = new BrokenStickData();

        // The values of x of the function describing the effect of temperature on dcd degradation
        [Param()]
        private double[] temp_dcd_x
        {
            set
            {
                dcdTempEffectData.xVals = new double[value.Length];
                for (int i = 0; i < value.Length; i++)
                    dcdTempEffectData.xVals[i] = value[i];
            }
        }

        // The values of y of the function describing the effect of temperature on dcd degradation
        [Param()]
        private double[] temp_dcd_y
        {
            set
            {
                dcdTempEffectData.yVals = new double[value.Length];
                for (int i = 0; i < value.Length; i++)
                    dcdTempEffectData.yVals[i] = value[i];
            }
        }

        private BrokenStickData dcdpHEffectData = new BrokenStickData();

        // The values of x of the function describing the effect of soil pH on dcd degradation
        [Param()]
        private double[] ph_dcd_x
        {
            set
            {
                dcdpHEffectData.xVals = new double[value.Length];
                for (int i = 0; i < value.Length; i++)
                    dcdpHEffectData.xVals[i] = value[i];
            }
        }

        // The values of y of the function describing the effect of soil pH on dcd degradation
        [Param()]
        private double[] ph_dcd_y
        {
            set
            {
                dcdpHEffectData.yVals = new double[value.Length];
                for (int i = 0; i < value.Length; i++)
                    dcdpHEffectData.yVals[i] = value[i];
            }
        }

        private BrokenStickData dcdCarbonEffectData = new BrokenStickData();

        // The values of x of the function describing the effect of soil organic carbon on dcd degradation
        [Param()]
        private double[] carbon_dcd_x
        {
            set
            {
                dcdCarbonEffectData.xVals = new double[value.Length];
                for (int i = 0; i < value.Length; i++)
                    dcdCarbonEffectData.xVals[i] = value[i];
            }
        }

        // The values of y of the function describing the effect of soil organic carbon on dcd degradation
        [Param()]
        private double[] carbon_dcd_y
        {
            set
            {
                dcdCarbonEffectData.yVals = new double[value.Length];
                for (int i = 0; i < value.Length; i++)
                    dcdCarbonEffectData.yVals[i] = value[i];
            }
        }


        #endregion

        #endregion

        #region Inputs from apsim

        // The thickness of each soil layer (mm)
        [Input()]
        private double[] dlayer = null;

        // Amount of dcd in the soil, per layer (kg/ha)
        [Input(IsOptional = true)]
        private double[] dcd = null;
        [Input(IsOptional = true)]
        private double[] ave_soil_temp = null;    // Soil temperature for each layer (oC) from soiltemp
        [Input(IsOptional = true)]
        private double[] st = null;               // Soil temp from soilN 
        [Input()]
        private double[] sw = null;                   // Soil water content (cm3/cm3)
        [Input()]
        private double[] carbon_tot = null;           // Total soil carbon (kg/ha)
        [Input()]
        private double[] bd = null;                   // Soil bulk density (g/cm3)
        [Input()]
        private double[] sat = null;                  // Soil water content at saturation (cm3/cm3)
        [Input()]
        private double[] ll15 = null;                 // Soil water content at lower limit/wilting point (cm3/cm3)

        #endregion

        #region Output variables

        private double[] dcd_effect;
        [Output]
        [Units("0-1")]
        [Description("nitrification inhibition factor due to the presence of DCD")]
        public double[] DCD_inhibitionEffect
        { get { return dcd_effect; } }

        private double[] fac_degradation;
        [Output]
        [Units("%")]
        [Description("fraction of DCD that is degraded each day")]
        public double[] DCD_degradFraction
        { get { return fac_degradation; } }

        private double[] delta_dcd;
        [Output()]
        [Units("kg/ha")]
        [Description("amount of DCD that is degraded each day (kg/ha)")]
        public double[] DCD_degradation
        {
            get
            {
                double[] Result = new double[dlayer.Length];
                for (int z = 0; z < dlayer.Length; z++)
                    Result[z] = -delta_dcd[z];
                return Result;
            }
        }

        #endregion

        #region Internal variables

        private double[] SoilTemp;                   // The soil temperature (oC)
        private double[] SoilCarbon;                 // Total soil carbon (%)
        private double[] SoilPH;                     // The soil pH
        private int dcd_layer;                       // The layer at which dcd will be applied
        private double[] delta_urea;                 // The urea amount resulting from DCD degradation (kg/ha)

        #endregion

        #region event to use

        /// <summary>
        /// send variation in soil nitrogen to APSIM
        /// </summary>
        [Event]
        public event NitrogenChangedDelegate NitrogenChanged;

        #endregion

        #region Initialisation

        [EventHandler()]
        public void OnInit2()
        {

            //Initialise the arrays:
            dcd_effect = new double[dlayer.Length];
            fac_degradation = new double[dlayer.Length];
            SoilCarbon = new double[dlayer.Length];
            SoilPH = new double[dlayer.Length];
            delta_dcd = new double[dlayer.Length];

            //Set base ph
            for (int z = 0; z < dlayer.Length; z++)
                SoilPH[z] = 6.0;  //Soil pH is not simulated so assumed as a constant

            //check the dcd effect coefficients:
            checkArray(ref dcdEffectData.xVals, ref dcdEffectData.yVals);
            //  This represents the response curve for nitrification inhibition due to the
            //    presence of dcd. It varies between 0 (no effect) and 1 (total inhibition).
            //  The x values represent the dcd amount in the soil and y the correspondent inhibiton effect.

            //check the coefficients for the sw effect on dcd degradation:
            checkArray(ref dcdSWEffectData.xVals, ref dcdSWEffectData.yVals);
            //  This is the effect of water content on dcd degradation, it varies between 0 (no degradation) 
            //    and 1(potential degradation).
            //  The x values represent the soil water content and y the correspondent water effect.

            //check the coefficients for the temp effect on dcd degradation:
            checkArray(ref dcdTempEffectData.xVals, ref dcdTempEffectData.yVals);
            //  This is the effect of soil temperature on dcd degradation, it varies between 0 (no degradation) 
            //    and 1(potential degradation).
            //  The x values represent the soil temperature and y the correspondent effect.

            //check the coefficients for the pH effect on dcd degradation:
            checkArray(ref dcdpHEffectData.xVals, ref dcdpHEffectData.yVals);
            //  This is the effect of soil pH on dcd degradation, it varies between 0 (no degradation) 
            //    and 1(potential degradation).
            //  The x values represent the soil pH and y the correspondent pH effect.

            //check the coefficients for the carbon effect on dcd degradation:
            checkArray(ref dcdCarbonEffectData.xVals, ref dcdCarbonEffectData.yVals);
            //  This is the effect of soil carbon on dcd degradation, it varies between 0 (no degradation) 
            //    and 1(potential degradation).
            //  The x values represent the soil carbon content and y the correspondent effect.

            Console.WriteLine("   DCDAction initialised");
            if (dcd == null)
                Console.WriteLine("    - there is no dcd in the simulation!");
            Console.WriteLine();

        }
        #endregion


        #region Calculations

        [EventHandler()]
        public void OnPrepare()
        {
            //Using the new manager dll, establish comm with SoilN and Solute modules
            //Dim MyDCD As Component = Mypaddock.ComponentByName("dcd")
            //Dim MySoilN As SoilN = Mypaddock.ComponentByType("SoilN")

            bool didInterpolate;  // an output from the linear interpolation function, actually not used

            if (dcd != null)
            {
                //Initialise extra variables
                delta_dcd = new double[dlayer.Length];
                delta_urea = new double[dlayer.Length];
                double[] SWFraction = CalcSW_Frac(sw);

                //Read soil temperature (preferably from SoilTemp, if not available use SoilN's
                if (ave_soil_temp != null)
                    for (int z = 0; z < dlayer.Length; z++)
                        SoilTemp[z] = ave_soil_temp[z];
                else if (st != null)
                    for (int z = 0; z < dlayer.Length; z++)
                        SoilTemp[z] = st[z];
                else
                    // something wrong

                    for (int z = 0; z < dlayer.Length; z++)
                    {
                        //calculate the dcd_inhibition factor
                        if (dcd[z] > 0)
                        {
                            //Transform dcd from kg/ha to ppm
                            double dcd_ppm = dcd[z] / (dlayer[z] * 10);  //g/L_soil
                            dcd_ppm = dcd_ppm * 1000 / bd[z];                   //ppm

                            //interpolated value based on the function given. Bounded between 0 and 1.
                            dcd_effect[z] = MathUtility.LinearInterpReal(dcd_ppm, dcdEffectData.xVals, dcdEffectData.yVals, out didInterpolate);
                            dcd_effect[z] = Math.Max(0.0, Math.Min(1.0, dcd_effect[z]));

                            //Calculate the dcd degradation
                            //  * all factor vary between 0 and 1, except f_temp
                            double f_water = MathUtility.LinearInterpReal(SWFraction[z], dcdSWEffectData.xVals, dcdSWEffectData.yVals, out didInterpolate);
                            f_water = Math.Max(0, Math.Min(1, f_water));
                            double f_temp = MathUtility.LinearInterpReal(SoilTemp[z], dcdTempEffectData.xVals, dcdTempEffectData.yVals, out didInterpolate);
                            f_temp = Math.Max(0, Math.Min(1, f_temp));
                            double f_ph = MathUtility.LinearInterpReal(SoilPH[z], dcdpHEffectData.xVals, dcdpHEffectData.yVals, out didInterpolate);
                            f_ph = Math.Max(0, Math.Min(1, f_ph));
                            SoilCarbon[z] = carbon_tot[z] / (bd[z] * dlayer[z] * 100);   //Transform soil carbon from kg/ha into %
                            double f_carbon = MathUtility.LinearInterpReal(SoilCarbon[z], dcdCarbonEffectData.xVals, dcdCarbonEffectData.yVals, out didInterpolate);
                            f_carbon = Math.Max(0, Math.Min(1, f_carbon));
                            fac_degradation[z] = dcd_degrad_pot * f_water * f_temp * f_ph * f_carbon;   //(%)
                            fac_degradation[z] = Math.Max(0, Math.Min(100, fac_degradation[z]));        //make sure degradation is between 0-100
                            delta_dcd[z] = -dcd[z] * fac_degradation[z] / 100;      //(kg/ha)
                            delta_urea[z] = -delta_dcd[z];                           //dcd degrades into urea
                        }
                        else
                        {
                            dcd_effect[z] = 0;
                            fac_degradation[z] = 0;
                        }
                    }

                //Set the values of the inhibition factor and the deltas for DCD and urea
                MyPaddock.Set("nitrification_inhibition", dcd_effect);
                MyPaddock.Set("dlt_dcd", delta_dcd);
                if (delta_urea.Sum() > 0.0)
                {
                    NitrogenChangedType NitrogenChanges = new NitrogenChangedType();
                    NitrogenChanges.Sender = "Inhibitor";
                    NitrogenChanges.DeltaUrea = delta_urea;
                    NitrogenChanges.DeltaNH4 = new double[dlayer.Length];
                    NitrogenChanges.DeltaNO3 = new double[dlayer.Length];
                    NitrogenChanged.Invoke(NitrogenChanges);
                }
            }
        }

        #endregion


        #region Functions

        void checkArray(ref double[] MyArray_x, ref double[] MyArray_y)
        {
            //make sure that both x and y arrays have the same length (if not, the smallest length is chosen)

            if (MyArray_x.Length > MyArray_y.Length)
                Array.Resize(ref MyArray_x, MyArray_y.Length);
            else if (MyArray_x.Length < MyArray_y.Length)
                Array.Resize(ref MyArray_y, MyArray_x.Length);
        }

        double[] CalcSW_Frac(double[] SoilWater)
        {
            //Calculates the soil water fraction (0.0-> LL15, 1.0-> SAT)

            double[] WFraction = new double[SoilWater.Length];

            for (int z = 0; z < SoilWater.Length - 1; z++)
                WFraction[z] = (SoilWater[z] - ll15[z]) / (sat[z] - ll15[z]);
            return WFraction;
        }

        #endregion


    }


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
    /// BrokenStick function data - collection straight lines defined by several points
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

}
