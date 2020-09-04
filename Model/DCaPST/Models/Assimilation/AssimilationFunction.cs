using System;

namespace DCAPST
{
    /// <summary>
    /// Manages the calculation of CO2 assimilation rate
    /// </summary>
    /// <remarks>
    /// See the supplementary material from the paper
    /// "Simulating daily field crop canopy photosynthesis: an integrated software package",
    /// by A. Wu et al (2018) for the theory behind this function.
    /// 
    /// Note that some adjustments have been made to account for the CCM model, 
    /// which as of Feb 2020 has not been published.
    /// </remarks>
    public class AssimilationFunction
    {
        /// <summary>
        /// Function terms
        /// </summary>
        public double[] X;

        /// <summary>
        /// Intercellular CO2
        /// </summary>
        public double Ci;

        /// <summary>
        /// Mesophyll resistance
        /// </summary>
        public double Rm;

        /// <summary>
        /// Mesophyll respiration
        /// </summary>
        public double MesophyllRespiration;
        
        /// <summary>
        /// Half the rubisco specificity reciprocal
        /// </summary>
        public double HalfRubiscoSpecificityReciprocal;
        
        /// <summary>
        /// A fraction of the diffusivity solubility ratio
        /// </summary>
        public double FractionOfDiffusivitySolubilityRatio;
        
        /// <summary>
        ///  The bundle sheath conductance
        /// </summary>
        public double BundleSheathConductance;
        
        /// <summary>
        /// Oxygen partial pressure
        /// </summary>
        public double Oxygen;

        /// <summary>
        /// Leaf respiration
        /// </summary>
        public double Respiration;

        /// <summary>
        /// Solves the assimilation function
        /// </summary>        
        public double Value()
        {
            if (X.Length != 9) throw new Exception("Invalid assimilation terms");

            double m = MesophyllRespiration;
            double h = HalfRubiscoSpecificityReciprocal;
            double f = FractionOfDiffusivitySolubilityRatio;
            double g = BundleSheathConductance;
            double o = Oxygen;
            double r = Respiration;

            var n1 = r - X[0];
            var n2 = m - Ci * X[3];
            var n3 = X[4] - X[6];

            var a1 = g * Rm - f * X[1] * X[8];
            var a2 = (Rm * X[3] + X[5]) * X[7];            

            var b0 = Rm * n1 - Ci;
            var b1 = f * X[8] * (r * X[1] - h * X[0]);
            var b2 = g * (b0 - o * X[1] - X[2]);
            var b3 = a2 * n1 + (n2 - n3) * X[7];

            var c1 = X[7] * (n1 * n2 + n3 * X[0] - X[6] * r);
            var c2 = g * (Ci * n1 + o * (h * X[0] + X[1] * r) + r * X[2]);            

            var a = a1 + a2;
            var b = b1 + b2 + b3;
            var c = c1 - c2;

            return SolveQuadratic(a, b, c);
        }

        /// <summary>
        /// The quadratic equation
        /// </summary>
        private static double SolveQuadratic(double a, double b, double c)
        {
            var root = b * b - 4 * a * c;
            return (-b - Math.Sqrt(root)) / (2 * a);
        }
    }
}
