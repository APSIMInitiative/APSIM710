namespace CMPServices
{
    using System;

    /// <summary>
    /// This is a container for a number of GrazPlan data types refactored from GRAZType.pas
    /// Type and constant definitions used in the GRAZPLAN soil moisture, pasture 
    /// and animal models.
    /// (This only contains a small section of the original unit)
    /// </summary>
    public class GrazType
    {
        public const int NONE = 0;
        public const int TOTAL = 0;
        public const int SURFACE = 0;
        public const double VeryLarge = 1.0E6;
        public const double VerySmall = 1.0E-4;

        public const int DigClassNo = 6;
        public const int HerbClassNo = DigClassNo * 2;
        public const int MaxPlantSpp = 50;
        public const int MaxSoilLayers = 50;

        public const int stSEEDL = 1;
        public const int ptLEAF = 1;
        public const int SOFT = 1;
        public const int EFFR = 1;

        public const int stESTAB = 2;
        public const int ptSTEM = 2;
        public const int HARD = 2;
        public const int OLDR = 2;

        public const int stSENC = 3;
        public const int ptROOT = 3;
        public const int UNRIPE = 1;
        public const int stDEAD = 4;
        public const int ptSEED = 4;
        public const int RIPE = 2;
        public const int stLITT1 = 5;
        public const int stLITT2 = 6;

        public const int sgGREEN = 123;
        public const int sgDRY = 456;
        public const int sgAV_DRY = 45;
        public const int sgEST_SENC = 23;
        public const int sgSTANDING = 1234;
        public const int sgLITTER = 56;

        public enum TOMElement { C, N, P, S };
        //public TPlantElement   = TOMElement.N..TOMElement.S;
        public enum TPlantNutrient { pnNO3, pnNH4, pnPOx, pnSO4 };

    }
}
