using System;
using System.Text;
using NUnit.Framework;
using ApsimFile;
using System.IO;
using CSGeneral;
using System.Xml;


namespace Test
   {
   [TestFixture]
   public class TestSoilParFileImporter
      {
      private const string ParContents = 
         "[300mm.soilwat2.parameters]\r\n" +
         "wet_soil_depth = 300\r\n" +

         "[600mm.soilwat2.parameters]\r\n" +
         "wet_soil_depth = 600\r\n" +

         "[900mm.soilwat2.parameters]\r\n" +
         "wet_soil_depth = 900\r\n" +

         "[1200mm.soilwat2.parameters]\r\n" +
         "wet_soil_depth = 1200\r\n" +

         "[1500mm.soilwat2.parameters]\r\n" +
         "wet_soil_depth = 1500\r\n" +

         "[BlackVertosol.soilwat2.parameters]\r\n" +

         "!Title = Jimbour Waco (Black Vertosol) #16\r\n" +

         "   diffus_const    = 40    ()     ! coeffs for dbar\r\n" +
         "   diffus_slope    = 16    ()     !     \r\n" +
         "   salb            = 0.13   ()     ! bare soil albedo\r\n" +

         "   ! CN2b CN_red CN_cov\r\n" +
         "   !  73   20     0.8   - cultivated cracking clays & black earths (eg. greenmount & capella)\r\n" +
         "   !                      & well structured grey clays (ackland)\r\n" +
         "   !  82   20     0.8   - mod. hard setting clays/brigalow (eg. Brigalow Res. Stn) ?brown clay?\r\n" +
         "   !  85   ??     0.8   - Maranoa (rainsim) ??? ?brown clay (eg. Wallumbilla)\r\n" +
         "   !  90   ??     0.8   - Billa Billa (rainsim)\r\n" +
         "   !  94   28     0.8   - hard setting red brown earths (eg ICRASAT & Tippera)\r\n" +
         "   !  94   42     0.8   - hard set pasture solodics\r\n" +

         "   cn2_bare        = 73       ()     ! runoff curve number for BARE soil at AMC2\r\n" +
         "   cn_red          = 20       ()     ! reduction in CN2_bare for cn_cov increase in cover\r\n" +
         "   cn_cov          = 0.8      ()     ! frac. cover for cn_red reduction in cover\r\n" +
         "                                     ! & max. cover for reduction\r\n" +

         "   dlayer  =  150.000 150.000 300.000 300.000 300.000 300.000 300.000 300.000 300.000 300.000 300.000     (mm)    ! layer depth\r\n" +
         "   air_dry =  0.145   0.260   0.290   0.290   0.300   0.310   0.320   0.330   0.340   0.350   0.360     (mm/mm) ! air dry\r\n" +
         "   ll15    =  0.290   0.290   0.290   0.290   0.300   0.310   0.320   0.330   0.340   0.350   0.360     (mm/mm) ! 15bar lower limit\r\n" +
         "   dul     =  0.540   0.530   0.540   0.540   0.520   0.500   0.500   0.480   0.470   0.460   0.440     (mm/mm) ! upper limit\r\n" +
         "   sat     =  0.590   0.580   0.590   0.580   0.570   0.550   0.550   0.530   0.520   0.510   0.490     (mm/mm)\r\n" +
         "   swcon   =  0.3     0.3     0.3     0.3     0.3     0.3     0.3     0.3     0.3     0.3     0.3     ()      ! soil conductivity\r\n" +
         "   bd      =  1.02    1.03    1.02    1.02    1.06    1.11    1.12    1.15    1.18     1.2    1.25     (g/cc)  ! bulk density gm dry soil/cc moist soil\r\n" +

         "[BlackVertosol.soiln2.parameters]\r\n" +
         "\r\n" +
         "   root_cn           = 40   ()         !\r\n" +
         "   root_wt           = 200   (kg/ha)    !\r\n" +
         "   soil_cn           = 12.5   ()         ! soil humic pool C:N ratio\r\n" +

         "   enr_a_coeff       = 7.4   ()\r\n" +
         "   enr_b_coeff       = 0.2  ()\r\n" +
         "   profile_reduction =    off              ! (on|off) whether to remap N+C if\r\n" +
         "                                           ! soil loss occurs.\r\n" +

         "   oc      = 1.04    0.89    0.89    0.89    0.77    0.45    0.27    0.22    0.16    0.13    0.12\r\n" +
         "   ph      = 8.4     8.8       9     9.2     9.2     9.1       9       9     8.9     8.9     8.9\r\n" +
         "   fbiom   = 0.025    0.02   0.015    0.01    0.01    0.01    0.01    0.01    0.01    0.01    0.01     ()    ! microbe fraction of hunic pool\r\n" +
         "   finert  = 0.4     0.6     0.8     0.9    0.95    0.95    0.95    0.95    0.95    0.95    0.95     ()    ! inert fraction of humic pool\r\n" +
         "   ureappm = 0       0       0       0       0       0       0       0       0       0       0     (ppm) ! ppm urea\r\n" +
         "   no3ppm = 6.5     2.1     2.1     1.7     1.7     1.7     1.7     1.7     1.7     1.7     1.7     ()   ! ppm nitrate\r\n" +
         "   nh4ppm = 0.6     0.1     0.1     0.1     0.1     0.1     0.1     0.1     0.1     0.1     0.1     ()   ! ppm ammonia\r\n" +

         "[BlackVertosol.Barley.parameters]\r\n" +
         "   ! Predicted values: false\r\n" +
         "   ll  = 0.290   0.290   0.320   0.380   0.390   0.390   0.410   0.480   0.470   0.460   0.440       ! Crop lower limits for each layer\r\n" +
         "   kl  = 0.1     0.1    0.08    0.06    0.04    0.02    0.01       0       0       0       0       ! Water Extraction parameter (0-1)\r\n" +
         "   xf  = 1       1       1       1       1       1       1       0       0       0       0       ! Root Exploration factor (0-1)\r\n" +
         "[BlackVertosol.Chickpea.parameters]\r\n" +
         "   ! Predicted values: false\r\n" +
         "   ll  = 0.290   0.290   0.360   0.430   0.510   0.500   0.500   0.480   0.470   0.460   0.440       ! Crop lower limits for each layer\r\n" +
         "   kl  = 0.1     0.1    0.08    0.06    0.04    0.02    0.01       0       0       0       0       ! Water Extraction parameter (0-1)\r\n" +
         "   xf  = 1       1       1       1       1       1       1       0       0       0       0       ! Root Exploration factor (0-1)\r\n" +
         "[BlackVertosol.Lucerne.parameters]\r\n" +
         "   ! Predicted values: false\r\n" +
         "   ll  = 0.290   0.290   0.290   0.290   0.300   0.310   0.320   0.330   0.340   0.350   0.360       ! Crop lower limits for each layer\r\n" +
         "   kl  = 0.1     0.1     0.1     0.1    0.09    0.09    0.09    0.09    0.09    0.09    0.09       ! Water Extraction parameter (0-1)\r\n" +
         "   xf  = 1       1       1       1       1       1       1       1       1       1       1       ! Root Exploration factor (0-1)\r\n" +
         "[BlackVertosol.Maize.parameters]\r\n" +
         "   ! Predicted values: false\r\n" +
         "   ll  = 0.290   0.290   0.340   0.340   0.370   0.400   0.420   0.480   0.470   0.460   0.440       ! Crop lower limits for each layer\r\n" +
         "   kl  = 0.1     0.1     0.1    0.08    0.06    0.04    0.02       0       0       0       0       ! Water Extraction parameter (0-1)\r\n" +
         "   xf  = 1       1       1       1       1       1       1       0       0       0       0       ! Root Exploration factor (0-1)\r\n" +
         "[BlackVertosol.Perennial Grass.parameters]\r\n" +
         "   ! Predicted values: false\r\n" +
         "   ll  = 0.290   0.290   0.390   0.410   0.400   0.400   0.410   0.410   0.400   0.400   0.420       ! Crop lower limits for each layer\r\n" +
         "   kl  = 0.1     0.1     0.1     0.1    0.09    0.07    0.05       0       0       0       0       ! Water Extraction parameter (0-1)\r\n" +
         "   xf  = 1       1       1       1       1       1       1       0       0       0       0       ! Root Exploration factor (0-1)\r\n" +
         "[BlackVertosol.Sorghum.parameters]\r\n" +
         "   ! Predicted values: false\r\n" +
         "   ll  = 0.290   0.290   0.350   0.380   0.400   0.400   0.400   0.480   0.470   0.460   0.440       ! Crop lower limits for each layer\r\n" +
         "   kl  = 0.1     0.1     0.1    0.08    0.06    0.04    0.02       0       0       0       0       ! Water Extraction parameter (0-1)\r\n" +
         "   xf  = 1       1       1       1       1       1       1       0       0       0       0       ! Root Exploration factor (0-1)\r\n" +
         "[BlackVertosol.nwheat.parameters]\r\n" +
         "   ! Predicted values: false\r\n" +
         "   ll   = 0.290   0.290   0.320   0.320   0.350   0.380   0.410   0.480   0.470   0.460   0.440       ! Crop lower limits for each layer\r\n" +
         "   wr   = 1.00    0.85    0.60    0.40    0.10    0.10   0.10\r\n" +
         "   nem  = 0.6     0.6     0.6     0.6     0.6     0.6    0.6\r\n" +
         "[BlackVertosol.Wheat.parameters]\r\n" +
         "   ! Predicted values: false\r\n" +
         "   ll  = 0.290   0.290   0.320   0.320   0.350   0.380   0.410   0.480   0.470   0.460   0.440       ! Crop lower limits for each layer\r\n" +
         "   kl  = 0.1     0.1    0.08    0.06    0.04    0.02    0.01       0       0       0       0       ! Water Extraction parameter (0-1)\r\n" +
         "   xf  = 1       1       1       1       1       1       1       0       0       0       0       ! Root Exploration factor (0-1)";

      [SetUp]
      public void Init()
         {
         StreamWriter Temp = new StreamWriter("Temp.par");
         Temp.Write(ParContents);
         Temp.Close();
         }

      [TearDown]
      public void TearDown()
         {
         File.Delete("Temp.par");
         }

      [Test]
      public void TestImportFromPar()
         {
         XmlNode S = Soil.CreateFromXML(SoilParFileImporter.Import(".\\Temp.par"));
         Assert.AreEqual(XmlHelper.Name(S), "BlackVertosol");
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "BD").Doubles,
                                            new double[] { 1.02, 1.03, 1.02, 1.02, 1.06, 1.11, 1.12, 1.15, 1.18, 1.2, 1.25 }));
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "OC").Doubles,
                                            new double[] {1.04, 0.89, 0.89, 0.89, 0.77, 0.45, 0.27, 0.22, 0.16, 0.13, 0.12 }));
         Assert.IsTrue(MathUtility.AreEqual(Soil.Get(S, "nwheat LL").Doubles,
                                            new double[] { 0.290, 0.290, 0.320, 0.320, 0.350, 0.380, 0.410, 0.480, 0.470, 0.460, 0.440 }));
        
         }
      }
   }
