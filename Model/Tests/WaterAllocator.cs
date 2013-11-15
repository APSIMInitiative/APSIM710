namespace APSIM.Tests
{
    using System;
    using System.Text;
    using NUnit.Framework;
    using Uptake;
    using CMPServices;
    
    
    [TestFixture]
    public class TestWaterAllocator
    {
        [SetUp]
        public void Setup()
        {
        }

        [TearDown]
        public void TearDown()
        {
        }

        [Test]
        public void TestUptake()
        {
            double[] Supply0 = {0.081007830798626, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
            double[] Supply1 = {0.246975108981132, 0.43468827009201, 0.548313617706299, 0.906075656414032, 0.795723915100098, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

            double[] RLD0 = {2.34804610954598e-05, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
            double[] RLD1 = {0.0518890619277954, 0.0231433678418398, 0.01268061529845, 0.00678020808845758, 0.00282185478135943, 0.000864986563101411, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

            double[] RR0 = {0.00012, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
            double[] RR1 = {0.00016, 0.00016, 0.00016, 0.00016, 0.00016, 0.00016, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

            double[] fKL0 = {0.07, 0.07, 0.056, 0.05, 0.03, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
            double[] fKL1 = {0.07, 0.07, 0.07, 0.06, 0.04, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

            double[] fLL0 = {0.1, 0.13, 0.14, 0.15, 0.19, 0.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
            double[] fLL1 = {0.1, 0.13, 0.14, 0.15, 0.19, 0.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
            
            WaterAllocator3 AWaterAllocator;
            double[] fTheta = new double[50];
            double fE0;
            SoilWaterParams SoilParams;
            double[] uptake0 = new double[50];
            double[] uptake1 = new double[50];
            
            SoilParams = new SoilWaterParams();
            SoilParams.LastLayer = 6;
            SoilParams.Layer_MM[0] = 0;
            SoilParams.Layer_MM[1] = 100;
            SoilParams.Layer_MM[2] = 100;
            SoilParams.Layer_MM[3] = 100;
            SoilParams.Layer_MM[4] = 150;
            SoilParams.Layer_MM[5] = 200;
            SoilParams.Layer_MM[6] = 200;

            SoilParams.Bottom_MM[0] = 0;
            SoilParams.Bottom_MM[1] = 100;
            SoilParams.Bottom_MM[2] = 200;
            SoilParams.Bottom_MM[3] = 300;
            SoilParams.Bottom_MM[4] = 450;
            SoilParams.Bottom_MM[5] = 650;
            SoilParams.Bottom_MM[6] = 850;

            SoilParams.BulkDensity[0] = 0;
            SoilParams.BulkDensity[1] = 1.2;
            SoilParams.BulkDensity[2] = 1.45;
            SoilParams.BulkDensity[3] = 1.45;
            SoilParams.BulkDensity[4] = 1.45;
            SoilParams.BulkDensity[5] = 1.45;
            SoilParams.BulkDensity[6] = 1.50;

            SoilParams.SW_SAT[0] = 0;
            SoilParams.SW_SAT[1] = 0.51;
            SoilParams.SW_SAT[2] = 0.42;
            SoilParams.SW_SAT[3] = 0.42;
            SoilParams.SW_SAT[4] = 0.42;
            SoilParams.SW_SAT[5] = 0.42;
            SoilParams.SW_SAT[6] = 0.4;

            SoilParams.SW_DUL[0] = 0;
            SoilParams.SW_DUL[1] = 0.17;
            SoilParams.SW_DUL[2] = 0.19;
            SoilParams.SW_DUL[3] = 0.2;
            SoilParams.SW_DUL[4] = 0.23;
            SoilParams.SW_DUL[5] = 0.27;
            SoilParams.SW_DUL[6] = 0.29;

            SoilParams.SW_WP[0] = 0;
            SoilParams.SW_WP[1] = 0.1;
            SoilParams.SW_WP[2] = 0.13;
            SoilParams.SW_WP[3] = 0.14;
            SoilParams.SW_WP[4] = 0.15;
            SoilParams.SW_WP[5] = 0.19;
            SoilParams.SW_WP[6] = 0.22;

            SoilParams.SatCond[0] = 0;

            SoilParams.PSD[1,0] = 0;

            SoilParams.EvapAlpha = 0;
            SoilParams.LastSoilELayer = 3;
            SoilParams.Albedo = 0.17;
            SoilParams.CN_BareSoil = 0;
            SoilParams.InterceptCapacity = 0.55;
            SoilParams.SnowMeltRate = 4.57;
            SoilParams.SnowMeltBase = 0;

            SoilParams.SoilM_SAT[0] = 0;
            SoilParams.SoilM_SAT[1] = 51;
            SoilParams.SoilM_SAT[2] = 42;
            SoilParams.SoilM_SAT[3] = 42;
            SoilParams.SoilM_SAT[4] = 62.9999961853027;
            SoilParams.SoilM_SAT[5] = 84;
            SoilParams.SoilM_SAT[6] = 80;

            SoilParams.SoilM_DUL[0] = 0;
            SoilParams.SoilM_DUL[1] = 17;
            SoilParams.SoilM_DUL[2] = 19;
            SoilParams.SoilM_DUL[3] = 20;
            SoilParams.SoilM_DUL[4] = 34.5;
            SoilParams.SoilM_DUL[5] = 54.0;
            SoilParams.SoilM_DUL[6] = 58;

            SoilParams.SoilM_WP[0] = 0;
            SoilParams.SoilM_WP[1] = 10;
            SoilParams.SoilM_WP[2] = 13;
            SoilParams.SoilM_WP[3] = 14;
            SoilParams.SoilM_WP[4] = 22.5;
            SoilParams.SoilM_WP[5] = 38;
            SoilParams.SoilM_WP[6] = 44;

            SoilParams.EvapU = 0;

            SoilParams.deriveParams();
            AWaterAllocator = new WaterAllocator3(SoilParams);

            double[] UptakeParams = new double[99];

            AWaterAllocator.setPlantParams( 0, UptakeParams);
            AWaterAllocator.setPlantParams(1, UptakeParams);

            AWaterAllocator.setPlantDemand( 0, 0.00107070570811629);
            AWaterAllocator.setPlantDemand( 1, 0.103722460567951);

            AWaterAllocator.setPlantSupply( 0, Supply0);
            AWaterAllocator.setPlantSupply( 1, Supply1);

            AWaterAllocator.setPlantRLD(    0, RLD0);
            AWaterAllocator.setPlantRLD(    1, RLD1);

            AWaterAllocator.setPlantRootR(  0, RR0);
            AWaterAllocator.setPlantRootR(  1, RR1);

            AWaterAllocator.setRootParams(  0, fKL0, fLL1);
            AWaterAllocator.setRootParams(  1, fKL0, fLL1);


            fE0 = 4.64000034332275;
            fTheta[0] = 0.135282158851624;
            fTheta[1] = 0.192098319530487;
            fTheta[2] = 0.218330517411232;
            fTheta[3] = 0.250675082206726;
            fTheta[4] = 0.289465487003326;
            fTheta[5] = 0.310377895832062;

            // do the calculation
            AWaterAllocator.computeWaterUptake( fE0, fTheta );

            AWaterAllocator.returnWaterUptake(0, ref uptake0);
            AWaterAllocator.returnWaterUptake(1, ref uptake1);

            // test values obtained from Pascal code built with singles, hence 1E-8 tolerance
            Assert.AreEqual(0.00107070570811629d, uptake0[0], 1E-8);

            Assert.AreEqual(0.00663335062563419, uptake1[0], 1E-8);
            Assert.AreEqual(0.0157194100320339, uptake1[1], 1E-8);
            Assert.AreEqual(0.0198283866047859, uptake1[2], 1E-8);
            Assert.AreEqual(0.0327659547328949, uptake1[3], 1E-8);
            Assert.AreEqual(0.0287753585726023, uptake1[4], 1E-8);
            Assert.AreEqual(0.0, uptake1[5], 1E-8);
            
             
        }
    }
}
