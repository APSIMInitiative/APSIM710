using System;
using System.Reflection;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using CSGeneral;
using System.Xml;

//namespace SoilNitrogen
//{
public class simpleSoilTemp
{

    public simpleSoilTemp(double latitude, double tav, double amp, double maxt, double mint)
    {
        sst_latitude = latitude;
        sst_tav = tav;
        sst_amp = amp;

        double ave_temp = (maxt + mint) * 0.5;
        for (int i = 0; i < surf_temp.Length; i++)
            surf_temp[i] = ave_temp;

    }

    //private double sst_latitude;

    #region Parameters used to initialise the model

    // local latidute
    public double sst_latitude = -999.0;

    // annual average ambient air temperature (oC)
    public double sst_tav = -999.0;

    // annual amplitude of the mean monthly air temperature (oC)
    public double sst_amp = -999.0;

    public DateTime sst_today;

    public double sst_maxt;

    public double sst_mint;

    public double sst_radn;

    public double sst_salb;

    public float[] sst_dlayer;

    public float[] sst_bd;

    public float[] sst_ll15_dep;

    public float[] sst_sw_dep;


    #endregion

    #region Variables other components can access

    public double[] st1;

    #endregion

    private double[] surf_temp = new double[366];   // actual soil surface temperatures (oC)
    private double[] st;

    //public void SoilTemp()
    public double[] SoilTemp(DateTime today, double maxt, double mint, double radn, double salb, float[] dlayer, float[] bd, float[] ll15_dep, float[] sw_dep)
    {

        sst_today = today;
        sst_maxt = maxt;
        sst_mint = mint;
        sst_radn = radn;
        sst_salb = salb;
        sst_dlayer = dlayer;
        sst_bd = bd;
        sst_ll15_dep = ll15_dep;
        sst_sw_dep = sw_dep;
        Array.Resize(ref st, sst_dlayer.Length);



        // Calculates average soil temperature at the centre of each layer
        // based on the soil temperature model of EPIC (Williams et al 1984)
        const double days_in_year = 365.25; // no. of days in one year
        const double nth_solst = 173.0;     // day of year of nthrn summer solstice
        const double temp_delay = 27.0;     // delay from solstice to warmest day (days)
        const double nth_hot = nth_solst + temp_delay; // warmest day of year in nth hemisphere
        const double sth_solst = nth_solst + days_in_year / 2.0; // day of year of sthrn summer solstice
        const double sth_hot = sth_solst + temp_delay; // warmest day of year of sth hemisphere
        const double ang = (2.0 * Math.PI) / days_in_year; // length of one day in radians
        // factor to convert day of year to radian fraction of year

        // Get a factor to calculate "normal" soil temperature from the
        // day of year assumed to have the warmest average soil temperature
        // of the year.  The normal soil temperature varies as a cosine
        // function of alx.  This is the number of radians (time) of a
        // year today is from the warmest soil temp.

        // Check for nth/sth hemisphere
        double alx; // time in radians of year from hottest instance to current
        // day of year as a radian fraction of one year for soil
        // temperature calculations
        if (sst_latitude >= 0)
            alx = ang * sst_today.AddDays(-nth_hot).DayOfYear;//DateUtility.OffsetDayOfYear(year, day_of_year, (int)-nth_hot);
        else
            alx = ang * sst_today.AddDays(-sth_hot).DayOfYear;//DateUtility.OffsetDayOfYear(year, day_of_year, (int)-sth_hot);
        if (alx < 0.0 || alx > 6.31)
            throw new Exception("Value for alx is out of range");

        // get change in soil temperature since hottest day, dec c.
        double dlt_temp = SoilTempDt(alx);

        // get temperature dumping depth (mm per radian of a year)
        double damp = SoilTempDampDepth();

        double cum_depth = 0.0;
        // Now get the average soil temperature for each layer.
        // The difference in temperature between surface and subsurface
        // layers ( exp(zd)) is an exponential function of the ratio of
        // the depth to the bottom of the layer and the temperature
        // damping depth of the soil.

        int nLayers = sst_dlayer.Length;
        st = new double[nLayers];
        for (int layer = 0; layer < nLayers; layer++)
        {
            // get the cumulative depth to bottom of current layer
            cum_depth += sst_dlayer[layer];

            // get the lag factor for depth. This reduces changes in
            // soil temperature with depth. (radians of a year)
            double depth_lag = MathUtility.Divide(cum_depth, damp, 0.0);

            // allow subsurface temperature changes to lag behind
            // surface temperature changes
            st[layer] = LayerTemp(depth_lag, alx, dlt_temp);
            if (st[layer] < -20.0 || st[layer] > 80.0)
                throw new Exception("Value for soil_temp is out of range");
        }

        return st;
    
    }

    private double LayerTemp(double depth_lag, double alx, double dlt_temp)
    {
        // Now get the average soil temperature for the layer.
        // The difference in temperature between surface and subsurface
        // layers ( exp(-depth_lag)) is an exponential function of the ratio of
        // the depth to the bottom of the layer and the temperature
        // damping depth of the soil.

        return (double)sst_tav + ((double)sst_amp / 2.0 * Math.Cos(alx - depth_lag) + dlt_temp) * Math.Exp(-depth_lag);
    }

    private double SoilTempDt(double alx)
    {
        //+  Purpose
        //           Calculates  the rate of change in soil surface temperature
        //           with time.
        //           jngh 24-12-91.  I think this is actually a correction to adjust
        //           today's normal sinusoidal soil surface temperature to the
        //           current temperature conditions.

        //+  Mission Statement
        //     Rate of change in soil surface temperature with time

        // Get today's top layer temp from yesterdays temp and today's
        // weather conditions.
        // The actual soil surface temperature is affected by current
        // weather conditions.
        int yesterday = sst_today.AddDays(-1).DayOfYear;// DateUtility.OffsetDayOfYear(year, day_of_year, -1);
        double ave_temp = (sst_maxt + sst_mint) * 0.5;

        surf_temp[sst_today.DayOfYear - 1] = (1.0 - sst_salb) * (ave_temp + (sst_maxt - ave_temp) *
            Math.Sqrt(sst_radn * 23.8846 / 800.0)) + sst_salb * surf_temp[yesterday - 1];

        // get last few days soil surface temperature for moving average
        const int ndays = 5;
        double ave_temp0 = 0;
        for (int day = 0; day < ndays; day++)
        {
            int doy = sst_today.AddDays(-day).DayOfYear;// DateUtility.OffsetDayOfYear(year, day_of_year, -day);
            ave_temp0 += surf_temp[doy - 1];
        }
        ave_temp0 = ave_temp0 / ndays;

        // Get today's normal surface soil temperature
        // There is no depth lag, being the surface, and there
        // is no adjustment for the current temperature conditions
        // as we want the "normal" sinusoidal temperature for this
        // time of year.

        double temp_a = LayerTemp(0.0, alx, 0.0);

        // Get the rate of change in soil surface temperature with time.
        // This is the difference between a five-day moving average and
        // today's normal surface soil temperature.

        double result = ave_temp0 - temp_a;

        // check output

        if (result < -100.0 || result > 100.0)
            throw new Exception("Value for soiln2_SoilTemp_dt is out of range");

        return result;
    }

    private double SoilTempDampDepth()
    {
        //+  Purpose
        //           Now get the temperature damping depth. This is a function of the
        //             average bulk density of the soil and the amount of water above
        //             the lower limit. I think the damping depth units are
        //             mm depth/radian of a g_year

        //+  Notes
        //       241091 consulted Brian Wall.  For soil temperature an estimate of
        //       the water content of the total profile is required, not the plant
        //       extractable soil water.  Hence the method used here - difference
        //       total lower limit and total soil water instead of sum of differences
        //       constrained to and above.  Here the use of lower limit is of no
        //       significance - it is merely a reference point, just as 0.0 could
        //       have been used.  jngh
        const double sw_avail_tot_min = 0.01;

        int nLayers = sst_dlayer.Length;

        // get average bulk density
        double bd_tot = 0.0;
        double cum_depth = 0.0;
        double ll_tot = 0.0;
        double sw_dep_tot = 0.0;
        for (int layer = 0; layer < nLayers; layer++)
        {
            bd_tot += sst_bd[layer] * sst_dlayer[layer];
            ll_tot += sst_ll15_dep[layer];
            sw_dep_tot += sst_sw_dep[layer];
            cum_depth += sst_dlayer[layer];
        }
        double ave_bd = MathUtility.Divide(bd_tot, cum_depth, 0.0);

        // favbd ranges from almost 0 to almost 1
        // damp_depth_max ranges from 1000 to almost 3500
        // It seems damp_depth_max is the damping depth potential.

        double favbd = ave_bd / (ave_bd + 686.0 * Math.Exp(-5.63 * ave_bd));
        double damp_depth_max = Math.Max(0.0, 1000.0 + 2500.0 * favbd);

        // Potential sw above lower limit - mm water/mm soil depth
        // note that this function says that average bulk density
        // can't go above 2.47222, otherwise potential becomes negative.
        // This function allows potential (ww) to go from 0 to .356

        double ww = Math.Max(0.0, 0.356 - 0.144 * ave_bd);

        // calculate amount of soil water, using lower limit as the
        // reference point.
        double sw_avail_tot = Math.Max(sw_dep_tot - ll_tot, sw_avail_tot_min);

        // get fractional water content -

        // wc can range from 0 to 1 while
        // wcf ranges from 1 to 0

        double wc = MathUtility.Divide(sw_avail_tot, ww * cum_depth, 0.0);
        wc = Math.Max(0.0, Math.Min(1.0, wc));
        double wcf = (1.0 - wc) / (1.0 + wc);

        // Here b can range from -.69314 to -1.94575
        // and f ranges from 1 to  0.142878
        // When wc is 0, wcf=1 and f=500/damp_depth_max
        // and soiln2_SoilTemp_DampDepth=500
        // When wc is 1, wcf=0 and f=1
        // and soiln2_SoilTemp_DampDepth=damp_depth_max
        // and that damp_depth_max is the maximum.

        double b = Math.Log(MathUtility.Divide(500.0, damp_depth_max, 1.0e10));
        double f = Math.Exp(b * wcf * wcf);
        // Get the temperature damping depth. (mm soil/radian of a g_year)
        // discount the potential damping depth by the soil water deficit.
        // Here soiln2_SoilTemp_DampDepth ranges from 500 to almost
        // 3500 mm/58 days.
        return f * damp_depth_max;
    }


}
//}
