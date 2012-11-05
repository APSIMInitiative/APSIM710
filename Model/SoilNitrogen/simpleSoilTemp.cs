using System;
using System.Reflection;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using CSGeneral;
using System.Xml;

/// <summary>
/// Calculates the average soil temperature at the centre of each layer, based on the soil temperature model of EPIC (Williams et al 1984)
/// This code was separated form old SoilN - tidied up but not updated (RCichota)
/// </summary>

public class simpleSoilTemp
{

    #region Parameters and inputs provided by the user or APSIM

    #region Parameters used on initialisation only

    // local latitude
    private double _latitude = -999.0;

    // annual average ambient air temperature (oC)
    private double _tav = -999.0;

    // annual amplitude of the mean monthly air temperature (oC)
    private double _amp = -999.0;

    #endregion

    #region Parameters that do or may change during simulation

    // today's net solar radiation
    private double _radn = 0.0;

    // today's maximun air temperature
    private double _maxt = 0.0;

    // today's minimun air temperature
    private double _mint = 0.0;

    // soil albedo (0-1)
    private double _salb;

    // values passed via SoilTemperature()

    public DateTime _today;

    public float[] _dlayer;

    public float[] _bd;

    public float[] _ll15_dep;

    public float[] _sw_dep;

    #endregion

    #endregion

    #region Internal variables

    // estimated soil surface temperatures (oC)
    private double[] surf_temp = new double[366];

    // estimated soil temperature profile (oC)
    private double[] st;

    // average air temperature (oC)
    private double ave_temp;

    #endregion

    #region Internal constants

    // day of year of nthrn summer solstice
    private const double nth_solst = 173.0;

    // delay from solstice to warmest day (days)
    private const double temp_delay = 27.0;

    // warmest day of year in nth hemisphere
    private const double nth_hot = nth_solst + temp_delay;

    // day of year of sthrn summer solstice
    private const double sth_solst = nth_solst + 365.25 / 2.0;

    // warmest day of year of sth hemisphere
    private const double sth_hot = sth_solst + temp_delay;

    // length of one day in radians
    private const double ang = (2.0 * Math.PI) / 365.25;

    // number of days to compute surface soil temperature
    private const int ndays = 5;

    #endregion

    public simpleSoilTemp(double latitude, double tav, double amp, double mint, double maxt)
    {
        _latitude = latitude;
        _tav = tav;
        _amp = amp;

        // need to initialise some values for surf_temp, repeat the value of ave_temp for the first day
        double ave_temp = (maxt + mint) * 0.5;
        for (int i = 0; i < surf_temp.Length; i++)
            surf_temp[i] = ave_temp;
    }

    public double[] SoilTemperature(DateTime today, double mint, double maxt, double radn, double salb, float[] dlayer, float[] bd, float[] ll15_dep, float[] sw_dep)
    {
        _today = today;
        _mint = mint;
        _maxt = maxt;
        _radn = radn;
        _salb = salb;
        _dlayer = dlayer;
        _bd = bd;
        _ll15_dep = ll15_dep;
        _sw_dep = sw_dep;

        st = new double[dlayer.Length];

        ave_temp = (_maxt + _mint) * 0.5;

        // Calculate "normal" soil temperature from the day of year assumed to have the warmest average soil temperature over the year
        // The normal soil temperature varies as a cosine function of alx, with average tav and amplitude amp

        // time of year, in radians, from hottest instance to current day
        double alx;
        if (_latitude >= 0)
            alx = ang * _today.AddDays(-nth_hot).DayOfYear;
        else
            alx = ang * _today.AddDays(-sth_hot).DayOfYear;
        if (alx < 0.0 || alx > 6.31)
            throw new Exception("Value for alx is out of range");

        // get the change in soil temperature since hottest day of the year
        double TempChange = dlt_temp(alx);

        // get temperature dumping depth (mm per radian of a year)
        double DampingDepth = DampDepth();

        // compute soil temp for each of the remaining layer
        double cum_depth = 0.0;
        for (int layer = 0; layer < _dlayer.Length; layer++)
        {
            // cumulative depth to bottom of current layer
            cum_depth += _dlayer[layer];

            // depth lag factor - This reduces changes in soil temperature with depth (radians of a year)
            double depth_lag = MathUtility.Divide(cum_depth, DampingDepth, 0.0);

            // soil temperature
            st[layer] = LayerTemp(depth_lag, alx, TempChange);
            if (st[layer] < -20.0 || st[layer] > 80.0)
                throw new Exception("Value for soil_temp is out of range");
        }

        return st;

    }

    private double dlt_temp(double alx)
    {
        // + Purpose
        //     Calculates  the rate of change in soil surface temperature with time.
        //       This is a correction to adjust today's normal sinusoidal soil surface temperature to the current temperature conditions.

        // estimate today's top layer temperature from yesterdays temp and today's weather conditions.
        surf_temp[_today.DayOfYear - 1] = (1.0 - _salb) * (ave_temp + (_maxt - ave_temp) * Math.Sqrt(_radn * 23.8846 / 800.0)) + _salb * surf_temp[_today.AddDays(-1).DayOfYear - 1];

        // average of soil surface temperature over last ndays
        double ave_surf_temp = 0;
        for (int day = 0; day < ndays; day++)
            ave_surf_temp += surf_temp[_today.AddDays(-day).DayOfYear - 1];
        ave_surf_temp = ave_surf_temp / ndays;

        // Calculate today's normal surface soil temperature.
        // There is no depth lag, being the surface, and there is no adjustment for the current temperature conditions
        //  as we want the "normal" sinusoidal temperature for this time of year.
        double normal_temp = LayerTemp(0.0, alx, 0.0);

        // Estimate the rate of change in soil surface temperature with time.
        // This is the difference between a five-day moving average and today's normal surface soil temperature.
        double result = ave_surf_temp - normal_temp;

        // check output
        if (result < -100.0 || result > 100.0)
            throw new Exception("Value for simpleSoilTemp dlt_temp is out of range");

        return result;
    }

    private double LayerTemp(double depth_lag, double alx, double dlt_temp)
    {
        // + Purpose
        //      Calculate the average soil temperature for a given layer.
        //      The difference in temperature between surface and layers is an exponential function of the ratio of
        //        the depth to the bottom of the layer and the temperature damping depth of the soil.

        return _tav + (_amp / 2.0 * Math.Cos(alx - depth_lag) + dlt_temp) * Math.Exp(-depth_lag);
    }

    private double DampDepth()
    {
        // + Purpose
        //      Now get the temperature damping depth. 
        //       This is a function of the average soil bulk density and the amount of water above the lower limit. 

        //+  Notes
        //     241091 consulted Brian Wall.  For soil temperature an estimate of the water content of the total profile is required,
        //      not the plant extractable soil water.  Hence the method used here - difference between total lower limit and total soil
        //      water.  Here the use of lower limit is of no significance - it is merely a reference point, just as 0.0 could have been used.  jngh

        const double sw_avail_tot_min = 0.01;

        // get average bulk density and total sw
        double cum_depth = 0.0;
        double bd_tot = 0.0;
        double ll_tot = 0.0;
        double sw_dep_tot = 0.0;
        for (int layer = 0; layer < _dlayer.Length; layer++)
        {
            bd_tot += _bd[layer] * _dlayer[layer];
            ll_tot += _ll15_dep[layer];
            sw_dep_tot += _sw_dep[layer];
            cum_depth += _dlayer[layer];
        }
        double ave_bd = MathUtility.Divide(bd_tot, cum_depth, 0.0);

        // favbd ranges from almost 0 to almost 1
        double favbd = ave_bd / (ave_bd + 686.0 * Math.Exp(-5.63 * ave_bd));

        // damp_depth_max ranges from 1000 to almost 3500
        // It seems damp_depth_max is the damping depth potential.
        double damp_depth_max = Math.Max(0.0, 1000.0 + 2500.0 * favbd);

        // Potential sw above lower limit - mm water/mm soil depth
        //   Note that this function says that average bulk density can't go above 2.47222,
        //    otherwise potential becomes negative.  This function allows potential (ww) to go from 0 to .356
        double ww = Math.Max(0.0, 0.356 - 0.144 * ave_bd);

        // calculate amount of soil water, using lower limit as the reference point.
        double sw_avail_tot = Math.Max(sw_dep_tot - ll_tot, sw_avail_tot_min);

        // get fractional water content
        // wc can range from 0 to 1 while wcf ranges from 1 to 0
        double wc = MathUtility.Divide(sw_avail_tot, ww * cum_depth, 0.0);
        wc = Math.Max(0.0, Math.Min(1.0, wc));
        double wcf = (1.0 - wc) / (1.0 + wc);

        // Here b can range from -.69314 to -1.94575 and f ranges from 1 to  0.142878
        // When wc is 0, wcf=1 and f=500/damp_depth_max and soiln2_SoilTemp_DampDepth=500
        // When wc is 1, wcf=0 and f=1 and soiln2_SoilTemp_DampDepth=damp_depth_max
        //   and that damp_depth_max is the maximum.
        double b = Math.Log(MathUtility.Divide(500.0, damp_depth_max, 1.0e10));
        double f = Math.Exp(b * wcf * wcf);

        // Get the temperature damping depth. (mm soil/radian of a g_year)
        //   discount the potential damping depth by the soil water deficit.
        // Here soiln2_SoilTemp_DampDepth ranges from 500 to almost 3500 mm/58 days.
        return f * damp_depth_max;
    }
}
