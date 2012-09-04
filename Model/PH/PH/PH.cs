using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
public class PHmodel
{
    [Output]
    public double NH3L;
    [Output]
    public double NH3A;
    [Output]
    public double NH4;
    [Output]
    public double H2S;
    [Output]
    public double CO2;
    [Output]
    public double FG;
    [Output]
    public double acetate;
    [Param]
    public double TAN=0;

    [Param]
    public double TIC;
    [Param]
    public double TS;
    [Param]
    public double TAc;
    [Param]
    public double Magnesium;
    [Param]
    public double Sodium;
    [Param]
    public double Potassium;
    [Param]
    public double Calcium;
    [Param]
    public double Cloride;
    [Param]
    public double Sulphate;
    [Param]
    public double H2O;
    [Param]
    public double pH;

 
  
    [Param]
    public double HS;
    [Param]
    public double HAc;
    [Param]
    public double Ac;
    [Param]
    public double OH;
    [Param]
    public double reduction_coeff;


    [Param]
    public double Ta;
    [Param]
    public double RE;
    [Param]
    public double Z;
    [Param]
    public double Z0;


    [Param]
    public double Uz;

    [Param]
    public double TLiq;


    [Param]
    public double Dether;





    public double GetHenrysCoeff(int compound, double liqTemperatureInKelvin) //Henry's coefficient, KH
    {
        double KH = 0.0;
        switch (compound)
        {
            case 0: //H2S
                KH = Math.Pow((5.703)-884.94/liqTemperatureInKelvin, 10);
                break;
            case 1: //ammonia NH3
                KH = Math.Pow((-1.69 * 1477.7 / liqTemperatureInKelvin), 10);
                break;
            case 2: //HAc
                KH = Math.Pow((3.65) - 2596 / liqTemperatureInKelvin, 10);
                break;
            case 3: //CO2
                KH = 358.4357 / ((Math.Exp(2441 * (1 / liqTemperatureInKelvin-1/298.15)))*liqTemperatureInKelvin);
                break;
            default: Console.WriteLine("Henry's coefficient not found for this compound");
                break;
        }
        return KH;
    }
    public double GetDissociationCoeff(int compound, double liqTemperatureInKelvin) //dissociation coeficient, K
    {
        double K = 0.0;

        switch (compound)
        {
            case 0: //H2S
                Math.Exp(-3448.7/liqTemperatureInKelvin+47.479-7.5227*Math.Log(liqTemperatureInKelvin));
                break;
            case 1: //ammonia NH3
                K = Math.Exp((-1843.22 / liqTemperatureInKelvin) - 0.0544943 * liqTemperatureInKelvin + 31.4335 * Math.Log(liqTemperatureInKelvin) - 177.95292);
                break;
            case 2: //HAc
                K = Math.Exp(-406.6*((3+Math.Pow(Math.E,liqTemperatureInKelvin/1846))/liqTemperatureInKelvin));
                break;
            case 3: //CO2 LNKCO2
                K = Math.Exp(-80063.5 / liqTemperatureInKelvin + 0.714984 * liqTemperatureInKelvin -478.653*Math.Log(liqTemperatureInKelvin)+2767.92);
                break;
            case 4: //CO2  LNKHC3
                K = Math.Exp(-6286.89 / liqTemperatureInKelvin - 0.050627 * liqTemperatureInKelvin - +12.405);
                break;
            case 5: //H2O
                K = Math.Exp(-10294.83/liqTemperatureInKelvin-0.039282*liqTemperatureInKelvin+14.01708);
                break;
           

            default: Console.WriteLine("Dissociation coefficient not found for this compound");
                break;
        }
        return K;
    }
    public double GetDiffusionCoeffAir(int compound, double airTemperatureInKelvin) //diffusion coefficient for air
    {
        double DaG = 0.0;
        double Ma = 29.0; //Air
        double MG = 0.0;
        double sumVG = 0.0;
        double sumVA = 21.0;//Air
        double pressure = 1.0;
        switch (compound)
        {
            case 0: //H2S
                MG = 34;
                sumVG = 20.96;
                break;
            case 1: //ammonia NH3
                MG = 17.0;
                sumVG = 11.63;
                break;
            case 2: //HAc
                MG = 48;
                sumVG = 51.88;
                break;
            case 3: //CO2
                MG = 27.46;
                sumVG = 44;
                break;

            default: Console.WriteLine("Diffusion coefficient not found for this compound");
                break;
        }
        DaG = (Math.Pow(10, -7) * Math.Pow(airTemperatureInKelvin, 1.75) * Math.Pow(1 / MG + 1 / Ma, 0.5))
                    / (pressure * Math.Pow(Math.Pow(sumVG, 1 / 3) + Math.Pow(sumVA, 1 / 3), 2));
        return DaG;
    }

    public double GetDiffusionCoeffLiq(int compound, double liqTemperatureInKelvin, double viscosity) //diffusion coefficient for liquid
    {
        double DG = 0.0;
        double MH2O = 18.0;
        double Vm = 0.0;
        switch (compound)
        {
            case 0: //H2S
                Vm = 33;
                break;
            case 1: //HS
                Vm = 29.3;
                break;
            case 2: //NH3
                Vm = 25.8;
                break;
            case 3: //NH4
                Vm = 29.5;
                break;
            case 4: //HAc
                Vm = 63.8;
                break;
            case 5: //Ac
                Vm = 64.7;
                break;
            case 6: //CO2
                Vm = 34;
                break;
            case 7: //HC03
                Vm = 49.9;
                break;
            case 8: //OH
                Vm = 15.7;
                break;
            case 9: //H
                Vm = 3.7;
                break;

            default: Console.WriteLine("Diffusion coefficient not found for this compound");
                break;
        }
        DG = 7.4 * liqTemperatureInKelvin * Math.Pow(10, -8) * Math.Pow(2.26 * MH2O, 1 / 2) / (viscosity * Math.Pow(Vm, 0.6));
        return DG;
    }
    void SolveCubic(double  a, double  b,double  c,double  d,ref int solutions,ref double[]  x)
{
      double    a1 = b/a, a2 = c/a, a3 = d/a;
      double    Q = (a1*a1 - 3.0*a2)/9.0;
      double R = (2.0*a1*a1*a1 - 9.0*a1*a2 + 27.0*a3)/54.0;
      double    R2_Q3 = R*R - Q*Q*Q;

      double    theta;

      if (R2_Q3 <= 0)
      {
            solutions = 3;
            theta = Math.Acos(R / Math.Sqrt(Q * Q * Q));
            x[0] = -2.0 * Math.Sqrt(Q) * Math.Cos(theta / 3.0) - a1 / 3.0;
            x[1] = -2.0 * Math.Sqrt(Q) * Math.Cos((theta + 2.0 * Math.PI) / 3.0) - a1 / 3.0;
            x[2] = -2.0 * Math.Sqrt(Q) * Math.Cos((theta + 4.0 * Math.PI) / 3.0) - a1 / 3.0;
      }
      else
      {
            solutions = 1;
            double inputOne = Math.Sqrt(R2_Q3) + R;
			double inputTwo=1/3.0;
            x[0] = Math.Pow(inputOne, inputTwo);
            x[0] += Q/x[0];
            x[0] *= (R < 0.0) ? 1 : -1;
            x[0] -= a1/3.0;
      }
}
    bool SolveQuadratic(bool posRoot, double a, double b1, double c, ref double  x)
{
   if ((b1*b1-4*a*c)<0)
   {
   	return false;
   }
	else if (posRoot)
       x = (-Math.Sqrt(b1 * b1 - 4 * a * c)) / (2 * a);
   else
   	x = (-b1-Math.Sqrt(b1*b1-4*a*c))/(2*a);
   return true;
}
    [EventHandler]
    public void OnPrepare()
    {
      
        NH3L=TAN/(1+(Math.Pow(10,-pH)/GetDissociationCoeff(1,TLiq)));
        NH4 = TAN / (1 + GetDissociationCoeff(1, TLiq) / (Math.Pow(10, -pH)));
        TAN = NH3L + NH4; //should not be necessary...
        H2S=TS/(1+GetDissociationCoeff(0,TLiq)/Math.Pow(10,-pH));
        TS=H2S+HS;
        HS = TS / (Math.Pow(10, -pH) / GetDissociationCoeff(0, TLiq) + 1);
        HAc = TAc / (Math.Pow(10, -pH) / GetDissociationCoeff(2, TLiq) + 1);
        TAc=HAc+Ac;
        Ac = TAc / (Math.Pow(10, -pH) / GetDissociationCoeff(2, TLiq) + 1);
        CO2 = TIC / (1 + GetDissociationCoeff(3, TLiq) / Math.Pow(10, -pH) + (GetDissociationCoeff(4, TLiq) * GetDissociationCoeff(3, TLiq)) / Math.Pow(10, -2 * pH));
        TIC = CO2 + GetDissociationCoeff(4, TLiq) + CO2;
        OH = GetDissociationCoeff(5,TLiq)/Math.Pow(10,-pH);
    }

    [EventHandler]
    public void OnProcess()
    {
        
        double vonKarman = 0.41; //is a constant
        //in the air
        double transport_coeff_air=0;
        double transport_coeff_liq = 0;
        double air_density = (353 / Ta) * (760 - 0.3783 * RE * Math.Pow(Math.E, 0.0596 * Ta - 14.6135)) / 760; //density of air, Pa, kg/m**3
        double air_viscosity = 0.3787 * Math.Pow(Ta, 0.683) * -10000000;   //viscosity of air, Ua, N s/m**2
        double friction_velocity_air = vonKarman * Uz / Math.Log10(Z / Z0);  //friction velocity in air, UstarA, m/s
        double viscosity_liquid = 1297.3 - 16.237 * TLiq + 0.0777 * Math.Pow(TLiq, 2) * -0.0003 * Math.Pow(TLiq, 3) * Math.Pow(10, -7) * Math.Pow(TLiq, 4);//viscosity of liquid, ULiq, N s/m**2
        double liq_density = 242.92 + 6.6793 * TLiq - 0.0185 * Math.Pow(TLiq, 2) + 2 * Math.Pow(TLiq, 3) * Math.Pow(10, -5); //density of liquid, PLiq, kg/m**3
        //loop through the four G compounds
        for (int i = 1; i < 2; i++)  //should be 0 to <4
        {
           
            double Gion= 0.0;
            double Wion=0.0;
            double diffusion_coeff_G_liq = 0.0;//diffusion coefficient of G in liquid, DG, m**2/s
            double diffusion_coeff_Gion_liq = 0.0; //diffusion coefficient of Gion in liquid, DG, m**2/s
            double diffusion_coeff_Wion_liq = 0.0;  //diffusion coefficient of Wion in liquid, DG, m**2/s
            double Gliq=0;
            double GairInf = 0;
            switch (i)
            {
                case 1:                    //ammonia
                    Gliq = NH3L;
                    Gion = NH4;
                    Wion = OH;
                    GairInf = NH3A;
                    diffusion_coeff_G_liq = GetDiffusionCoeffLiq(2, TLiq, viscosity_liquid);   //NH3
                    diffusion_coeff_Gion_liq = GetDiffusionCoeffLiq(3, TLiq, viscosity_liquid);//NH4
                    diffusion_coeff_Wion_liq = GetDiffusionCoeffLiq(8, TLiq, viscosity_liquid); //
                    break;

            }

            double diffusion_G_air = GetDiffusionCoeffAir(i, Ta);   //diffusion coefficient, DaG, m**2/s
            double HenrysCoefficient = GetHenrysCoeff(i, TLiq);
            double Schmidt_G_air = air_viscosity / (air_density * diffusion_G_air);   //Schmidt number for air, Sca, dimensionless
            double transport_coeff_G_air = 10E-3 + 0.042 * friction_velocity_air * Math.Pow(Schmidt_G_air, 0.67); //transport coefficient of G for air, kaG, m/s
            double dissociation_coeff_G = GetDissociationCoeff(i, TLiq); // dissociation coefficient of G in liquid
            double Schmidt_G_liq = viscosity_liquid / (liq_density * diffusion_coeff_G_liq);
            double transport_coeff_G_liq = 0;  // transport coefficient for G in liquid, kLiqg, m/s
            if (friction_velocity_air > 0.3)
                transport_coeff_G_liq = 0.001 + -341 * friction_velocity_air * Math.Pow(Schmidt_G_liq, -0.5);
            else if (0.093 < friction_velocity_air && friction_velocity_air < 0.3)
                transport_coeff_G_liq = 0.001 + -1440000 * Math.Pow(friction_velocity_air, 2.2) * Math.Pow(Schmidt_G_liq, -0.5);
            else if (friction_velocity_air < 0.093)
                transport_coeff_G_liq = 0.00000278 * Math.Pow(diffusion_coeff_G_liq / Dether, 2 / 3);
            double A = 2 * diffusion_coeff_G_liq * Gliq;
            double C = 2 * diffusion_coeff_Gion_liq * Gion;
            double thickness_liq_layer = (reduction_coeff * diffusion_coeff_G_liq) / transport_coeff_G_liq; //thickness of liquid layer, tl, m
            double beta = diffusion_coeff_Gion_liq * Gion - diffusion_coeff_G_liq * Wion;
            double D = 2 * thickness_liq_layer * transport_coeff_liq * GairInf;
            double F = diffusion_coeff_Gion_liq * diffusion_coeff_Wion_liq * dissociation_coeff_G;
            double Y = A * (A + 2 * C - 2 * beta + 2 * D) + C * (C - 2 * beta + 2 * D) + D * (D - 2 * beta);
            double R = thickness_liq_layer * transport_coeff_G_air * HenrysCoefficient;
          
            double result = 0;
            double quadratic_coeff = 4 * Math.Pow(diffusion_coeff_G_liq,2) + Math.Pow(R, 2) + 2 * diffusion_coeff_G_liq * R;
            double linear_coeff = -4 * (diffusion_coeff_G_liq*(A + C + D - beta) + R * (A - beta + D) + F);
            SolveQuadratic(true,quadratic_coeff, linear_coeff, Y, ref result);
            Gliq = result;
            switch (i)
            {
                case 1:                    //ammonia
                    TAN = -transport_coeff_air * 10000 * (NH3L - HenrysCoefficient * Gliq);
                    break;

            }

        }
        double YH=Sodium + Potassium + 2 * Calcium + 2 * Magnesium - Cloride -2 * Sulphate;
        double KW = 0, H2CO3 = 0;
        double X = KW + H2CO3 * GetDissociationCoeff(6, TLiq) + H2S * GetDissociationCoeff(1, TLiq) + HAc * GetDissociationCoeff(2, TLiq);
        double Q = 2 * H2CO3 * GetDissociationCoeff(6, TLiq) * GetDissociationCoeff(3, TLiq);
        int solutions = 0;
        double[] results = new double[3];
        SolveCubic(1 + NH3L / GetDissociationCoeff(1, TLiq), YH, -X, -Q, ref solutions, ref results);
   
        pH = Math.Log10(1 / results[0]);
    }
}

