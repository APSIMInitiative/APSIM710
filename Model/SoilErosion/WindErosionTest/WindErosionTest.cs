using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace WindErosionTest
{
    class Program
    {
        static void Main(string[] args)
        {
            System.IO.StreamReader file =
                new System.IO.StreamReader(args[0]);
            string line;
            SoilErosion model = new SoilErosion();
            model.wind_model = "shao";
            //model.flux_model = "white";
            model.sw = new double[1];
            model.bd = new double[1];
            model.bd[0] = 1.0;
            model.wind_height = 3.0;
            //model.soil_type = "silt_loam";
            model.moisture_option = 1;

            model.fraction_clay = 0.0976;
            model.fraction_silt = 0.5179;
            model.fraction_fine_sand = 0.2454;
            model.max_clay_radius = 1.0;
            model.max_silt_radius = 25.0;
            model.max_fine_sand_radius = 50;
            //model.n_classes = 200;
            //model.dust_cutoff = 20;

            double time_step = 60; // seconds per time step
            // model.coeff_flux = 7.57 / 2.0;  // Use Shao's value, but compensate (roughly) for his incorrect White algorithm
            double horizTot = 0.0;
            double vertTot = 0.0;
            while ((line = file.ReadLine()) != null)
            {
                string[] fields = Regex.Split(line, @"\s+");
                if ((fields.Count() < 4) || (fields[0] == ""))
                    break;
                double time = Double.Parse(fields[0]);
                model.sw[0] = Double.Parse(fields[1]);
                //model.sw[0] = 0.017;
                model.plant_cover = Double.Parse(fields[2]);
                //model.plant_cover = 0.29;
                double windspeed = 100.0 * Double.Parse(fields[3]);
                Console.Write(time.ToString());
                double vertFlux = model.VerticalFlux(windspeed);   
                Console.WriteLine();
                if (vertFlux > 0.0)
                {
                    Console.WriteLine(time.ToString() + ", " + model.totHorizFlux.ToString() + ", " + vertFlux.ToString());
                    horizTot += model.totHorizFlux * time_step;
                    vertTot += vertFlux * time_step;
                }

            }

            file.Close();
            Console.WriteLine("Total fluxes, " + horizTot.ToString() + ", " + vertTot.ToString());
        
        }
    }
}
