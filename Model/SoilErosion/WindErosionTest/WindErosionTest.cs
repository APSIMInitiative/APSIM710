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
            model.sw = new double[1];
            model.bd = new double[1];
            model.bd[0] = 1.0;
            model.wind_height = 0.0;
            while ((line = file.ReadLine()) != null)
            {
                string[] fields = Regex.Split(line, @"\s+");
                if ((fields.Count() < 4) || (fields[0] == ""))
                    break;
                double time = Double.Parse(fields[0]);
                model.sw[0] = Double.Parse(fields[1]);
                model.plant_cover = Double.Parse(fields[2]);
                double windspeed = 100.0 * Double.Parse(fields[3]);
                double vertFlux = model.VerticalFlux(windspeed);
                if (vertFlux > 0.0)
                    Console.WriteLine(time.ToString() + ", " + model.totHorizFlux.ToString() + ", " + vertFlux.ToString());

            }

            file.Close();

        }
    }
}
