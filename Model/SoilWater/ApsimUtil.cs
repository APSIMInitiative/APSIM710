using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

   class ApsimUtil
      {
      
      //.net component interface alreay has the fatal_error. You just do a "throw new Exception("message");
      //but does not have a way to throw a warning message yet. So do it yourself below.
      static public void warning_error(string WarningMessage)
         {
         Console.WriteLine("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         Console.WriteLine("                 APSIM Warning Error              ");
         Console.WriteLine("                 -------------------              ");
         Console.WriteLine(WarningMessage);
         Console.WriteLine("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
         }


      //TODO: where does this come from? I am taking a total guess that this is what it does.
      //TODO: I am guessing that it checks if it is between the bounds and then sends a warning to the summary file it is outside
      //      the bounds. I assume that is why they have the final parameter which is a string. 
      //      I am guessing it is used in the message to the summary file to specify which variable was outside the bounds. 
      //      Unlike u_bound and l_bound it does not force the variable to be between the bounds. It just warns the user in the summary file.
      static public void bound_check_real_var(double Variable, double LowerBound, double UpperBound, string VariableName)
         {
         string warningMsg;
         if(Variable > UpperBound)
            {
            warningMsg = "The variable: /'" + VariableName + "/' is above the expected upper bound of: " + UpperBound;
            warning_error(warningMsg);
            }
         if(Variable < LowerBound)
            {
            warningMsg = "The variable: /'" + VariableName + "/' is below the expected lower bound of: " + LowerBound;
            warning_error(warningMsg);
            }
         }

      //TODO: total guess. Especially if it is ElementToStopChecking or it should be ElementsReadSuccessfully?
      static public void bound_check_real_array(double[] A, double LowerBound, double UpperBound, string ArrayName, int ElementToStopChecking) 
         {
         for (int i=au.si; i<=au.ci(ElementToStopChecking); i++)
            {
            bound_check_real_var(A[i], LowerBound, UpperBound, ArrayName + "(" + au.cl(i) + ")");
            }
         }


      static public double add_cover(double cover1, double cover2)
         {
         //!+ Sub-Program Arguments
         //   real       cover1                ! (INPUT) first cover to combine (0-1)
         //   real       cover2                ! (INPUT) second cover to combine (0-1)

         //!+ Purpose
         //!     Combines two covers

         //!+  Definition
         //!     "cover1" and "cover2" are numbers between 0 and 1 which
         //!     indicate what fraction of sunlight is intercepted by the
         //!     foliage of plants.  This function returns a number between
         //!     0 and 1 indicating the fraction of sunlight intercepted
         //!     when "cover1" is combined with "cover2", i.e. both sets of
         //!     plants are present.

         //!+  Mission Statement
         //!     cover as a result of %1 and %2

         double     bare;     //! bare proportion (0-1)

         bare = (1.0 - cover1)*(1.0 - cover2);
         return (1.0 - bare);

         }



      static public double root_proportion(int Layer, double[] Dlayer, double RootDepth)
         {

         //integer    layer                 ! (INPUT) layer to look at
         //real       dlayr(*)              ! (INPUT) array of layer depths
         //real       root_depth            ! (INPUT) depth of roots

         //!+ Purpose
         //!       returns the proportion of layer that has roots in it (0-1).

         //!+  Definition
         //!     Each element of "dlayr" holds the height of  the
         //!     corresponding soil layer.  The height of the top layer is
         //!     held in "dlayr"(1), and the rest follow in sequence down
         //!     into the soil profile.  Given a root depth of "root_depth",
         //!     this function will return the proportion of "dlayr"("layer")
         //!     which has roots in it  (a value in the range 0..1).

         //!+  Mission Statement
         //!      proportion of layer %1 explored by roots

         double       depth_to_layer_bottom;  //! depth to bottom of layer (mm)
         double       depth_to_layer_top;     //! depth to top of layer (mm)
         double       depth_to_root;          //! depth to root in layer (mm)
         double       depth_of_root_in_layer; //! depth of root within layer (mm)




         depth_to_layer_bottom = au.sum_real_array(Dlayer, Layer);
         depth_to_layer_top = depth_to_layer_bottom - Dlayer[au.ci(Layer)];
         depth_to_root  = Math.Min(depth_to_layer_bottom, RootDepth);

         depth_of_root_in_layer = mu.dim(depth_to_root, depth_to_layer_top);
         return mu.divide(depth_of_root_in_layer, Dlayer[au.ci(Layer)], 0.0);

         }

/*

      static public double linear_interp_real (canopy_height[crop], canopy_fact_height, canopy_fact, num_canopy_fact)
         {
   
         }

      //TODO: where does this come from?
      static public double add_cover(cover_surface_crop, effective_crop_cover)
         {
         //sv- total guess that this is what it does


         }
*/
      static public double sum_cover_array(double[] cover, int num_covers)
         {
         //! (INPUT) cover array (0-1)
         //! (INPUT) number of covers in array

         return 0.0;  //TODO: don't implement this yet  because we have a fallow waterbalance with no plants yet.

         }

      static public bool date_within(string WinterDate, string SummerDate, Double TodayJulianDay)
         {

         //nb. I think today has to be a Julian Day (and not "day of year") to account for leap years which might throw off matching "day of year" 
         //    to "1-Apr" "1-Nov" because in leap years 1-Apr and 1-Nov will have different "day of year" values to non leap years.

         //TODO: finish this off.

         //sv- NB. This is not a proper conversion. I assume that the StartDateString and EndDateString are in the format "1-Apr" with no year.
         //       This is because the only place I use it is in soilwat2_ritchie_evaporation() which only needs to check if today is between,
         //       WinterDate and SummerDate, so that it knows to use either WinterU/WinterCona or SummerU/SummerCona. The year is irrelavant.
         //       It was just taking too long to figure out a way to check all the different formats that StartDateString, EndDateString could
         //       be in to see if they have a year. Because by default it uses current year (time of computer clock not current time of apsim simulation)
         //       but you can't just assume if the startdate and enddate are the current year then it was not present because the user might have actually
         //       specified that year as a legitamate value. So the only way is to try to check startdate and enddate for separators and see if you can get
         //       a year that way. But there are just too many date formats.

         DateTime winterDT;
         DateTime summerDT;
         int      winterDOY;
         int      summerDOY;
         int      todayDOY;
         int      todayYear;


         ApsimUtil.jday_to_day_of_year(TodayJulianDay, out todayDOY, out todayYear);
         //append the current year to the Winter "1-Apr" and Summer "1-Nov" strings.
         //so when datetime is converted to "day of year" it will compensate for leap years etc.
         WinterDate = WinterDate + "-" + todayYear;
         SummerDate = SummerDate + "-" + todayYear;

         try 
            {
            winterDT = DateTime.Parse(WinterDate); 
            summerDT = DateTime.Parse(SummerDate);
            }
         catch (FormatException)
            {
            throw new Exception("Cannot convert either winterdate: " + WinterDate + " or summerdate: " + SummerDate + " for U and Cona specified in your soil to a date."); 
            }

         winterDOY = winterDT.DayOfYear;
         summerDOY = summerDT.DayOfYear;
         

         //TODO: I have had to change this because the SoilWat fortran code does not do what it says it does. 
         //If you look at u and cona values and the dates they kick in on (1-Apr, 1-Nov). They do not match the logic in the fortran code. 
         //The code below does not match the fortan code because I am recreating its incorrect behaviour not its logic.
         //When you make the code below match the fortran code it behaves correctly and so does NOT match the incorrect fortran behaviour.
         //So I have changed the code so it matches the incorrect behaviour. Turn sumes1, sumes2 into output variables so you can see them in the output file.

         ////fortran code does not take into consideration leap year so I have to reintroduce this wrong behaviour so my code will match.
         //if (DateTime.IsLeapYear(todayYear))
         //   {
         //   todayDOY = todayDOY + 1;
         //   }


         if (winterDOY <= summerDOY)
            {
            //if startDOY and endDOY are within the same year
            return ((todayDOY >= winterDOY) && (todayDOY <= summerDOY));
            }
         else
            {
            //if startDOY is last year, and endDOY is this year
            return ((todayDOY >= winterDOY) || (todayDOY <= summerDOY));
            }

         }

      static public void jday_to_day_of_year(Double JulianDay, out int DOY, out int Year)
      {

          //sv- Julian Day is NOT Day of Year. It is a common misonception that Julian Date is DOY but it is incorrect.
          //    Julian Day is days since 1 January 4713 BC Greenwich noon, Julian proleptic calendar. Expressed as a double -> days_since_4713BC.fraction_of_day
          //    The correct term is Ordinal Date or just plain old 'Day of Year'. 
          //    See http://en.wikipedia.org/wiki/Julian_day and http://en.wikipedia.org/wiki/ISO_8601#Ordinal_dates

          DateUtility.JulianDayNumberToDayOfYear((int)Math.Truncate(JulianDay), out DOY, out Year);
      }
}

