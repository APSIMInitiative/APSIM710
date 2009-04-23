#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "drawable.h"

#include "Axis.h"
#include <values.h>
/**# implementation Axis:: id(C_0853986263)
*/
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
Axis::Axis (void)
   {
   Minimum_fixed = false;
   Maximum_fixed = false;
   Increment_fixed = false;
   Grid_visible = false;
   Inverted = false;
   Minimum_data = MAXDOUBLE;
   Maximum_data = MAXDOUBLE;
   }

// ------------------------------------------------------------------
//  Short description:
//    communicate with tchart

//  Notes:

//  Changes:
//    DPH 6/2/1997

// ------------------------------------------------------------------
void Axis::Design(TChart* TChart_ptr)
   {
   TChartAxis* Axis_ptr;
   switch (Axis_type)
      {
      case Left : Axis_ptr = TChart_ptr->LeftAxis;      break;
      case Top : Axis_ptr = TChart_ptr->TopAxis;        break;
      case Right : Axis_ptr = TChart_ptr->RightAxis;    break;
      case Bottom : Axis_ptr = TChart_ptr->BottomAxis;  break;
      };


   double min, max;
   Axis_ptr->CalcMinMax(min, max);
   if (max-min < 1 && !Increment_fixed)
      {
      Increment_fixed = true;
      Increment = 0.1;
      }
   if (Minimum_fixed)
      {
      Axis_ptr->Minimum = Minimum;
      Axis_ptr->AutomaticMinimum = false;
      }

   if (Maximum_fixed)
      {
      Axis_ptr->Maximum = Maximum;
      Axis_ptr->AutomaticMaximum = false;
      }

   if (Increment_fixed)
      Axis_ptr->Increment = Increment;

   if (Minimum_data != MAXDOUBLE)
      {
      Axis_ptr->Maximum = Maximum_data;
      Axis_ptr->Minimum = Minimum_data;
      Axis_ptr->AutomaticMinimum = true;
      }
   if (Maximum_data != MAXDOUBLE)
      {
      Axis_ptr->Maximum = Maximum_data;
      Axis_ptr->Minimum = Minimum_data;
      Axis_ptr->AutomaticMaximum = true;
      }

   Axis_ptr->Grid->Visible = Grid_visible;
   Axis_ptr->DateTimeFormat = "dd/mm/yy";
   Axis_ptr->Inverted = Inverted;
   Axis_ptr->AxisValuesFormat = "###0.###";
   }
   
