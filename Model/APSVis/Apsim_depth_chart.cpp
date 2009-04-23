#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "APSIM_depth_chart.h"
#include "scatter_format.h"

// ------------------------------------------------------------------
//  Short description:
//    initialise

//  Notes:

//  Changes:
//    DPH 2/10/1997
//    dph 10/3/98 added "accumulated_value = 0" for safety

// ------------------------------------------------------------------
APSIM_depth_chart::APSIM_depth_chart (TForm* parent)
   : Depth_chart (parent)
   {
   Current_layer_number = 1;
   Num_layers = 1000;
   Accumulated_value = 0.0;
   }

// ------------------------------------------------------------------
//  Short description:
//    called to retrieve the next xy point.  Return true if this is
//    the last point for this plot.

//  Notes:
//    Assumes table_ptr is an open table.

//  Changes:
//    DPH 18/4/1997
//    dph 10/3/98 Reset accumulated_value to 0 when finished with
//                this variable. D-127

// ------------------------------------------------------------------
bool APSIM_depth_chart::Get_next_point (Table_base& Table_obj,
                                        char* x_value,
                                        char* y_value)
   {
   char name[100];
   char array_spec[10];

   // get x value
   sprintf(array_spec, "(%i)", Current_layer_number);
   strcpy(name, XYs[Current_xy_index]->X_column_name.c_str());
   strcat(name, array_spec);
   Table_obj.Get_data_by_name (name, x_value);

   // get y value
   strcpy(name, XYs[Current_xy_index]->Y_column_name.c_str());
   strcat(name, array_spec);
   Table_obj.Get_data_by_name (name, y_value);

   // accumulate y values.
   double thickness = atof(y_value);
   Accumulated_value += thickness;
   sprintf(y_value, "%7.1f", Accumulated_value - thickness / 2.0);

   // increment current layer number
   Current_layer_number++;
   if (Current_layer_number > Num_layers)
      {
      Current_layer_number = 1;
      Accumulated_value = 0.0;
      }

   return (Current_layer_number == 1);
   }

// ------------------------------------------------------------------
//  Short description:
//    called at beginning of data reading loop to let derived
//    classes initialise things.  We use it to calculate the number
//    of layers we're dealing with.

//  Notes:

//  Changes:
//    DPH 21/4/1997

// ------------------------------------------------------------------
void APSIM_depth_chart::Initialise_xys (Table_base& Table_obj)
   {
   Depth_chart::Initialise_xys (Table_obj);

   char name[100];
   char value[50];
   char array_spec[10];

   // keep looping through all x variables until we cannot find it.
   bool Found = true;
   Num_layers = 0;
   while (Found)
      {
      Num_layers++;
      sprintf(array_spec, "(%i)", Num_layers);
      strcpy(name, XYs[0]->X_column_name.c_str());
      strcat(name, array_spec);
      Table_obj.Get_data_by_name (name, value);
      Found = (value[0] != 0);
      }
   Num_layers--;
   }

