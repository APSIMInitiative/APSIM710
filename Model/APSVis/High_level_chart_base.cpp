#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "high_level_chart_base.h"
#include "bar_format.h"
static const char* MISSING_VALUE = "-";
#include <stdlib.h>

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997
//    dph 10/3/98 added accumulate x and y options C-081

// ------------------------------------------------------------------
High_level_chart_base::XY_pair::XY_pair (const char* p_X_column_name,
                  const char* p_Y_column_name,
                  Field_type_enum p_X_type,
                  Field_type_enum p_Y_type,
                  X_axis_link_enum p_x_axis_link,
                  Y_axis_link_enum p_y_axis_link,
                  Format_base* p_Format_ptr,
                  const char* p_Title,
                  bool p_Accumulate_x,
                  bool p_Accumulate_y)
   {
   X_column_name = p_X_column_name;
   Y_column_name = p_Y_column_name;
   X_type = p_X_type;
   Y_type = p_Y_type;
   x_axis_link = p_x_axis_link;
   y_axis_link = p_y_axis_link;
   Format_ptr = p_Format_ptr;
   Title = p_Title;
   Accumulate_x = p_Accumulate_x;
   Accumulate_y = p_Accumulate_y;
   Accumulated_x = 0.0;
   Accumulated_y = 0.0;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997
//    dph 10/3/98 added accumulate x and y options C-081

// ------------------------------------------------------------------
High_level_chart_base::XY_pair::XY_pair (void)
   {
   X_type = numeric;
   Y_type = numeric;
   x_axis_link = bottom;
   y_axis_link = ::left;
   Format_ptr = NULL;
   Accumulate_x = false;
   Accumulate_y = false;
   Accumulated_x = 0.0;
   Accumulated_y = 0.0;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
High_level_chart_base::XY_pair::~XY_pair (void)
   {
   if (Format_ptr != NULL)
      delete Format_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
High_level_chart_base::High_level_chart_base(TForm* parent)
   {
   Chart_ptr = new Chart_base(parent);
   Own_chart_ptr = true;
   Num_xys = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
High_level_chart_base::~High_level_chart_base(void)
   {
   Clear_xys();
   if (Own_chart_ptr)
      delete Chart_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    return number of plots to caller.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
int High_level_chart_base::Get_num_plots(void)
   {
   return Chart_ptr->Get_num_plots();
   }

// ------------------------------------------------------------------
//  Short description:
//    return number of plots to caller.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
bool High_level_chart_base::Something_to_draw(void)
   {
   return (Num_xys > 0);
   }

// ------------------------------------------------------------------
//  Short description:
//    return number of plots to caller.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_chart_base::Clear_xys (void)
   {
   // loop through all xys
   for (int xy_number = 0; xy_number < Num_xys; xy_number++)
      delete XYs[xy_number];

   Num_xys = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//    return number of plots to caller.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_chart_base::Set_splitting_field(const char* Splitting_f)
   {
   Splitting_field = Splitting_f;
   }

// ------------------------------------------------------------------
//  Short description:
//        add an xy pair to list.

//  Notes:

//  Changes:
//    DPH 18/4/1997
//    dph 10/3/98 make sure all bar formats go to beginning of
//                XYs array.  d-123
//    dph 10/3/98 added accumulate x and y options C-081

// ------------------------------------------------------------------
void High_level_chart_base::Add_xy_pair (const char* X_field_name,
                                    const char* Y_field_name,
                                    Field_type_enum X_type,
                                    Field_type_enum Y_type,
                                    X_axis_link_enum X_axis_link,
                                    Y_axis_link_enum Y_axis_link,
                                    Format_base* Format_ptr,
                                    const char* Title,
                                    bool Accumulate_x,
                                    bool Accumulate_y)
   {
   // make sure we have enough space for another xy pair.
   if (Num_xys >= MAX_XY_PAIRS)
      {
      char msg[200];
      sprintf(msg, "Too many xy pairs.  There is a maximum of %i xy pairs", MAX_XY_PAIRS);
      throw string(msg);
      }

   // are we dealing with a bar format object?
   int New_position;
   if (dynamic_cast<Bar_format*> (Format_ptr) != NULL)
      {
      // yes we are.  we need to move all elements of XYs array down 1
      // position to make room for this series at the top of the array.
      for (int i = Num_xys-1; i >= 0; i--)
         XYs[i+1] = XYs[i];

      New_position = 0;
      }
   else
      New_position = Num_xys;

   // create xy pair object and add to our list.
   XYs[New_position] = new High_level_chart_base::XY_pair (X_field_name,
                                                 Y_field_name,
                                                 X_type,
                                                 Y_type,
                                                 X_axis_link,
                                                 Y_axis_link,
                                                 Format_ptr,
                                                 Title,
                                                 Accumulate_x,
                                                 Accumulate_y);
   Num_xys++;
   }

// ------------------------------------------------------------------
//  Short description:
//    create a series of plots from data in the specified database.

//  Notes:

//  Changes:
//    DPH 21/4/1997

// ------------------------------------------------------------------
void High_level_chart_base::Create_plots_from_database (Database& DB)
   {
   // loop through each table in database.
   for (int Table_number = 0;
            Table_number < DB.Count_tables();
            Table_number++)
      {
      Create_plots_from_table ((*DB.Get_table (Table_number)));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    create a series of plots from data in the specified table.

//  Notes:
//    Assumes that the table passed in is closed.

//  Changes:
//    DPH 21/4/1997
//    dph 7/4/98 added a better check for missing numbers.  Previously
//               a negative number was considered missing D-131 

// ------------------------------------------------------------------
void High_level_chart_base::Create_plots_from_table (Table_base& Table_obj)
   {
   Plot* Plot_ptr;
   char x_value[100], y_value[100];

   // open the table.
   Table_obj.Open();

   // initialise the xy pair objects.
   Initialise_xys (Table_obj);

   // loop through all x/y points.
   while (More_xy_points(Table_obj))
      {
      // get next point and the series to send them to.
      Get_next_point_and_plot (Table_obj, x_value, y_value, &Plot_ptr);

      // do we have both x and y values?
      if (strlen(x_value) > 0 && strlen(y_value) > 0)
         {
         // yes - do we have missing x or y values?
         if (strcmpi(x_value, MISSING_VALUE) != 0 &&
             strcmpi(y_value, MISSING_VALUE) != 0)
            {
            // no - add data to plot.
             Plot_ptr->Data_series->Add_data (x_value, y_value);
            }
         }
      }

   // close the table.
   Table_obj.Close();
   }

// ------------------------------------------------------------------
//  Short description:
//    loop through all xy pairs and initialise each one.

//  Notes:

//  Changes:
//    DPH 21/4/1997

// ------------------------------------------------------------------
void High_level_chart_base::Initialise_xys (Table_base& Table_obj)
   {
   Current_xy_index = 0;
   for (int XY_number = 0; XY_number < Num_xys; XY_number++)
      {
      XYs[XY_number]->x_index = Table_obj.Get_field_index (XYs[XY_number]->X_column_name.c_str());
      XYs[XY_number]->y_index = Table_obj.Get_field_index (XYs[XY_number]->Y_column_name.c_str());
      XYs[XY_number]->Accumulated_x = 0.0;
      XYs[XY_number]->Accumulated_y = 0.0;
      }

   if (Need_to_create_new_plots(Table_obj))
      {
      // yes go create plots
      Create_plots(Table_obj.Get_title().c_str());
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    returns true if there are more xy points to go.

//  Notes:
//    Assumes table_ptr is an open table.

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
bool High_level_chart_base::More_xy_points (Table_base& Table_obj)
   {
   return (!Table_obj.At_end_of_data());
   }

// ------------------------------------------------------------------
//  Short description:
//    called to retrieve the next xy point.

//  Notes:
//    Assumes table_ptr is an open table.

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_chart_base::Get_next_point_and_plot (Table_base& Table_obj,
                                          char* x_value,
                                          char* y_value,
                                          Plot** Plot_ptr)
   {
   // get next x and y point.
   bool Last_point = Get_next_point (Table_obj, x_value, y_value);

   // return pointer to plot to caller.
   *Plot_ptr = XYs[Current_xy_index]->Plot_ptr;

   // advance index to next xy pair if necessary.
   if (Last_point)
      {
      Current_xy_index++;

      // goto next record in table?
      if (Current_xy_index >= Num_xys)
         {
         // yes - advance table to next record and reset our index.
         Table_obj.Next();
         Current_xy_index = 0;

         // do we need to create new plots?
         if (Need_to_create_new_plots(Table_obj))
            {
            // yes go create plots
            Create_plots(Table_obj.Get_title().c_str());
            }
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     Get the next x and y point to plot.  Returns true if this
//     is the last point for this plot.

//  Notes:

//  Changes:
//    DPH 18/4/1997
//    dph 10/3/98 instead of throwing an exception when the x_index
//                and y_index are invalid, I have simply returned a
//                blank string.  D-124
//    dph 10/3/98 added logic to accumulate variables when necessary C-081

// ------------------------------------------------------------------
bool High_level_chart_base::Get_next_point (Table_base& Table_obj,
                                       char* x_value,
                                       char* y_value)
   {
   // get x value.
   if (XYs[Current_xy_index]->x_index < 0)
      strcpy(x_value, "");
   else
      {
      Table_obj.Get_data_by_index (XYs[Current_xy_index]->x_index, x_value);
      if (XYs[Current_xy_index]->Accumulate_x)
         {
         XYs[Current_xy_index]->Accumulated_x += atof(x_value);
         gcvt(XYs[Current_xy_index]->Accumulated_x, 3, x_value);
         }
      }

   if (XYs[Current_xy_index]->y_index < 0)
      strcpy(y_value, "");
   else
      {
      Table_obj.Get_data_by_index (XYs[Current_xy_index]->y_index, y_value);
      if (XYs[Current_xy_index]->Accumulate_y)
         {
         XYs[Current_xy_index]->Accumulated_y += atof(y_value);
         gcvt(XYs[Current_xy_index]->Accumulated_y, 3, y_value);
         }
      }

   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if caller needs to create a new set of plots/series
//    for each xy pair.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
bool High_level_chart_base::Need_to_create_new_plots (Table_base& Table_obj)
   {
   bool Do_create_plots = false;

   if (Splitting_field.length() > 0)
      {
      // get current splitting field.
      char value[100];
      Table_obj.Get_data_by_name (Splitting_field.c_str(), value);
      Current_splitting_value = value;

      // is this the same as previous splitting field?  If so then we need to
      // tell caller to create new plots.
      Do_create_plots = (Current_splitting_value != Previous_splitting_value);

      // save current splitting value to previous splitting value.
      Previous_splitting_value = Current_splitting_value;
      }

   if (Table_obj.At_first() && Current_xy_index == 0)
      Do_create_plots = true;

   return Do_create_plots;
   }

// ------------------------------------------------------------------
//  Short description:
//    create the necessary plots for each xy pair and store pointers
//    to the series in the protected member field called Series_ptr

//  Notes:

//  Changes:
//    DPH 21/4/1997

// ------------------------------------------------------------------
void High_level_chart_base::Create_plots (const char* Title_of_table)
   {
   // loop through all xy pairs and create plots.
   for (int XY_number = 0;
        XY_number < Num_xys;
        XY_number++)
      {
      // go create new plot.
      XYs[XY_number]->Plot_ptr = new Plot;

      // create new series and give it to our plot.
      XYs[XY_number]->Plot_ptr->Data_series = XYs[XY_number]->Format_ptr->Create_series (Get_num_plots());

      // format the plot in the current xy pair.
      Format_plot (XYs[XY_number]->Plot_ptr,
                   XYs[XY_number],
                   Title_of_table);

      // give new plot to chart only if x and y variables exist in file.
      Chart_ptr->Add_plot (*(XYs[XY_number]->Plot_ptr));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Format the specified plot given the xy pair structure.

//  Notes:

//  Changes:
//    DPH 21/4/1997

// ------------------------------------------------------------------
void High_level_chart_base::Format_plot (Plot* Plot_ptr,
                                    XY_pair* XY_ptr,
                                    const char* Title_of_table)
   {
   Plot_ptr->X_axis.Axis_type = (Axis::Axis_type_enum) XY_ptr->x_axis_link;
   Plot_ptr->Y_axis.Axis_type = (Axis::Axis_type_enum) (XY_ptr->y_axis_link + 2);
   Plot_ptr->Data_series->Title = XY_ptr->Title + " - " + Title_of_table;
   if (strlen(Splitting_field.c_str()) > 0)
      {
      Plot_ptr->Data_series->Title += "(";
      Plot_ptr->Data_series->Title += Current_splitting_value;
      Plot_ptr->Data_series->Title += ")";
      }

   Plot_ptr->X_axis.Title.Text = XY_ptr->X_column_name;
   Plot_ptr->Y_axis.Title.Text = XY_ptr->Y_column_name;

   if (XY_ptr->X_type == date)
      Plot_ptr->Data_series->x_data_type = Series_base::date;
   else if (XY_ptr->X_type == alpha)
      Plot_ptr->Data_series->x_data_type = Series_base::alpha;
   }

// ------------------------------------------------------------------
//  Short description:
//    let user edit a chart.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_chart_base::User_edit (void)
   {
   Chart_ptr->User_edit();
   }

// ------------------------------------------------------------------
//  Short description:
//    allow the user to zoom the chart.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_chart_base::Allow_zoom (bool zoom)
   {
   Chart_ptr->Allow_zoom = zoom;
   }

