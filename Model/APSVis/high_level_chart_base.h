#ifndef HIGH_LEVEL_CHART_H
#define HIGH_LEVEL_CHART_H

#include "drawable.h"
#include "chart_base.h"
#include "format_base.h"
#include "database.h"

class High_level_screen;   // forward

enum X_axis_link_enum {top, bottom};
enum Y_axis_link_enum {left, right};
enum Field_type_enum {numeric, alpha, date};
#define MAX_XY_PAIRS 50

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a high level chart object.

//  Notes:

//  Changes:
//    DPH 18/4/1997
//    dph 10/3/98 added accumulate x and y options C-081

// ------------------------------------------------------------------
class CHART_EXPORT High_level_chart_base
   {
   protected:
      class XY_pair
         {
         public:
            XY_pair (void);
            XY_pair (const char* X_column_name,
                     const char* Y_column_name,
                     Field_type_enum X_type,
                     Field_type_enum Y_type,
                     X_axis_link_enum x_axis_link,
                     Y_axis_link_enum y_axis_link,
                     Format_base* Format_ptr,
                     const char* Title,
                     bool Accumulate_x = false,
                     bool Accumulate_y = false);
            ~XY_pair (void);

            string X_column_name;               // name of x column in table.
            string Y_column_name;               // name of y column in table.
            Field_type_enum X_type;             // what type of data is X?
            Field_type_enum Y_type;             // what type of data is y?
            X_axis_link_enum x_axis_link;       // plot on which x axis?
            Y_axis_link_enum y_axis_link;       // plot on which y axis?
            Format_base* Format_ptr;            // object used to format plot
            string Title;                       // title of series - used in legend
            bool Accumulate_x;                  // true if we are to accumulate the x variable.
            bool Accumulate_y;                  // true if we are to accumulate the y variable.

            int x_index;                        // used internally by high_level_chart
            int y_index;                        //           "
            Plot* Plot_ptr;                     //           "
            double Accumulated_x;               // current values of accumulate x
            double Accumulated_y;               // current values of accumulate y

         };

   public:
      High_level_chart_base(TForm* parent);

      virtual ~High_level_chart_base(void);

      // Clear all xy pairs from chart.
      void Clear_xys (void);

      // return true if chart has something to draw.
      bool Something_to_draw (void);

      // return number of plots to caller.
      int Get_num_plots (void);

      // properties.
      void Set_splitting_field(const char* Splitting_field);

      // Create plots on chart from specified table.
      void Create_plots_from_table (Table_base& Table_obj);

      // Create plots on chart from specified database.
      void Create_plots_from_database (Database& DB_obj);

      // allow user to edit chart.
      void User_edit (void);

      // allow user to zoom chart.
      void Allow_zoom (bool Zoom = true);

   protected:
      Chart_base* Chart_ptr;
      bool Own_chart_ptr;
      string Splitting_field;
      string Current_splitting_value, Previous_splitting_value;
      int Current_xy_index;

      // storage for all xy pairs.
      XY_pair* XYs[MAX_XY_PAIRS];
      int Num_xys;

      // add an xy pair to list.
      void Add_xy_pair (const char* X_field_name,
                        const char* Y_field_name,
                        Field_type_enum X_type,
                        Field_type_enum Y_type,
                        X_axis_link_enum X_axis_link,
                        Y_axis_link_enum Y_axis_link,
                        Format_base* Format_ptr,
                        const char* Title,
                        bool Accumulate_x = false,
                        bool Accumulate_y = false);
      bool More_xy_points (Table_base& Table_obj);
      void Get_next_point_and_plot (Table_base& Table_obj,
                              char* x_value,
                              char* y_value,
                              Plot** Plot_ptr);
      void Create_plots (const char* Title_of_table);



      virtual void Initialise_xys (Table_base& Table_obj);
      virtual bool Get_next_point (Table_base& Table_obj,
                              char* x_value,
                              char* y_value);
      virtual bool Need_to_create_new_plots (Table_base& Table_obj);
      virtual void Format_plot (Plot* Plot_ptr,
                                XY_pair* XY_ptr,
                                const char* Title_of_table);
      virtual void Do_final_formatting (void) {};


   friend High_level_screen;
   };

#endif 
