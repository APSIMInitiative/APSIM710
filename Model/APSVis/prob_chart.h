#ifndef PROB_CHART_H
#define PROB_CHART_H

#include "High_level_chart_base.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a probability chart object.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
class CHART_EXPORT Probability_chart : public High_level_chart_base
   {
   public:
      Probability_chart(TForm* parent);

      virtual ~Probability_chart(void);

      void Add_xy (const char* X_field_name,
                    Format_base* Format_ptr = NULL,
                   const char* Title = NULL);

      bool Do_prob_exceedance;

   protected:
      int Current_point;

      vector <double> *Values;

      virtual void Initialise_xys (Table_base& Table_obj);
      virtual bool Get_next_point (Table_base& Table_obj,
                                   char* x_value,
                                   char* y_value);
      virtual void Format_plot (Plot* Plot_ptr,
                                XY_pair* XY_ptr,
                                const char* Title_of_table);


   };

#endif 
