#ifndef FREQUENCY_CHART_H
#define FREQUENCY_CHART_H

#include "Prob_chart.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a probability chart object.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
class CHART_EXPORT Frequency_chart : public Probability_chart
   {
   public:
      Frequency_chart(TForm* parent);

      virtual ~Frequency_chart(void);

      void Add_xy (const char* X_field_name,
                   Format_base* Format_ptr = NULL,
                   const char* Title = NULL);

      enum Frequency_type_enum {count, prob_exceed, prob_cum};
      Frequency_type_enum Frequency_type;
      double Interval;
      double Start_point;

   protected:
      virtual bool Get_next_point (Table_base& Table_obj,
                                   char* x_value,
                                   char* y_value);
      virtual void Format_plot (Plot* Plot_ptr,
                                XY_pair* XY_ptr,
                                const char* Title_of_table);


   };

#endif 
