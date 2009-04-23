#ifndef PRED_OBS_CHART_H
#define PRED_OBS_CHART_H

#include "high_level_chart_base.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates an XY scatter chart

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
class CHART_EXPORT Pred_obs_chart : public High_level_chart_base
   {
   public:
      Pred_obs_chart(TForm* parent);

      void Add_xy (const char* Pred_field_name,
                   const char* Obs_field_name,
                   const char* Title = NULL);

      bool Display_1_1_line;
      bool Display_regr_line;
   protected:
      virtual void Do_final_formatting (void);
      virtual bool Pred_obs_chart::Get_next_point (Table_base& Table_obj,
                                                   char* x_value,
                                                   char* y_value);

   private:
      double Maximum_value;
      double Minimum_value;
      double SumX;
      double SumY;
      double SumXY;
      double SumX2;
      double SumY2;
      int Num_points;
   };

#endif
