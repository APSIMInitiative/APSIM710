#ifndef FILTER_H
#define FILTER_H

#include "data.h"
#include <general/date_class.h>

class DATA_EXPORT Table_base;  // forward
const int MAX_NUM_FILTERS = 10;
// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a record filter which can be used in
//    a table to filter records.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class DATA_EXPORT Record_filter
   {
   public:
      enum Type_enum {Automatic, Numerical, String, Date};

   private:
      enum Filter_operator {LT, LE, EQ, GE, GT, NE};
      class Internal_filter
         {
         public:
            Internal_filter(void) {Field_index = -1;}
            string Field_name;
            int Field_index;
            Filter_operator Operator;
            string st_value;
            GDate  date_value;
            double num_value;
         };
      Internal_filter Filter_list[MAX_NUM_FILTERS];
      int Num_filters;
      bool Is_OR;
      Type_enum Data_type;

      bool Numerical_comp (const char* lhs,
                           Filter_operator op,
                           double rhs);
      bool Date_comp (const char* lhs,
                      Filter_operator op,
                      GDate& rhs);
      bool String_comp (const char* lhs,
                        Filter_operator op,
                        const char* rhs);

   public:
      Record_filter (void);

      // clear all filters
      void Clear (void);

      // add a filter
      void Add (const char* Field_name,
                const char* Operator,
                const char* Value,
                Type_enum Data_type = Automatic);

      // called by query_table
      bool Keep_record (Table_base& Table_obj);

      // set the AND_OR operator (OR is the default)
      void Set_AND (bool Is_AND = true)
         {Is_OR = (!Is_AND);}
      void Set_OR (bool Is_OR = true)
         {Is_OR = Is_OR;}
   };

#endif