//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "filter.h"
#include <General\string_functions.h>
#include "table_base.h"
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Record_filter::Record_filter (void)
   {
   Is_OR = true;
   Clear();
   }

// ------------------------------------------------------------------
//  Short description:
//    clear all filters.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Record_filter::Clear (void)
   {
   Num_filters = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//    add a new filter.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Record_filter::Add (const char* Field_name,
                         const char* Operator,
                         const char* Value,
                         Type_enum Data_t)
   {
   Filter_list[Num_filters].Field_name = Field_name;
   if (strcmpi(Operator, "<") == 0)
      Filter_list[Num_filters].Operator = LT;
   else if (strcmpi(Operator, "<=") == 0)
      Filter_list[Num_filters].Operator = LE;
   else if (strcmpi(Operator, ">") == 0)
      Filter_list[Num_filters].Operator = GT;
   else if (strcmpi(Operator, ">=") == 0)
      Filter_list[Num_filters].Operator = GE;
   else if (strcmpi(Operator, "<>") == 0)
      Filter_list[Num_filters].Operator = NE;
   else
      Filter_list[Num_filters].Operator = EQ;

   // need to determine type of data.
   if (Data_t == Automatic)
      {
      if (Is_numerical(Value))
         {
         Data_type = Numerical;
         Filter_list[Num_filters].num_value = atof(Value);
         }

      else
         {
         // try dates.
         Filter_list[Num_filters].date_value.Read (Value);
         if (Filter_list[Num_filters].date_value.Is_valid())
            Data_type = Date;
         else
            Data_type = String;
         }
      }
   else
      Data_type = Data_t;

   Filter_list[Num_filters].st_value = Value;
   stripLeadingTrailing(Filter_list[Num_filters].st_value, " ");
   Num_filters++;
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if calling Query_table object should keep this record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Record_filter::Keep_record (Table_base& Table_obj)
   {
   bool Result = true;
   if (Num_filters > 0)
      {
      // loop through all the Filters

      Result = (!Is_OR);
      for (int Filter_num = 0; Filter_num < Num_filters; Filter_num++)
         {
         Internal_filter& Filter = Filter_list[Filter_num];

         // get current value for field.
         char Value[50];
         if (Filter.Field_index < 0)
            Filter.Field_index = Table_obj.Get_field_index(Filter.Field_name.c_str());
         Table_obj.Get_data_by_index (Filter.Field_index, Value);

         bool Match;
         switch (Data_type)
            {
            case Record_filter::Numerical : Match = Numerical_comp (Value,
                                                                    Filter.Operator,
                                                                    Filter.num_value);
                                            break;
            case Record_filter::Date : Match = Date_comp (Value,
                                                          Filter.Operator,      // too slow !!!
                                                          Filter.date_value);

                                       break;
            case Record_filter::String : Match = String_comp (Value,
                                                              Filter.Operator,
                                                              Filter.st_value.c_str());
                                         break;
            };

         // combine match with result depending on Is_OR
         if (Is_OR)
            {
            Result = (Result || Match);
            if (Result)
               return true;
            }
         else
            Result = (Result && Match);
         }
      }
   return Result;
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if arguments compare numerically

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Record_filter::Numerical_comp (const char* lhs,
                                    Filter_operator op,
                                    double rhs_value)
   {
   double lhs_value = atof(lhs);

   // do comparisons.
   bool Match;
   switch (op)
      {
      case LT : Match = (lhs_value < rhs_value);
                break;
      case LE : Match = (lhs_value <= rhs_value);
                break;
      case EQ : Match = (lhs_value == rhs_value);
                break;
      case GE : Match = (lhs_value >= rhs_value);
                break;
      case GT : Match = (lhs_value > rhs_value);
                break;
      case NE : Match = (lhs_value != rhs_value);
                break;
      }
   return Match;
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if arguments compare using string comparison

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Record_filter::String_comp (const char* lhs,
                                 Filter_operator op,
                                 const char* rhs)
   {
   int Comp = strcmpi(lhs, rhs);

   // do comparisons.
   bool Match;
   switch (op)
      {
      case LT : Match = (Comp < 0);
                break;
      case LE : Match = (Comp <= 0);
                break;
      case EQ : Match = (Comp == 0);
                break;
      case GE : Match = (Comp >= 0);
                break;
      case GT : Match = (Comp > 0);
                break;
      case NE : Match = (Comp != 0);
                break;
      }
   return Match;
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if arguments compare using date comparison

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Record_filter::Date_comp (const char* lhs,
                               Filter_operator op,
                               GDate& rhs_value)
   {
   GDate lhs_value;
   lhs_value.Read (lhs);

   // do comparisons.
   bool Match;
   switch (op)
      {
      case LT : Match = (lhs_value < rhs_value);
                break;
      case LE : Match = (lhs_value <= rhs_value);
                break;
      case EQ : Match = (lhs_value == rhs_value);
                break;
      case GE : Match = (lhs_value >= rhs_value);
                break;
      case GT : Match = (lhs_value > rhs_value);
                break;
      case NE : Match = (lhs_value != rhs_value);
                break;
      }
   return Match;
   }

