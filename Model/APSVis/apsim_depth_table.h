#ifndef APSIM_DEPTH_TABLE_H
#define APSIM_DEPTH_TABLE_H

#include "apsim_table.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates an APSIM table but specifically
//    deals with depth profile information.

//  Notes:
//    This class reads the following file format :-
//          Date    dlayer(1)  dlayer(2)  swdep(1)  swdep(2)
//         1/1/96     10         50         10        100
//         2/1/96     10         50          9         99
//
//    The caller however sees the file format as :-
//          Date      dlayer         swdep
//         1/1/96       10            10
//         1/1/96       50           100
//         2/1/96       10             9
//         2/1/96       50            99
//
//    There is also the option to accumulate a variable (usually dlayer).
//    If the name of the variable is specified in Cumulative_variable_name
//    then the values for this variable will be accumulated.


//  Changes:
//    DPH 16/7/1997

// ------------------------------------------------------------------
class DATA_EXPORT APSIM_depth_table : public APSIM_table
   {
   public:
      APSIM_depth_table (void);
      APSIM_depth_table (const char* File_name);

      string Cumulative_variable_name;

      // data retrieval methods.
      virtual void Get_data_by_index (unsigned int Field_index, char* Value);
      virtual int Get_field_index (const char* Field_name);


   protected:
      vector <string> Real_field_names;
      int Num_layers;
      int Current_layer;
      double Cumulative_value;
      virtual void Read_field_names (void);
      virtual void Read_next_record (void);
      virtual void Position_at_first (void);
      virtual void Position_at_last (void);
      virtual void Position_at_next (void);
      virtual void Position_at_prior (void);
   };

#endif
