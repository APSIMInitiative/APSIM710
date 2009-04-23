#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "apsim_depth_table.h"
#include <general\string_functions.h>
#include <general\stream_functions.h>
#include <general\stl_functions.h>
#include <general\math_functions.h>
#include <strstrea.h>
#include <stdio.h>

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
APSIM_depth_table::APSIM_depth_table (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
APSIM_depth_table::APSIM_depth_table (const char* Fname)
   : APSIM_table (Fname)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    goto first record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_depth_table::Read_field_names (void)
   {
   APSIM_table::Read_field_names();

   Real_field_names = Field_names;
   Num_layers = 0;

   // Loop through all field names and copy all fields to our field list.
   // Where array fields are encountered (eg dlayer(1), dlayer(2)) only
   // copy one field name (dlayer) to the field list minus the array index.

   vector <string> Our_list;
   for (vector<string>::iterator Iter = Field_names.begin();
                                 Iter != Field_names.end();
                                 Iter++)
      {
      string This_field = *Iter;

      if (This_field.find("(1)") != string::npos)
         {
         This_field.erase(This_field.find("(1)"));
         Our_list.push_back (This_field);
         }
      else if (This_field.find("(") != string::npos)
         {
         // extract array specifier.
         string Array_spec = This_field.substr(This_field.find("("));

         istrstream Array_stream ((char*) Array_spec.c_str());
         string Array_size_string;
         Read_token (Array_stream, "( ", " )", Array_size_string);

         // update number of layers for this table.
         Num_layers = max(Num_layers, atoi (Array_size_string.c_str()));
         }
      else
         Our_list.push_back (This_field);
      }
   Field_names = Our_list;
   }

// ------------------------------------------------------------------
//  Short description:
//    return index of a field to caller.  Returns -1 if not found.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
int APSIM_depth_table::Get_field_index (const char* Field_name)
    {
    int Indx = Locate_string (Field_name, Real_field_names);
    if (Indx < 0)
       {
       // must be a depth field - add (1)

       string New_field = Field_name;
       New_field += "(1)";

       // Get index for field name.
       Indx = Locate_string (New_field.c_str(), Real_field_names);
       }
    return Indx;
    }

// ------------------------------------------------------------------
//  Short description:
//    return data to caller using an index into the field names list as lookup.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_depth_table::Get_data_by_index (unsigned int Field_index, char* Value)
   {
   unsigned int Pos_array = Real_field_names[Field_index].find("(");
   bool Is_depth_field = (Pos_array != string::npos);

   if (Is_depth_field)
      {
      // create a base for array variable by striping off the array specifier.
      string New_field = Real_field_names[Field_index].substr(0, Pos_array);

      // work out if this variable is a cumulative one.
      bool Is_cumulative = (Cumulative_variable_name == New_field);

      // need to build up a field name.
      char Field_name[100];
      sprintf(Field_name, "%s(%i)", New_field.c_str(), Current_layer);

      // Get index for field name.
      Field_index = Get_field_index(Field_name);

      // get value for this variable.
      APSIM_table::Get_data_by_index (Field_index, Value);

      // is this a cumulative variable?  If so then add value to running total
      if (Is_cumulative)
         {
         Cumulative_value += atof(Value);
         char Cumulative_str[100];
         sprintf (Cumulative_str, "%5.1f", Cumulative_value);
         strcpy(Value, Cumulative_str);
         }
      }
   else
      APSIM_table::Get_data_by_index (Field_index, Value);
   }

// ------------------------------------------------------------------
//  Short description:
//    position stream at beginning of first record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_depth_table::Position_at_first (void)
   {
   APSIM_table::Position_at_first ();
   Current_layer = Num_layers + 2;
   }

// ------------------------------------------------------------------
//  Short description:
//    position stream at beginning of last record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_depth_table::Position_at_last (void)
   {
   Current_layer = 0; // forces read_next_record to read in a line.
   APSIM_table::Position_at_last ();
   }

// ------------------------------------------------------------------
//  Short description:
//    position stream at beginning of next record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_depth_table::Position_at_next (void)
   {
   if (Current_layer < 1 || Current_layer > Num_layers)
      APSIM_table::Position_at_next ();
   Current_layer++;
   }

// ------------------------------------------------------------------
//  Short description:
//    position stream at beginning of previous record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_depth_table::Position_at_prior (void)
   {
   if (Current_layer < 1 || Current_layer > Num_layers)
      APSIM_table::Position_at_prior ();

   Current_layer--;
   }

// ------------------------------------------------------------------
//  Short description:
//    read next line from stream.
//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_depth_table::Read_next_record (void)
   {
   if (Current_layer < 1 || Current_layer > Num_layers)
      {
      APSIM_table::Read_next_record();
      if (Current_layer == 0)
         Current_layer = Num_layers;
      else
         Current_layer = 1;
      Cumulative_value = 0.0;
      }
   }

