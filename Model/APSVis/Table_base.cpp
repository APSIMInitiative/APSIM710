//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "table_base.h"
#include <stdio.h>
#include <values.h>  // MAXFLOAT
#include <general\string_functions.h>
#include "filter.h"
#define Missing_value "*"

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Table_base::Table_base (void)
   {
   strcpy (Current_record, "");
   Num_fields = 0;
   Is_valid = true;
   }

// ------------------------------------------------------------------
//  Short description:
//    return the field names to the caller.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
vector<string>* Table_base::Get_field_names (void)
   {
   return &Field_names;
   }

// ------------------------------------------------------------------
//  Short description:
//    return the title to the caller.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
string Table_base::Get_title (void)
   {
   return Title;
   }

// ------------------------------------------------------------------
//  Short description:
//    return index of a field to caller.  Returns -1 if not found.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
int Table_base::Get_field_index (const char* Field_name)
    {
    return Locate_string(Field_name, Field_names);
    }

// ------------------------------------------------------------------
//  Short description:
//    return data to caller using an ASCII field name as lookup.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_base::Get_data_by_name (const char* Field_name, char* Value)
   {
   int Index = Get_field_index (Field_name);
   if (Index >= 0)
      Get_data_by_index (Index, Value);
   else
      strcpy(Value, "");
   }

// ------------------------------------------------------------------
//  Short description:
//    return data to caller using an index into the field names list as lookup.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_base::Get_data_by_index (unsigned int Field_index, char* Value)
   {
   int Num_chars = End_field[Field_index] - Start_field[Field_index] + 1;
   strncpy(Value, &Current_record[Start_field[Field_index]], Num_chars);
   Value[Num_chars] = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//    open the table.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_base::Open ()
   {
   Open_table();
   if (Field_names.size() == 0)
      Read_field_names();
   First();
   }

// ------------------------------------------------------------------
//  Short description:
//    close the table.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_base::Close ()
   {
   Close_table();
   }

// ------------------------------------------------------------------
//  Short description:
//    Goto first record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_base::First ()
   {
   End_of_data = false;
   Current_record_number = 0;
   Position_at_first();
   MoveBy(1);
   }

// ------------------------------------------------------------------
//  Short description:
//    Goto last record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_base::Last ()
   {
   Current_record_number = -1;
   Position_at_last();
   MoveBy(-1);
   }

// ------------------------------------------------------------------
//  Short description:
//    goto next record on stream.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_base::Next (void)
   {
   MoveBy(1);
   }

// ------------------------------------------------------------------
//  Short description:
//    goto prior record on stream.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_base::Prior (void)
   {
   MoveBy(-1);
   }

// ------------------------------------------------------------------
//  Short description:
//    move up or down a specified number of records.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_base::MoveBy (int Num_records)
   {
   int Num_found = 0;
   do
      {
      // move table pointer.
      if (Num_records > 0)
         {
         Current_record_number++;
         Position_at_next ();
         }
      else
         {
         Current_record_number--;
         Position_at_prior ();
         }

      // read in next record
      Read_next_record ();

      // calculate field positions
      Calc_field_positions();

      // make sure this record is ok by the filter.
      if (Filter.Keep_record(*this))
         Num_found++;
      }
   while (!At_end_of_data() && Num_found != abs(Num_records));
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if positioned at first record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Table_base::At_first ()
   {
   return (Current_record_number == 1);
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if at end of data

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Table_base::At_end_of_data ()
   {
   return End_of_data;
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if table is valid.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Table_base::Get_is_valid (void)
   {
   return Is_valid;
   }

// ------------------------------------------------------------------
//  Short description:
//    calculate all field positions in current record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_base::Calc_field_positions (void)
   {
   #define START 0
   #define IN_FIELD 1

   Num_fields = 0;
   int Pos = 0;
   int State = START;
   while (Current_record[Pos] != 0)
      {
      switch (State)
         {
         case START : if (Current_record[Pos] == ',')
                         {
                         // Blank field.
                         Start_field[Num_fields] = 0;
                         End_field[Num_fields] = -1;
                         Num_fields++;
                         }
                      else if (Current_record[Pos] != ',' &&
                               Current_record[Pos] != ' ')
                         {
                         // start field.
                         Start_field[Num_fields] = Pos;
                         State = IN_FIELD;
                         }
                      break;
         case IN_FIELD : if (Current_record[Pos] == ' ' ||
                             Current_record[Pos] == ',')
                            {
                            // end field.
                            End_field[Num_fields] = Pos - 1;
                            Num_fields++;
                            State = START;
                            }

                         break;
         }
      Pos++;
      }
   if (State == IN_FIELD)
      {
      End_field[Num_fields] = Pos - 1;
      Num_fields++;
      }
   }


