#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "apsim_binary_table.h"
#include <General\string_functions.h>
#include <assert.h>
#include <strstrea.h>

struct Variable_struct
   {
   char name[15];
   char value[30];
   };
struct Heading_struct
   {
   char name[30];
   char units[10];
   short int array_size;
   short int data_type;
   };
static const int ID_Double=1;
static const int ID_String=2;
static const int ID_Real=3;
static const int ID_Integer=4;
static const int ID_Logical=5;
static const int ID_Date=9;
static const int Width_string=15;

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
APSIM_binary_table::APSIM_binary_table (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
APSIM_binary_table::APSIM_binary_table (const char* Fname)
   : Table_stream (Fname)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    open the table.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_binary_table::Open_table (void)
   {
   In_stream.open(File_name.c_str(), ios::binary);
   }


// ------------------------------------------------------------------
//  Short description:
//    calculate number of records.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_binary_table::Calc_num_records (void)
   {
   In_stream.seekg (0, ios::end);
   In_stream.clear ();
   unsigned long Start_of_last_record = In_stream.tellg() - (long) Record_length;
   Num_records = (Start_of_last_record - Start_of_data) / Record_length + 1;
   }

// ------------------------------------------------------------------
//  Short description:
//    read in the title from the file.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_binary_table::Read_title (void)
   {
   Title = "";

   // check the file for validity.
   char Identifier[13];
   In_stream.read ((char*) &Identifier, sizeof Identifier - 1);
   Identifier[12] = 0;
   Is_valid = (strcmpi(Identifier, "APSIM binary") == 0);

   // only continue if the file is valid.
   if (Is_valid)
      {
      // scan the variable list for a title keyword
      short int Num_variables;
      In_stream.read ((char*) &Num_variables, sizeof Num_variables);

      // loop through all variables, read in each variable and
      // locate the title variable.
      Variable_struct Variable;
      for (unsigned int var = 0; var < Num_variables; var++)
         {
         In_stream.read ((char*) &Variable, sizeof Variable);
         Variable.name[sizeof Variable.name - 1] = 0;
         Variable.value[sizeof Variable.value - 1] = 0;
         string name(Variable.name);
         string value(Variable.value);
         stripLeadingTrailing(name, " ");
         stripLeadingTrailing(value, " ");
         To_lower(name);
         if (name == "title")
            Title = value;
         }
      // did we find the title - if so then store title.
      // if not then invalid file.
      if (Title == "")
         Is_valid = false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    read in the field names from the stream.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_binary_table::Read_field_names (void)
   {
   Read_title();

   // binary stream should now be positioned at start of headings.
   // Only continue though if we are working on a valid file.
   if (Is_valid)
      {
      // read in the number of headings to follow.
      short int Num_headings;
      In_stream.read ((char*) &Num_headings, sizeof Num_headings);

      // loop through all headings, create a field info object for
      // each and store in our fields vector
      unsigned long Offset_so_far = 0;
      Field_info info;
      Heading_struct Heading;
      for (unsigned int head = 0; head < Num_headings; head++)
         {
         In_stream.read ((char*) &Heading, sizeof Heading);
         Heading.name[sizeof Heading.name - 1] = 0;

         // loop through all array indexes.
         for (int array = 1; array <= Heading.array_size; array++)
            {
            // fill our field info object.
            info.Field_name = Heading.name;
            stripLeadingTrailing(info.Field_name, " ");
            if (Heading.array_size > 1)
               {
               ostrstream array_stream;
               array_stream << "(" << array << ")" << ends;
               info.Field_name += array_stream.str();
               delete array_stream.str();
               }

            info.Data_type = Convert_data_types (Heading.data_type);
            info.Field_offset = Offset_so_far;
            Offset_so_far += Get_field_size (Heading.data_type);

            Fields.push_back (info);
            Field_names.push_back (info.Field_name);
            }
         }
      Record_length = Offset_so_far;
      Start_of_data = In_stream.tellg();
      Calc_num_records();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    convert the apsim binary data type into our enum data type.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
APSIM_binary_table::Field_info::Data_type_enum
   APSIM_binary_table::Convert_data_types (int Data_type)
   {
   if (Data_type == ID_Integer)
      return APSIM_binary_table::Field_info::Dinteger;

   else if (Data_type == ID_Real || Data_type == ID_Double )
      return APSIM_binary_table::Field_info::Dreal;

   else if (Data_type == ID_String)
      return APSIM_binary_table::Field_info::Dstring;

   else if (Data_type == ID_Logical)
      return APSIM_binary_table::Field_info::Dlogical;

   else if (Data_type == ID_Date)
      return APSIM_binary_table::Field_info::Ddate;

   else
      assert (false);

   return APSIM_binary_table::Field_info::Dinteger;  //should never get to here.
   }

// ------------------------------------------------------------------
//  Short description:
//    convert the apsim binary data type into our enum data type.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
int APSIM_binary_table::Get_field_size (int Data_type)
   {
   if (Data_type == ID_Integer)
      return sizeof(short int);

   else if (Data_type == ID_Real || Data_type == ID_Double )
      return sizeof (double);

   else if (Data_type == ID_String )
      return Width_string;

   else if (Data_type == ID_Logical)
      return sizeof (bool);

   else if (Data_type == ID_Date)
      return Width_string;
   else
      assert (false);
   return 0;
   }

// ------------------------------------------------------------------
//  Short description:
//    position stream at beginning of last record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_binary_table::Position_at_last (void)
   {
   Current_record_number = Num_records;
   }

// ------------------------------------------------------------------
//  Short description:
//    move up or down a specified number of records.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_binary_table::Get_data_by_index (unsigned int Field_index, char* St_value)
   {
   // need to position file at start of field.
   unsigned long Pos_field = Start_of_data + (Current_record_number-1) * Record_length
                             + Fields[Field_index].Field_offset;

   In_stream.seekg (Pos_field);

   // read in data according to type.
   switch (Fields[Field_index].Data_type)
      {
      case APSIM_binary_table::Field_info::Dinteger :
                      {
                      short int value;
                      In_stream.read ((char*)&value, sizeof value);
                      itoa(value, St_value, 10);
                      break;
                      }
      case APSIM_binary_table::Field_info::Dreal    :
                      {
                      double value;
                      In_stream.read ((char*)&value, sizeof value);
                      int dec, sign = 4;
                      strcpy(St_value, fcvt(value, 0, &dec, &sign));
                      break;
                      }
      case APSIM_binary_table::Field_info::Dstring  :
      case APSIM_binary_table::Field_info::Ddate    :
                      {
                      In_stream.read (St_value, Width_string);
                      St_value[Width_string-1] = 0;
                      stripLeadingTrailing(St_value, " ");
                      break;
                      }
      case APSIM_binary_table::Field_info::Dlogical :
                      {
                      bool value;
                      In_stream.read ((char*)&value, sizeof value);
                      if (value)
                         strcpy(St_value, "1");
                      else
                         strcpy(St_value, "0");
                      break;
                      }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    move up or down a specified number of records.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_binary_table::Read_next_record (void)
   {
   End_of_data = (Current_record_number > Num_records);
   }


