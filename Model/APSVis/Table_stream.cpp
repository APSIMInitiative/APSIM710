//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "table_stream.h"
#include <stdio.h>
#include <values.h>  // MAXFLOAT
#include <general\string_functions.h>

#define Missing_value "*"

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Table_stream::Table_stream (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Table_stream::Table_stream (const char* Fname)
   {
   File_name = Fname;
   }

// ------------------------------------------------------------------
//  Short description:
//    open the table.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_stream::Open_table (void)
   {
   Table_base::Open_table();
   In_stream.open(File_name.c_str(), ios::binary);
   }

// ------------------------------------------------------------------
//  Short description:
//    close the table.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_stream::Close_table (void)
   {
   In_stream.close();
   }

// ------------------------------------------------------------------
//  Short description:
//    position stream at first record.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    DPH 22/2/2001 Moved the clear before the seekg.

// ------------------------------------------------------------------
void Table_stream::Position_at_first (void)
   {
   Table_base::Position_at_first ();

   In_stream.clear();
   In_stream.seekg(0);
   }

// ------------------------------------------------------------------
//  Short description:
//    position stream at last record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_stream::Position_at_last (void)
   {
   Table_base::Position_at_last ();

   In_stream.seekg(-1, ios::end);
   In_stream.clear();

   // skip over any blank lines.
   while (In_stream && In_stream.peek() == '\n' || In_stream.peek() == '\r')
      In_stream.seekg(-1, ios::cur);

   // move past \r\n so that the stream is pointing to the next record.
   In_stream.seekg(3, ios::cur);
   }

// ------------------------------------------------------------------
//  Short description:
//    position stream at beginning of prior record.

//  Notes:
//    Because the file pointer to already pointing to the next record,
//    we need to go back 2 records.

//  Changes:
//    DPH 17/4/1997
//    dph 28/10/97 fixed defect.  Changed last line from a 1 to a 2.

// ------------------------------------------------------------------
void Table_stream::Position_at_prior (void)
   {
   Table_base::Position_at_prior();

   // scan backwards until we find a endl
   int Num_found = 0;
   do
      {
      In_stream.seekg (-1, ios::cur);
      if (In_stream.peek() == '\n')
         Num_found++;
      }
   while (In_stream && Num_found < 2);

   // move stream pointer to next char.
   In_stream.seekg (1, ios::cur);
   }

// ------------------------------------------------------------------
//  Short description:
//    read field names from stream.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_stream::Read_field_names (void)
   {
   Table_base::Read_field_names();

   Title = File_name;

   Field_names.erase (Field_names.begin(), Field_names.end());
   for (unsigned int Field_number = 0;
        Field_number < Num_fields;
        Field_number++)
      {
      char Field_name[20];
      sprintf (Field_name, "Field%i", Field_number + 1);
      Field_names.push_back (string (Field_name));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    read next line from stream.
//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_stream::Read_next_record (void)
   {
   In_stream.getline (Current_record, sizeof Current_record, '\n');
   if (Current_record[strlen(Current_record)-1] == '\r')
      Current_record[strlen(Current_record)-1] = '\0';

   if (Current_record[0] == 0 || In_stream.eof())
      End_of_data = true;
   }

