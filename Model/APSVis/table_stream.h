#ifndef TABLE_STREAM_H
#define TABLE_STREAM_H

#include "table_base.h"

// ------------------------------------------------------------------
//  Short description:
//    this class is will read a simple space separated text file

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class DATA_EXPORT Table_stream : public Table_base
   {
   public:

      Table_stream (void);
      Table_stream (const char* File_name);

      // name of file.
      string File_name;

   protected:
      ifstream In_stream;
      virtual void Open_table (void);
      virtual void Close_table (void);
      virtual void Read_field_names (void);
      virtual void Read_next_record (void);
      virtual void Position_at_first (void);
      virtual void Position_at_last (void);
      virtual void Position_at_prior (void);

   };

#endif