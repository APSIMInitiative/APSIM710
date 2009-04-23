#ifndef TABLE_BASE_H
#define TABLE_BASE_H

#include <fstream.h>
#include "data.h"
#include "filter.h"
#define MAX_RECORD_SIZE   3000
#define MAX_NUM_FIELDS     500


// ------------------------------------------------------------------
//  Short description:
//    this class is a bass class for all tables.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class DATA_EXPORT Table_base
   {
   public:

      Table_base (void);
      virtual ~Table_base (void) {};

      // data retrieval methods.
      virtual void Get_data_by_name (const char* Field_name, char* Value);
      virtual void Get_data_by_index (unsigned int Field_index, char* Value);

      // general property retrieval methods.
      virtual vector<string>* Get_field_names (void);
      virtual int Get_field_index (const char* Field_name);
      string Get_title(void);

      // stream handling functions
      void Open (void);
      void Close (void);
      bool At_first (void);
      bool At_end_of_data (void);

      // navigation methods.
      void First(void);
      void Last(void);
      void Next(void);
      void Prior(void);
      void MoveBy(int Num_records);

      // filter stuff
      Record_filter Filter;

      // validity check
      bool Get_is_valid(void);

   protected:
      vector<string> Field_names;
      string Title;

      char Current_record[MAX_RECORD_SIZE];
      unsigned int Start_field[MAX_NUM_FIELDS];
      unsigned int End_field[MAX_NUM_FIELDS];
      unsigned int Num_fields;

      unsigned int Current_record_number;
      bool End_of_data;
      bool Is_valid;

      virtual void Open_table (void) {};
      virtual void Close_table (void) {};
      virtual void Read_field_names (void) {};
      virtual void Read_next_record (void) {};
      virtual void Position_at_first (void) {};
      virtual void Position_at_last (void) {};
      virtual void Position_at_next (void) {};
      virtual void Position_at_prior (void) {};

   private:
      void Calc_field_positions (void);
   };

#endif