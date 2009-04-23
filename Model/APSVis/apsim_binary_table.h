#ifndef APSIM_BINARY_TABLE_H
#define APSIM_BINARY_TABLE_H

#include "table_stream.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates an APSIM binary table

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class DATA_EXPORT APSIM_binary_table : public Table_stream
   {
   public:
      APSIM_binary_table (void);
      APSIM_binary_table (const char* File_name);

      virtual void Get_data_by_index (unsigned int Field_index, char* St_value);

   protected:
      virtual void Open_table (void);
      virtual void Read_field_names (void);
      virtual void Read_next_record (void);
      virtual void Position_at_first (void) {};
      virtual void Position_at_last (void);
      virtual void Position_at_next (void) {};
      virtual void Position_at_prior (void) {};

   private:
      class Field_info
         {
         public:

            // data members
            enum Data_type_enum {Dinteger, Dreal, Dstring, Dlogical, Ddate};
            string Field_name;
            Data_type_enum Data_type;
            unsigned long Field_offset;

            // operators.
            bool operator== (const Field_info& rhs) const;

         };

      vector <Field_info> Fields;
      unsigned long Start_of_data;
      unsigned long Record_length;
      unsigned int Num_records;

      APSIM_binary_table::Field_info::Data_type_enum
         APSIM_binary_table::Convert_data_types (int Data_type);
      int APSIM_binary_table::Get_field_size (int Data_type);
      void Read_title (void);
      void Calc_num_records (void);

   };

#endif
