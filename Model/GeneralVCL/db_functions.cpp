//---------------------------------------------------------------------------
#include <general/pch.h>
#include <vcl.h>
#pragma hdrstop

#include "db_functions.h"
#include <General/string_functions.h>
#include <General/math_functions.h>
#include <adodb.hpp>

#include <stdexcept>
using namespace std;

//---------------------------------------------------------------------------
// Adds the specified fields to the specified dataset.
// Does NOT removes any existing fields of the dataset
// The values are used to determine the datatype of the field.
//---------------------------------------------------------------------------
void addDBFields(TDataSet* dataset,
                 vector<string>& fieldNames,
                 vector<string>& fieldValues) throw(runtime_error)
   {
   // make sure we have the same number of values as we do field names.
   if (fieldValues.size() != fieldNames.size())
      throw runtime_error("The number of field names does not equal the number "
                          "of field values.");

   dataset->Active = false;
   for (unsigned fieldI = 0; fieldI < fieldNames.size(); fieldI++)
      addDBField(dataset, fieldNames[fieldI], fieldValues[fieldI]);
   }

//---------------------------------------------------------------------------
// Adds the specified field to the specified dataset.
// The fieldValue is used to determine the datatype of the field.
// A field is not added if it already exists.
//---------------------------------------------------------------------------
void addDBField(TDataSet* dataset,
                               const string& fieldName,
                               const string& fieldValue)
   {
   if (dataset->FieldDefs->IndexOf(fieldName.c_str()) == -1)
      {
      TFieldDef *fieldDef = dataset->FieldDefs->AddFieldDef();
      fieldDef->Name = fieldName.c_str();

      try
         {
         if (fieldValue == "" || Is_numerical(fieldValue.c_str()))
            fieldDef->DataType = ftFloat;

         else
            {
            // the constructor below will throw if fieldValue is not
            // a date.
            TDateTime(fieldValue.c_str(), TDateTime::Date);
            fieldDef->DataType = ftDate;
            }
         }
      catch (...)
         {
         fieldDef->DataType = ftString;
         fieldDef->Size = 200;
         }
      }
   }

//---------------------------------------------------------------------------
// Return a list of field names to caller for the specified dataset.
//---------------------------------------------------------------------------
void getDBFieldNames(TDataSet* dataset,
                                    std::vector<std::string>& fieldNames)
   {
   for (int field = 0; field < dataset->FieldCount; field++)
      fieldNames.push_back(dataset->Fields->Fields[field]->FieldName.c_str());
   }
// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// Will through runtime_error if cannot convert the values to a float.
// ------------------------------------------------------------------
void getDBFieldValues(TDataSet* dataset,
                                     const string& fieldName,
                                     vector<double>& values) throw (runtime_error)
   {
   try
      {
      dataset->First();
      while (!dataset->Eof)
         {
         values.push_back( StrToFloat(dataset->FieldValues[fieldName.c_str()]) );
         dataset->Next();
         }
      }
   catch (...)
      {
      throw runtime_error("Cannot convert field to numerical value."
                          "\nField: " + fieldName +
                          "\nValue: " + AnsiString(dataset->FieldValues[fieldName.c_str()]).c_str() +
                          "\nDataset: " + dataset->Name.c_str());
      }
   }
// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// Will through runtime_error if cannot convert the values to a float.
// ------------------------------------------------------------------
void getDBFieldValues(TDataSet* dataset,
                      const string& fieldName,
                      vector<unsigned>& values)
   {
   try
      {
      dataset->First();
      while (!dataset->Eof)
         {
         values.push_back( StrToInt(dataset->FieldValues[fieldName.c_str()]) );
         dataset->Next();
         }
      }
   catch (...)
      {
      throw runtime_error("Cannot convert field to numerical value."
                          "\nField: " + fieldName +
                          "\nValue: " + AnsiString(dataset->FieldValues[fieldName.c_str()]).c_str() +
                          "\nDataset: " + dataset->Name.c_str());
      }
   }

// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// ------------------------------------------------------------------
void getDBFieldValues(TDataSet* dataset,
                                     const std::string& fieldName,
                                     vector<string>& values)
   {
   dataset->First();
   while (!dataset->Eof)
      {
      values.push_back(AnsiString(dataset->FieldValues[fieldName.c_str()]).c_str());
      dataset->Next();
      }
   }

//---------------------------------------------------------------------------
// Copy a record from the source dataset to the destination dataset.
// Returns true if a record was copied.
// The newly appended record will be the current record after this operation.
//---------------------------------------------------------------------------
bool copyDBRecord(TDataSet* source, TDataSet* destination)
   {
   if (!source->Eof)
      {
      destination->Append();
      for (int field = 0; field < source->FieldCount; field++)
         {
         String fieldName = source->Fields->Fields[field]->FieldName;
         try
            {
            destination->FieldValues[fieldName] = source->FieldValues[fieldName];
            }
         catch (...)
            { }              
         }
      destination->Post();
      return true;
      }
   else
      return false;
   }
//---------------------------------------------------------------------------
// Append a new record to the specified dataset.
// Then add the specified values to the new record and post the changes.
//---------------------------------------------------------------------------
void appendDBRecord(TDataSet* dataset,
                                   vector<string>& fieldNames,
                                   vector<string>& fieldValues) throw(runtime_error)
   {
   // make sure we have the same number of values as we do field names.
   if (fieldValues.size() != fieldNames.size())
      throw runtime_error("The number of field names does not equal the number "
                          "of field values.");

   unsigned fieldI;

   dataset->Append();
   try
      {
      for (fieldI = 0; fieldI < fieldNames.size(); fieldI++)
         {
         AnsiString fieldName = fieldNames[fieldI].c_str();
         if (fieldValues[fieldI] != "" && dataset->FieldDefs->IndexOf(fieldName) != -1)
            dataset->FieldValues[fieldName] = fieldValues[fieldI].c_str();
         }
      }
   catch (const Exception& err)
      {
      dataset->Post();
      throw runtime_error(err.Message.c_str());
      }
   catch (...)
      {
      dataset->Post();
      throw runtime_error("Invalid data found in column: " + fieldNames[fieldI] + ". Value=" + fieldValues[fieldI]);
      }
   dataset->Post();
   }
//---------------------------------------------------------------------------
// Append a new record to the specified dataset.
// Then add the specified values to the new record and post the changes.
//---------------------------------------------------------------------------
void appendDBRecordNoErrors(TDataSet* dataset,
                            vector<string>& fieldNames,
                            vector<string>& fieldValues)
   {
   // make sure we have the same number of values as we do field names.
   if (fieldValues.size() != fieldNames.size())
      throw runtime_error("The number of field names does not equal the number "
                          "of field values.");

   unsigned fieldI;

   dataset->Append();
   for (fieldI = 0; fieldI < fieldNames.size(); fieldI++)
      {
      AnsiString fieldName = fieldNames[fieldI].c_str();
      if (fieldValues[fieldI] != "" && dataset->FieldDefs->IndexOf(fieldName) != -1)
         {
         try
            {
            dataset->FieldValues[fieldName] = fieldValues[fieldI].c_str();
            }
         catch (const Exception& err)
            {
            }
         catch (...)
            {
            }
         }
      }
   dataset->Post();
   }
//---------------------------------------------------------------------------
// Return a DB value to caller - as a string. Missing values will be a
// blank string.
//---------------------------------------------------------------------------
string getDBValue(TDataSet* dataset, const std::string& fieldName)
   {
   try
      {
      return AnsiString(dataset->FieldValues[fieldName.c_str()]).c_str();
      }
   catch (const Exception& err)
      {
      return "";
      }
   }

//---------------------------------------------------------------------------
// Return a DB value to caller - as a string. To test for a missing value
// call isMissing function.
//---------------------------------------------------------------------------
double getDBDouble(TDataSet* dataset, const std::string& fieldName)
   {
   try
      {
      return dataset->FieldValues[fieldName.c_str()];
      }
   catch (const Exception& err)
      {
      return missingValue<double>();
      }
   }
//---------------------------------------------------------------------------
// Return a DB value to caller - as a string. To test for a missing value
// call isMissing function.
//---------------------------------------------------------------------------
unsigned getDBUnsigned(TDataSet* dataset, const std::string& fieldName)
   {
   try
      {
      return dataset->FieldValues[fieldName.c_str()];
      }
   catch (const Exception& err)
      {
      return missingValue<unsigned>();
      }
   }
// ------------------------------------------------------------------
// Execute the specified query.
// ------------------------------------------------------------------
void executeQuery(TADOConnection* connection, const string& sql)
   {
   TADOQuery* query = new TADOQuery(NULL);
   try
      {
      query->Connection = connection;
      query->SQL->Text = sql.c_str();
      query->ExecSQL();
      delete query;
      }
   catch (const Exception& err)
      {
      delete query;
      throw runtime_error(err.Message.c_str());
      }
   }
// ------------------------------------------------------------------
// Run the specified query. Caller should delete the returned query
// when done.
// ------------------------------------------------------------------
TDataSet* runQuery(TADOConnection* connection, const string& sql)
   {
   TADOQuery* query = new TADOQuery(NULL);
   try
      {
      query->Connection = connection;
      query->SQL->Text = sql.c_str();
      query->Open();
      }
   catch (const Exception& err)
      {
      delete query;
      throw runtime_error(err.Message.c_str());
      }
   return query;
   }

// ------------------------------------------------------------------
// Fill dataset from the contents of the specified grid.
// ------------------------------------------------------------------
void fillDataSetFromGrid(TDataSet* dataset, TStringGrid* grid, const string& seriesName)
   {
   dataset->DisableControls();
   dataset->Active = false;
   dataset->FieldDefs->Clear();
   vector<string> fieldNames, fieldValues;
   for (int col = 0; col != grid->ColCount; col++)
      {
      string name = grid->Cells[col][0].c_str();
      replaceAll(name, "\r", "");
      fieldNames.push_back(name);
      fieldValues.push_back(grid->Cells[col][1].c_str());
      }
   if (seriesName != "")
      {
      fieldNames.push_back("series");
      fieldValues.push_back(seriesName);
      }

   addDBFields(dataset, fieldNames, fieldValues);
   dataset->Active = true;
   for (int row = 1; row != grid->RowCount; row++)
      {
      if (grid->Cells[0][row] == "")
         break;

      fieldValues.erase(fieldValues.begin(), fieldValues.end());
      for (int col = 0; col != grid->ColCount; col++)
         fieldValues.push_back(grid->Cells[col][row].c_str());
      if (seriesName != "")
         fieldValues.push_back(seriesName);
      appendDBRecord(dataset, fieldNames, fieldValues);
      }
   dataset->EnableControls();
   dataset->First();
   dataset->Edit();
   dataset->Post();
   }
