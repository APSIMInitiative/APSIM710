//---------------------------------------------------------------------------
#ifndef db_functionsH
#define db_functionsH
#include <vector>
#include <string>
#include <db.hpp>
#include <stdexcept>
#include <General/platform.h>

namespace Adodb {
   class TADOConnection; }
//---------------------------------------------------------------------------
// Adds the specified fields to the specified dataset.
// Does NOT removes any existing fields of the dataset
// The values are used to determine the datatype of the field.
// A field is not added if it already exists.
//---------------------------------------------------------------------------
void EXPORT addDBFields(TDataSet* dataset,
                 std::vector<std::string>& fieldNames,
                 std::vector<std::string>& fieldValues) throw(std::runtime_error);

//---------------------------------------------------------------------------
// Adds the specified field to the specified dataset.
// The fieldValue is used to determine the datatype of the field.
// A field is not added if it already exists.
//---------------------------------------------------------------------------
void EXPORT addDBField(TDataSet* dataset,
                const std::string& fieldName,
                const std::string& fieldValue);

//---------------------------------------------------------------------------
// Return a list of field names to caller for the specified dataset.
//---------------------------------------------------------------------------
void EXPORT getDBFieldNames(TDataSet* dataset,
                     std::vector<std::string>& fieldNames);

// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// Will through runtime_error if cannot convert the values to a float.
// ------------------------------------------------------------------
void EXPORT getDBFieldValues(TDataSet* dataset,
                      const std::string& fieldName,
                      std::vector<double>& values) throw (std::runtime_error);
// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// ------------------------------------------------------------------
void EXPORT getDBFieldValues(TDataSet* dataset,
                      const std::string& fieldName,
                      std::vector<std::string>& values);
// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// ------------------------------------------------------------------
void EXPORT getDBFieldValues(TDataSet* dataset,
                      const std::string& fieldName,
                      std::vector<unsigned>& values);

//---------------------------------------------------------------------------
// Copy a record from the source dataset to the destination dataset.
// Returns true if a record was copied.
// The newly appended record will be the current record after this operation.
//---------------------------------------------------------------------------
bool EXPORT copyDBRecord(TDataSet* source, TDataSet* destination);

//---------------------------------------------------------------------------
// Append a new record to the specified dataset.
// Then add the specified values to the new record and post the changes.
//---------------------------------------------------------------------------
void EXPORT appendDBRecord(TDataSet* dataset,
                    std::vector<std::string>& fieldNames,
                    std::vector<std::string>& fieldValues) throw(std::runtime_error);

void EXPORT appendDBRecordNoErrors(TDataSet* dataset,
                            std::vector<std::string>& fieldNames,
                            std::vector<std::string>& fieldValues);
//---------------------------------------------------------------------------
// Return a DB value to caller - as a string. Missing values will be a
// blank string.
//---------------------------------------------------------------------------
string EXPORT getDBValue(TDataSet* dataset, const std::string& fieldName);

//---------------------------------------------------------------------------
// Return a DB value to caller - as a double. To test for a missing value
// call isMissing function.
//---------------------------------------------------------------------------
double EXPORT getDBDouble(TDataSet* dataset, const std::string& fieldName);

//---------------------------------------------------------------------------
// Return a DB value to caller - as an unsigned. To test for a missing value
// call isMissing function.
//---------------------------------------------------------------------------
unsigned EXPORT getDBUnsigned(TDataSet* dataset, const std::string& fieldName);

// ------------------------------------------------------------------
// Execute the specified query.
// ------------------------------------------------------------------
void EXPORT executeQuery(Adodb::TADOConnection* connection, const string& sql);

// ------------------------------------------------------------------
// Run the specified query. Caller should delete the returned query
// when done.
// ------------------------------------------------------------------
TDataSet* EXPORT runQuery(Adodb::TADOConnection* connection, const string& sql);

// ------------------------------------------------------------------
// Fill dataset from the contents of the specified grid.
// ------------------------------------------------------------------
void EXPORT fillDataSetFromGrid(TDataSet* data, TStringGrid* grid, const string& seriesName = "");

#endif
