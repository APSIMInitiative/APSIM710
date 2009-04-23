//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "SeriesSplitter.h"
#include "DataContainer.h"
#include <general/stl_functions.h>
#include <generalvcl/db_functions.h>
using namespace std;


//---------------------------------------------------------------------------
// This function returns a series number for the current record of 'data'.
// This series number is an index into the 'seriesnames' vector.
//---------------------------------------------------------------------------
int calcSeries(TDataSet* data, const vector<string>& fieldNames, vector<string>& seriesNames)
   {
   string seriesName = "";
   for (unsigned i = 0; i != fieldNames.size(); i++)
      {
      if (seriesName != "")
         seriesName += ", ";
      seriesName += String(data->FieldValues[fieldNames[i].c_str()]).c_str();
      }
   vector<string>::iterator i = find_if(seriesNames.begin(), seriesNames.end(),
                                        CaseInsensitiveStringComparison(seriesName));
   if (i == seriesNames.end())
      {
      seriesNames.push_back(seriesName);
      return seriesNames.size();
      }
   else
      return i - seriesNames.begin() + 1;
   }

//---------------------------------------------------------------------------
// this function filters an existing dataset.
//---------------------------------------------------------------------------
void processSeriesSplitter(DataContainer& parent,
                           const XMLNode& properties,
                           vector<TDataSet*> sources,
                           TDataSet& result)
   {
   static vector<string> seriesNames;
   vector<string> fieldNames = parent.reads(properties, "FieldName");

   if (sources.size() == 1 && fieldNames.size() > 0)
      {
      TDataSet* source = sources[0];
      if (!result.Active)
         {
         source->First();
         int seriesNumber = calcSeries(source, fieldNames, seriesNames);

         result.FieldDefs->Assign(source->FieldDefs);
         addDBField(&result, "SeriesName", seriesNames[seriesNumber-1].c_str());
         addDBField(&result, "SeriesNumber", "1");
         seriesNames.clear();
         result.Active = true;
         }

      // loop through all records.
      source->First();
      while (!source->Eof)
         {
         // add a new record that is identical to the current source record.
         copyDBRecord(source, &result);

         // calculate a series number and number for this record.
         int seriesNumber = calcSeries(source, fieldNames, seriesNames);

         result.Edit();
         result.FieldValues["SeriesName"] = seriesNames[seriesNumber-1].c_str();
         result.FieldValues["SeriesNumber"] = seriesNumber;
         result.Post();

         source->Next();
         }
      }


   }

