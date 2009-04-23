//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Probability.h"
#include "DataContainer.h"
#include "DataProcessor.h"
#include <generalvcl\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\xml.h>

using namespace std;

//---------------------------------------------------------------------------
// this function creates probability distributions from source data.
//---------------------------------------------------------------------------
void processProbability(DataContainer& parent,
                        const XMLNode& properties,
                        vector<TDataSet*> sources,
                        TDataSet& result)
   {
   vector<string> fieldNames = parent.reads(properties, "fieldname");
   bool exceedence = Str_i_Eq(parent.read(properties, "exceedence"), "yes");
   if (sources.size() == 1)
      {
      if (!result.Active)
         {
         result.FieldDefs->Clear();
         copySeriesFieldDefs(sources[0], result);

         for (unsigned f = 0; f != fieldNames.size(); f++)
            addDBField(&result, fieldNames[f].c_str(), "1.0");
         addDBField(&result, "Probability", "1.0");
         if (result.FieldDefs->Count > 0)
            result.Active = true;
         }

      if (result.Active)
         {
         int numValues = 0;
         vector<vector<double> > values, probValues;
         for (unsigned f = 0; f != fieldNames.size(); f++)
            {
            values.push_back(vector<double>());
            probValues.push_back(vector<double>());
            getDBFieldValues(sources[0], fieldNames[f], values[f]);
            Calculate_prob_dist(values[f], exceedence, probValues[f]);
            numValues = values[f].size();
            }

         // Now loop through all values and append a record for each.
         sources[0]->First();
         for (int recordNum = 0; recordNum < numValues; recordNum++)
            {
            result.Append();
            copySeriesValues(sources[0], result);
            for (unsigned f = 0; f != fieldNames.size(); f++)
               {
               result.FieldValues[fieldNames[f].c_str()] = values[f][recordNum];
               if (f == 0)
                  {
                  double value = probValues[f][recordNum] + 0.0005;
                  Round_to_nearest(value, 0.001, false);
                  result.FieldValues["Probability"] = value;
                  }
               }
            result.Post();
            }
         }
      }
   }

