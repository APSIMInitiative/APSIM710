//---------------------------------------------------------------------------
#pragma hdrstop
#include "RealSet.h"
#include "Kruskal_wallis.h"
#include <general\pch.h>
#include <vcl.h>

#include "KWTest.h"
#include "DataContainer.h"
#include "DataProcessor.h"
#include <generalvcl\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <generalvcl\vcl_functions.h>

using namespace std;

static const double KRUSKAL_WALLIS_CRITICAL_VALUE = 0.1;

// ------------------------------------------------------------------
// Loop through all series in specified table and store the values
// as a new distribution in the specified vector of distributions.
// ------------------------------------------------------------------
void AddDistributionFromTable(TDataSet* table, vector<RealSet>& distributions,
                              const string& fieldName)
   {
   RealSet distribution;
   table->First();
   while (!table->Eof)
      {
      if (!table->FieldValues[fieldName.c_str()].IsNull())
         distribution.add(table->FieldValues[fieldName.c_str()]);
      table->Next();
      }
   distributions.push_back(distribution);
   }

//---------------------------------------------------------------------------
// Given the 2 source data sets, write the KW stats to result.
//---------------------------------------------------------------------------
void doKWTest(TDataSet* source1, const string& fieldName1,
              TDataSet* source2, const string& fieldName2,
              TDataSet& result)
   {
   if (source1 != NULL && source2 != NULL && source1->Active && source2->Active &&
       fieldName1 != "" && fieldName2 != "")
      {
      if (!result.Active)
         {
         result.FieldDefs->Clear();
         copySeriesFieldDefs(source1, result);
         addDBField(&result, "PValue", "1.0");
         addDBField(&result, "Description", "a");
         addDBField(&result, "AreNot", "a");
         result.Active = true;
         }

      vector<RealSet> distributions;
      AddDistributionFromTable(source1, distributions, fieldName1);
      AddDistributionFromTable(source2, distributions, fieldName2);

      if (distributions.size() == 2)
         {
         source1->First();
         KruskalWallisResult kwResult = KruskalWallis(distributions);
         double pValue = kwResult.p;

         result.Append();
         copySeriesValues(source1, result);
         result.FieldValues["PValue"] = pValue;
         if (pValue < KRUSKAL_WALLIS_CRITICAL_VALUE)
            {
            result.FieldValues["Description"] = "There is a significant difference between the distributions.";
            result.FieldValues["AreNot"] = "ARE";
            }

         else
            {
            result.FieldValues["Description"] = "There is NO significant difference between the distributions.";
            result.FieldValues["AreNot"] = "ARE NOT";
            }

         result.Post();
         }
      }
   }

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void processKWTest(DataContainer& parent,
                   const XMLNode& properties,
                   vector<TDataSet*> sources,
                   TDataSet& result)
   {
   vector<string> fieldNames = parent.reads(properties, "fieldName");

   if (sources.size() == 2 && fieldNames.size() == 1)
      doKWTest(sources[0], fieldNames[0],
               sources[1], fieldNames[0],
               result);
   else if (sources.size() == 1 && fieldNames.size() == 2)
      doKWTest(sources[0], fieldNames[0],
               sources[0], fieldNames[1],
               result);
   }

