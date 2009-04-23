//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Stats.h"
#include "DataContainer.h"
#include "DataProcessor.h"
#include <generalvcl\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <numeric>
#include <list>

using namespace std;

//---------------------------------------------------------------------------
// Return true if the specified array of stats contains the specified stat
//---------------------------------------------------------------------------
bool StatsContains(const vector<string>& stats, const string& name)
   {
   return (find_if(stats.begin(), stats.end(),
                   CaseInsensitiveStringComparison(name)) != stats.end());
   }

//---------------------------------------------------------------------------
// Process a rolling mean.
//---------------------------------------------------------------------------
void processRollingMean(DataContainer& parent,
                        const XMLNode& properties,
                        vector<TDataSet*> sources,
                        TDataSet& result,
                        const vector<string>& fieldNames)
   {
   int numYears = atoi(parent.read(properties, "RollingMean").c_str());

   if (!result.Active && numYears > 0 && sources.size() == 1)
      {
      result.FieldDefs->Clear();
      result.FieldDefs->Assign(sources[0]->FieldDefs);
      result.Active = true;
      }

   if (result.Active && sources.size() == 1)
      {
      TDataSet* source = sources[0];
      source->First();

      vector<list<double> > values;
      for (unsigned i = 0; i != fieldNames.size(); i++)
         values.push_back(list<double>());

      // prime the list.
      for (int i = 1; i < numYears && !source->Eof; i++)
         {
         for (unsigned f = 0; f != fieldNames.size(); f++)
            values[f].push_back(StrToFloat(source->FieldValues[fieldNames[f].c_str()]));
         source->Next();
         }

      // Now we have enough values to calculate our running mean.
      while (!source->Eof)
         {
         copyDBRecord(source, &result);
         result.Edit();
         for (unsigned f = 0; f != fieldNames.size(); f++)
            {
            values[f].push_back(StrToFloat(source->FieldValues[fieldNames[f].c_str()]));
            double RunningMean = accumulate(values[f].begin(), values[f].end(), 0.0);
            RunningMean /= values[f].size();
            result.FieldValues[fieldNames[f].c_str()] = RunningMean;
            values[f].pop_front();
            }
         result.Post();
         source->Next();
         }

      }
   }

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void processStats(DataContainer& parent,
                  const XMLNode& properties,
                  vector<TDataSet*> sources,
                  TDataSet& result)
   {
   vector<string> fieldNames = parent.reads(properties, "fieldname");
   vector<string> stats = parent.reads(properties, "stat");
   bool includeZeros = (parent.read(properties, "IncludeZeros") == "Yes" ||
                        parent.read(properties, "IncludeZeros") == "");

   if (parent.read(properties, "RollingMean") != "")
      processRollingMean(parent, properties, sources, result, fieldNames);
   else
      {
      if (!result.Active && sources.size() == 1)
         {
         result.FieldDefs->Clear();
         copySeriesFieldDefs(sources[0], result);

         // add fields.
         for (unsigned f = 0; f != fieldNames.size(); f++)
            for (unsigned s = 0; s != stats.size(); s++)
               addDBField(&result, fieldNames[f] + "-" + stats[s], "1.0");

         if (result.FieldDefs->Count > 0)
            result.Active = true;
         }

      if (result.Active && sources.size() == 1)
         {
         TDataSet* source = sources[0];

         result.Append();
         copySeriesValues(source, result);

         for (unsigned f = 0; f != fieldNames.size(); f++)
            {
            vector<double> values;
            source->First();

            while (!source->Eof)
               {
               try
                  {
                  double value = StrToFloat(source->FieldValues[fieldNames[f].c_str()]);
                  if (value == 0 && !includeZeros)
                     { }
                  else
                     values.push_back( value );
                  }
               catch (const Exception& err)
                  { }
               source->Next();
               }

            if (values.size() > 0)
               {
               if (StatsContains(stats, "mean"))
                  result.FieldValues[string(fieldNames[f] + "-Mean").c_str()] = Calculate_mean(values);
               if (StatsContains(stats, "Count"))
                  result.FieldValues[string(fieldNames[f] + "-Count").c_str()] = values.size();
               if (StatsContains(stats, "Minimum"))
                  result.FieldValues[string(fieldNames[f] + "-Minimum").c_str()] = min_element(values.begin(), values.end(),
                                                       less<double>());
               if (StatsContains(stats, "Maximum"))
                  result.FieldValues[string(fieldNames[f] + "-Maximum").c_str()] = max_element(values.begin(), values.end(),
                                                       less<double>());
               if (StatsContains(stats, "Sum"))
                  result.FieldValues[string(fieldNames[f] + "-Sum").c_str()] = accumulate(values.begin(), values.end(), 0.0);
               if (StatsContains(stats, "10"))
                  result.FieldValues[string(fieldNames[f] + "-10").c_str()] = Calculate_percentile(values, false, 10);
               if (StatsContains(stats, "20"))
                  result.FieldValues[string(fieldNames[f] + "-20").c_str()] = Calculate_percentile(values, false, 20);
               if (StatsContains(stats, "30"))
                  result.FieldValues[string(fieldNames[f] + "-30").c_str()] = Calculate_percentile(values, false, 30);
               if (StatsContains(stats, "40"))
                  result.FieldValues[string(fieldNames[f] + "-40").c_str()] = Calculate_percentile(values, false, 40);
               if (StatsContains(stats, "50"))
                  result.FieldValues[string(fieldNames[f] + "-50").c_str()] = Calculate_percentile(values, false, 50);
               if (StatsContains(stats, "60"))
                  result.FieldValues[string(fieldNames[f] + "-60").c_str()] = Calculate_percentile(values, false, 60);
               if (StatsContains(stats, "70"))
                  result.FieldValues[string(fieldNames[f] + "-70").c_str()] = Calculate_percentile(values, false, 70);
               if (StatsContains(stats, "80"))
                  result.FieldValues[string(fieldNames[f] + "-80").c_str()] = Calculate_percentile(values, false, 80);
               if (StatsContains(stats, "90"))
                  result.FieldValues[string(fieldNames[f] + "-90").c_str()] = Calculate_percentile(values, false, 90);
               }
            }

         result.Post();
         }
      }
   }
