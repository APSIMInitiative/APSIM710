//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Regression.h"
#include "DataContainer.h"
#include "DataProcessor.h"
#include <generalvcl\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>

using namespace std;
//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void processRegression(DataContainer& parent,
                       const XMLNode& properties,
                       vector<TDataSet*> sources,
                       TDataSet& result)
   {
   string XFieldName = parent.read(properties, "XFieldName");
   string YFieldName = parent.read(properties, "YFieldName");
   if (XFieldName != "" && YFieldName != "" && sources.size() == 1)
      {
      TDataSet* source = sources[0];
      if (!result.Active)
         {
         result.FieldDefs->Clear();
         copySeriesFieldDefs(source, result);

         addDBField(&result, "RegrX", "1.0");
         addDBField(&result, "RegrY", "1.0");
         addDBField(&result, "1:1X", "1.0");
         addDBField(&result, "1:1Y", "1.0");
         addDBField(&result, "Equation", "x");
         addDBField(&result, "m", "1.0");
         addDBField(&result, "c", "1.0");
         addDBField(&result, "r2", "1.0");
         addDBField(&result, "n", "1.0");
         addDBField(&result, "StdErr(m)", "1.0");
         addDBField(&result, "StdErr(c)", "1.0");
         addDBField(&result, "RMSD", "1.0");
         result.Active = true;
         }

      // Loop through all series blocks and all records within that series.
      vector<double> x,y;
      source->First();
      while (!source->Eof)
         {
         try
            {
            double xValue = source->FieldValues[XFieldName.c_str()];
            double yValue = source->FieldValues[YFieldName.c_str()];
            x.push_back(xValue);
            y.push_back(yValue);
            }
         catch (...)
            {
            }
         source->Next();
         }
      if (x.size() != 0 && y.size() != 0)
         {
         source->First();
         Regr_stats stats;
         calcRegressionStats(x, y, stats);

         double minX = *min_element(x.begin(), x.end());
         double maxX = *max_element(x.begin(), x.end());
         //double minY = *min_element(y.begin(), y.end());
         //double maxY = *max_element(y.begin(), y.end());

         string equation;
         equation = " y = " + ftoa(stats.m, 2) + " x + " + ftoa(stats.c, 2);
         equation += " (r2 = " + ftoa(stats.R2, 2) + ")";

         result.Append();
         copySeriesFieldDefs(source, result);
         result.FieldValues["RegrX"] = minX;
         result.FieldValues["RegrY"] = stats.m * minX + stats.c;
         result.FieldValues["1:1X"] = minX;
         result.FieldValues["1:1Y"] = minX;
         result.FieldValues["Equation"] = equation.c_str();
         result.FieldValues["m"] = stats.m;
         result.FieldValues["c"] = stats.c;
         result.FieldValues["r2"] = stats.R2;
         result.FieldValues["n"] = x.size();
         result.FieldValues["StdErr(m)"] = stats.SEslope;
         result.FieldValues["StdErr(c)"] = stats.SEcoeff;
         result.FieldValues["RMSD"] = stats.RMSD;
         result.Post();
         result.Append();
         result.FieldValues["RegrX"] = maxX;
         result.FieldValues["RegrY"] = stats.m * maxX + stats.c;
         result.FieldValues["1:1X"] = maxX;
         result.FieldValues["1:1Y"] = maxX;
         result.FieldValues["Equation"] = equation.c_str();
         result.FieldValues["m"] = stats.m;
         result.FieldValues["c"] = stats.c;
         result.FieldValues["r2"] = stats.R2;
         result.FieldValues["n"] = x.size();
         result.FieldValues["StdErr(m)"] = stats.SEslope;
         result.FieldValues["StdErr(c)"] = stats.SEcoeff;
         result.FieldValues["RMSD"] = stats.RMSD;
         result.Post();
         }
      }
   }

