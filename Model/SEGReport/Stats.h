//---------------------------------------------------------------------------
#ifndef StatsH
#define StatsH

#include <db.hpp>
class XMLNode;
class DataContainer;

//---------------------------------------------------------------------------
// This creates a dataset that provides some
// simple stats on a source dataset variable. e.g. mean, median, percentiles...
//---------------------------------------------------------------------------
void processStats(DataContainer& parent,
                  const XMLNode& properties,
                  vector<TDataSet*> sources,
                  TDataSet& result);

#endif
