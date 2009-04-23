//---------------------------------------------------------------------------

#ifndef CumulativeH
#define CumulativeH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// this function adds a cumulative column for each source column
//---------------------------------------------------------------------------
void processCumulative(DataContainer& parent,
                       const XMLNode& properties,
                       vector<TDataSet*> sources,
                       TDataSet& result);
#endif
