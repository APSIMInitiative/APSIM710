//---------------------------------------------------------------------------
#ifndef RegressionH
#define RegressionH

#include <db.hpp>
class XMLNode;
class DataContainer;

//---------------------------------------------------------------------------
// this creates a dataset that represents a
// regression.
//---------------------------------------------------------------------------
void processRegression(DataContainer& parent,
                       const XMLNode& properties,
                       vector<TDataSet*> sources,
                       TDataSet& result);
#endif
