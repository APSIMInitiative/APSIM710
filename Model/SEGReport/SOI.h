//---------------------------------------------------------------------------
#ifndef SOIH
#define SOIH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// derived from DataProcessor, this adds an SOI column
//---------------------------------------------------------------------------
void processSOI(DataContainer& parent,
                const XMLNode& properties,
                vector<TDataSet*> sources,
                TDataSet& result);
#endif
