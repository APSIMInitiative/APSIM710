#ifndef SeriesSplitterH
#define SeriesSplitterH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// this function filters an existing dataset.
//---------------------------------------------------------------------------
void processSeriesSplitter(DataContainer& parent,
                           const XMLNode& properties,
                           vector<TDataSet*> sources,
                           TDataSet& result);
#endif
