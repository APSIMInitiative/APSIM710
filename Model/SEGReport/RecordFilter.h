//---------------------------------------------------------------------------
#ifndef RecordFilterH
#define RecordFilterH

#include <db.hpp>
class XMLNode;
class DataContainer;

//---------------------------------------------------------------------------
// This filters an existing dataset extracting specified records.
//---------------------------------------------------------------------------
void processRecordFilter(DataContainer& parent,
                         const XMLNode& properties,
                         vector<TDataSet*> sources,
                         TDataSet& result);

#endif
