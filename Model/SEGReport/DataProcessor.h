//---------------------------------------------------------------------------
#ifndef DataProcessorH
#define DataProcessorH

#include <db.hpp>

class DataContainer;
//---------------------------------------------------------------------------
// This function performs some data processing based on the specified
// parent and xml.
//---------------------------------------------------------------------------
void processData(DataContainer& parent, const std::string& xml, TDataSet& result);

//---------------------------------------------------------------------------
// Called by processor functions to copy the fielddefs for all 'series'
// fields i.e. 'series' and 'title'
//---------------------------------------------------------------------------
void copySeriesFieldDefs(TDataSet* source, TDataSet& result);

//---------------------------------------------------------------------------
// Called by processor functions to copy the values for all 'series'
// fields i.e. 'series' and 'title'
//---------------------------------------------------------------------------
void copySeriesValues(TDataSet* source, TDataSet& result);

#endif
