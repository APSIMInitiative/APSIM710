#ifndef ThingFactoryH
#define ThingFactoryH
//---------------------------------------------------------------------------
// This function creates parts.
//---------------------------------------------------------------------------
plantThing* createThing(XMLNode parameters, ScienceAPI& api, plantInterface& plant, const std::string& type, std::string& name);




#endif
