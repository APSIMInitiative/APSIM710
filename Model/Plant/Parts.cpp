#include "StdPlant.h"

#include "Parts.h"

using namespace std;

//  initialise data members.
Parts::Parts(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : CompositePart(parameters, scienceAPI, p, name, false)
   {
   deleteChildren = false;
   findParts(parameters);
   }

void Parts::findParts(XMLNode parameters)
   {
   //---------------------------------------------------------------------------
   // This is a proxy class so go find all specified child parts
   // and add them to our collection.
   //---------------------------------------------------------------------------
   for (XMLNode::iterator child = parameters.begin();
                          child != parameters.end();
                          child++)
      {
      string childName = child->getName();
      add(&plant->All().find(childName));
      }
   }

