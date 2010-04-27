#ifndef ApsimRegistrationTypeH
#define ApsimRegistrationTypeH

#include <string>
#include <General/platform.h>

enum EventTypeCode {get=1,         respondToGet=2,
                    set=9,         respondToSet=3,
                    event=5,       respondToEvent=6,
                                   respondToGetSet=4,
                                   invalid=100};

EventTypeCode EXPORT stringToTypeCode(const std::string&);
const std::string EXPORT typeCodeToString (EventTypeCode type);
EventTypeCode  EXPORT opposite(EventTypeCode type);

#endif
