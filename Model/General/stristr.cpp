#include <../General/pch.h>
#include <stdlib.h>
#include "stristr.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifdef __WIN32__
#include <Shlwapi.h>
#endif

char * stristr(const char *text_str, const char *srch_str)
/*	If the string 'text_str' contains a substring equal to the
 * string 'srch_str' (case insensitive), then this function will return
 * a pointer to that substring.  Otherwise, NULL will be returned.
 */
{	
#ifdef __WIN32__
	return StrStrI(text_str, srch_str);
#else
#ifdef __GNUC__
	return strcasestr(text_str, srch_str);
#endif
#endif
}
