#include <stdlib.h>
#include "stristr.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <Shlwapi.h>

char * stristr(const char *text_str, const char *srch_str)
/*	If the string 'text_str' contains a substring equal to the
 * string 'srch_str' (case insensitive), then this function will return
 * a pointer to that substring.  Otherwise, NULL will be returned.
 */
{	
	return StrStrI(text_str, srch_str);
}

