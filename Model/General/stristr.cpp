#include <stdlib.h>
#include "stristr.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>

char * stristr(const char *text_str, const char *srch_str)
/*	If the string 'text_str' contains a substring equal to the
 * string 'srch_str' (case insensitive), then this function will return
 * a pointer to that substring.  Otherwise, NULL will be returned.
 */
{	static char table[256];
	static int firstcall=true;
	char *text, *txt, *srch, *sr, *past;
	int txt_len, sr_len, i;
	char sr1;

/* Initialise if not done already. */
	if (firstcall)
	{	for (i=0; i<sizeof(table); i++)
			table[i] = (char) toupper(i);
		firstcall = false;
	}

	txt_len = strlen(text_str);
	sr_len = strlen(srch_str);
	if (sr_len == 0)
		return (char*) text_str;
	if (sr_len > txt_len)
		return NULL;

	text = (char *)text_str;
	srch = (char *)srch_str;
	past = text + (txt_len - sr_len + 1);
	sr1 = table[*srch++];

	do
	{	if (table[*text++] == sr1)
		{	txt = text;
			sr = srch;
			do
			{	if (*sr == '\0')
					return (char*)(text-1);
			} while (table[*txt++] == table[*sr++]);
		}
	} while (text != past);

	return NULL;
}

