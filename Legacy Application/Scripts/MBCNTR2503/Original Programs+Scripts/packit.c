/* ******************************************

	packit

****************************************** */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>

unsigned char hexify(unsigned char c);

void packit(unsigned char *unpack, unsigned char *pack)

{	int ulen, plen;
	unsigned char *s0, *s1, *s2, *t1;

	ulen = strlen((char *)unpack);
	plen = (ulen + 1) / 2;
	memset(pack, 0, plen*sizeof(char));
	s0 = (unsigned char *)strdup((char *)unpack);
	for(s1=s0, s2=s0+1, t1=pack; s1-s0<ulen; s1+=2, s2+=2, t1++)
	{
		*s1 = hexify(*s1);
		*s2 = hexify(*s2);
		if (s2-s0<ulen)
			*t1 = (*s1 & 15) << 4 | *s2 & 15;
		else
			*t1 = ((*s1 & 15) << 4);
/*      printf("\nt1[%d]=%02x %d, *s1=%d, *s2=%d", t1-pack, *t1, *t1, *s1, *s2);
*/
	}
	free(s0);
}

unsigned char hexify(unsigned char c)

{	c = tolower(c);
	if (c>='a' && c<='f')
		c = 10 + (c - 'a');
	return(c);
}

