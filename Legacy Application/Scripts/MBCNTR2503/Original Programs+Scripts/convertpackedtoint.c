/* ******************************************

	ConvertPackedToInt

****************************************** */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>

void unpackit(unsigned char *pack, unsigned char *unpack, int plen);

int ConvertPackedToInt(unsigned char *pack, int plen)

{	unsigned char *unpack, *s;
	int ulen, sign, k;

	ulen = plen*2+1;
	unpack = (unsigned char *)malloc(ulen);
   memset(unpack, 0, ulen);
   unpackit(pack, unpack, plen);
   s = unpack+strlen((char *)unpack)-1;
   if(*s!='d')
      sign = 1;
   else
      sign = -1;
   *s = 0;
   k = atoi((char *)unpack);
   k *= sign;
   free(unpack);
   return(k);
}

