/* ******************************************

****************************************** */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>

char dehexify(char c)

{	if (c>=10)
		c+='a'-10;
	else
		c+='0';
	return(c);
}

