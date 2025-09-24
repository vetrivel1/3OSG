/* ******************************************

	makeunixpath.c

****************************************** */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char *MakeUnixPath(char *path, char *directory)

{  char *s, *t, *u;
	int i, len, ulen;

	len = strlen(path);
	for(s=path, i=0; *s!=0 && *s!=':'; s++, i++)
   	;
   if(*s==':')
   {  len -= i+1;
   	memmove(path, s+1, len);
      *(s+len-1)=0;
   }
   for(s=path; s-path<len; s++)
      if(*s=='\\')
      	*s = '/';
      else
      if(isupper(*s))
         *s = tolower(*s);
      if(*path=='/')
      	u=strdup(path);
      else
      {	ulen = strlen(path)+strlen(directory)+1;
      	u=(char *)malloc(ulen);
      	memset(u, 0, ulen);
         sprintf(u, "%s%s", directory, path);
      }
   return(u);
}

