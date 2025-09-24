/* ******************************************

	ParseExpression

****************************************** */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/mtio.h>
#include "cnp01.h"

parse_rec *ParseExpression(char *r, int *StringCount, char c, int UseQuotes, int UseParens, int UseEOR)
// this works except for "skipped" items, i.e., where string will be NULL
{  int Parens;
   int Quotes;
   int done;
   char *s, *t, *u, *v;
   parse_rec *Parse, *ps;
   int ParseLimit, LastParsedStringIsEmpty;

	ParseLimit=100;
   Parse=(parse_rec *)malloc(ParseLimit*sizeof(parse_rec));
   memset(Parse, 0, ParseLimit*sizeof(parse_rec));
	s = strdup(r);
   for(t=s+strlen(s)-1; t>=s && (*t=='\r' || *t=='\n'); t--)
   	*t=0;
   if(*t==c)
   	LastParsedStringIsEmpty=1;
   else
   	LastParsedStringIsEmpty=0;
   for(t=s, v=s+strlen(s), ps=Parse; t<v; t++, ps++)
   {  if(ps-Parse == ParseLimit)
      {  ParseLimit += 100;
         Parse = (parse_rec *)realloc(Parse, ParseLimit * sizeof(parse_rec));
         ps = Parse+ParseLimit-100;
         memset(ps, 0, 100 * sizeof(parse_rec));
      }
      for(u=t, Parens=0, Quotes=0, done=0; !done; u++)
      {  if(UseQuotes && *u=='"')
			   Quotes = !Quotes;
			if(UseParens && *u=='(' && !Quotes)
				Parens++;
         else
         if(UseParens && *u==')' && Parens && !Quotes)
      	    Parens--;
         if(*u==c && !Parens && !Quotes || *u==0)
            done=1;
      }
      u--;
      *u=0;
      while(t<u && *t==' ')
      {  *t=0;
         t++;
      }
      if(UseQuotes && *t=='"')
      {   memmove(t, t+1, strlen(t)-1);
   	     u--;
          *u=0;
          u--;
          *u=0;
          ps->Type = 0;
          u+=2;
      }
      else
         ps->Type = 1;
      if(u>=t && UseQuotes && *u=='"')
      	*u=0;
      if(UseParens && Parens || UseQuotes && Quotes)
	   {	if(Parse->String!=NULL)
      		free(Parse->String);
      	Parse->Type=-1;
         Parse->String=(char *)malloc(100+strlen(s));
      	sprintf(Parse->String, "\nMismatched parens or quotes:  %s", s);
   	  	return(Parse);
	   }
      ps->String = strdup(t);
      t=u;
   }
//   for(ps=Parse; ps-Parse<StringCount; ps++)
//   	printf("\nps[%d] = <%s>", ps-Parse, ps->String);
	if(LastParsedStringIsEmpty)
   {  if(ps-Parse == ParseLimit)
      {  ParseLimit += 1;
         Parse = (parse_rec *)realloc(Parse, ParseLimit * sizeof(parse_rec));
	      ps = Parse+ParseLimit-1;
         memset(ps, 0, sizeof(parse_rec));
      }
      ps->String = strdup("");
      ps++;
   }
	if(StringCount!=NULL)
   	if(UseEOR)
	   	*StringCount=ps-Parse;
      else
      	*StringCount=ps-Parse-1;
   free(s);
   return(Parse);
}

void RemoveParse(parse_rec *p0, int p0Limit)

{	parse_rec *p1;

	for(p1=p0; p1-p0<p0Limit && p1->String!=NULL; p1++)
   	free(p1->String);
   free(p0);
	return;
}

void GetStringsFromParse(string_rec *sr, char *ParseString, char ParseChar)

{  parse_rec *Value, *v1;
   int ValueCount;
   char **u1;

  	Value=ParseExpression(ParseString, &ValueCount, ParseChar, 0, 0, 0);
  	sr->u0Limit=sr->u0Count=ValueCount;
   sr->u0=(char **)malloc(sr->u0Limit*sizeof(char *));
   for(u1=sr->u0, v1=Value; u1-sr->u0<sr->u0Count; u1++, v1++)
   	*u1=strdup(v1->String);
	RemoveParse(Value, ValueCount);
   return;
}

void GetStringsFromFile(string_rec *sr, char *path)

{	FILE *Handle;
	char *scratch, *s, **u1;

	Handle=fopen(path, "rt");
  	if(Handle==NULL)
   {	printf("\nSplit file \"%s\" open failure", path);
     	exit(1);
   }
   sr->u0Count=sr->u0Limit=0;
   sr->u0=NULL;
   scratch=(char *)malloc(8192);
	fgets(scratch, 8191, Handle);
   while(!feof(Handle))
   {	if(sr->u0Count==sr->u0Limit)
     	{	sr->u0Limit+=20;
        	sr->u0=(char **)realloc(sr->u0, sr->u0Limit*sizeof(char *));
         memset(sr->u0+sr->u0Count, 0, 20*sizeof(char *));
      }
      u1=sr->u0+sr->u0Count;
      for(s=scratch+strlen(scratch)-1; s>=scratch && (*s=='\r' || *s=='\n'); s--)
      	*s=0;
      *u1=strdup(scratch);
      sr->u0Count++;
      fgets(scratch, 8191, Handle);
   }
   fclose(Handle);
}

