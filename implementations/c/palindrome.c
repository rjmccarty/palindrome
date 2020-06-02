/*
 * FILENAME :        palindrome.c
 *
 * DESCRIPTION :
 *       Determines whether names in an input file are palindromes.
 *
 * USAGE :
 *	palindrome -c <numrecords> <infile>
 *
 *	where
 *		numrecords = the number of records in "infile" to process.
 *		infile = the pathname of the input file. If infile is not
 *			specified, stdin is used.
 *
 * INPUT :
 *       Each line of the input file contains 25 character wide first and last
 *	name fields, left justified and blank (space) filled. For example (the
 *	numbers are for reference showing the field widths):
 *
 *	0123456789012345678901234501234567890123456789012345
 *	Joe                      Smith
 *	
 *
 * OUTPUT :
 *	Each first and last name is written to stdout with an indication
 *	whether the field is a palindrome, in the following format:
 *
 *	01234567890123456789012345012345678901234567890123456789012
 *      xxx xxxxxxxxxxxxxxxxxxxxxxxxx xxx xxxxxxxxxxxxxxxxxxxxxxxxx
 *
 *	as in these examples:
 *
 *	01234567890123456789012345012345678901234567890123456789012
 *	 NO Fred                      YES Laval
 *	YES Hannah                    YES Hamamah                  
 *	YES Nan                        NO Nanunanu
 *
 * NOTE :
 *	The palindrome check is case-insensitive.
 *
 * AUTHOR :
 *	Rick McCarty (20200506)
 *
 * UPDATED: 20200518: Reformat, add error output for file open.
 *		Fixed character count lines in above comments to
 *		remove extra 0s. Add -r option.
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#define TRUE  (1==1)
#define FALSE (!TRUE)
#define OPTLIST "c:r"
#define USAGE "USAGE: %s [-c <numrecords>] [-r] [<infile>]\n"
#define MAXPARMS 1	/* Maximum number of positional parameters */

struct parms *processOptions(int, char **);
int processFile(struct parms *);
int isPalindrome(char *);
char *str2upper(char *);

typedef struct parms {
  int reccount;
  int report;
  char *inputFile;
} parms;

char *str2upper(char *str) {
  char *curch = str;
  while (*curch) {
    *curch = toupper((unsigned char) *curch);
    ++curch;
  }

  return (str);
}

struct parms *processOptions(int argc, char **argv) {
  extern char *optarg;
  extern int optind;
  extern int opterr;
  char **arguments;
  int ch;
  int reccount = 0;
  int report = 0;
  char *inputFile = NULL;
  struct parms *cmdparms = NULL;
  
  while ((ch = getopt (argc, argv, OPTLIST)) >= 0)
    switch (ch) {
      case 'c':
        reccount = atoi(optarg);
        break;

      case 'r':
	++report;
	break;
	
      case '?':
      default:
	fprintf(stderr, USAGE, argv[0]);
        return (NULL);
    }

   if (argc - optind > MAXPARMS) {
     fprintf(stderr, "Too many positional parameters.\n");
     fprintf(stderr, USAGE, argv[0]);
     return (NULL);
   }

   cmdparms = calloc(1, sizeof(struct parms));
   cmdparms->reccount = reccount;
   cmdparms->report = report;
   arguments = &argv[optind];

   if (argc == optind)		/* No input file name - use stdin */
     cmdparms->inputFile = NULL;
   else
     cmdparms->inputFile = arguments[0];
   
   return (cmdparms);
}

int processFile(struct parms *cmdparms) {
  FILE *fp;
  int count = 0;
  char firstname[30];
  char lastname[30];

  if (cmdparms->inputFile == NULL)
    fp = stdin;
  else {
    fp = fopen(cmdparms->inputFile, "r");

    if (!fp) {
      fprintf(stderr, "ERROR: Can't open file (%s)\n%s\n",
	      cmdparms->inputFile, strerror(errno));
      return (-1);
    }
  }

  while (fscanf(fp, "%25s%25s\n", firstname, lastname) != EOF) {
    fprintf(stdout, "%3s %-25s %3s %-25s\n",
	    isPalindrome(firstname) ? "YES" : "NO",
	    firstname,
	    isPalindrome(lastname) ? "YES" : "NO",
	    lastname);

    fflush(stdout);

    if (cmdparms->reccount > 0 && ++count >= cmdparms->reccount)
      break;
  }

  if (cmdparms->report)
    printf("Records processed: %d\n", count);
  
  fclose(fp);
  return (0);
}

int isPalindrome(char *str) {
  char *dupstr = strdup(str);
  int curch = 0; 
  int endch = strlen(dupstr) - 1; 
  dupstr = str2upper(dupstr);
  
  /*
   * Loop through string, comparing begin/end chars and iterating toward
   * the middle of the string. If we reach the middle without a mismatch,
   * it is a palindrome;
   */
  while (endch > curch) { 
    if (dupstr[curch++] != dupstr[endch--]) { 
      free(dupstr);
      return (FALSE); 
    } 
  } 

  free(dupstr);
  return (TRUE);
}

int main (int argc, char **argv) {
  struct parms *cmdparms = NULL;

  setvbuf(stdout, NULL, _IONBF, 0);
  
  if ((cmdparms = processOptions(argc, argv)) == NULL)
    return (1);

  (void) processFile(cmdparms);
  free(cmdparms);
  return (0);
}
