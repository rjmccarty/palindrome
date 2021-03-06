IDENTIFICATION DIVISION.
PROGRAM-ID. PALINDROMECHECK.
 
DATA DIVISION.
LINKAGE SECTION.
    01 TEXTTOCHECK  PIC X(25).
    01 ISPALINDROME PIC X(3) JUSTIFIED RIGHT.
 
PROCEDURE DIVISION USING TEXTTOCHECK, ISPALINDROME.

SET ISPALINDROME TO 'NO'

IF FUNCTION UPPER-CASE(FUNCTION TRIM(TEXTTOCHECK)) EQUAL FUNCTION UPPER-CASE(FUNCTION REVERSE(FUNCTION TRIM(TEXTTOCHECK))) THEN
   SET ISPALINDROME TO 'YES'
END-IF

EXIT PROGRAM.
