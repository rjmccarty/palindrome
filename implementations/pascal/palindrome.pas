PROGRAM Palindrome;
// (RJMc) Must be compiled with 'fpc -Mobjfpc palindrome.pas' because
// AssignFile and CloseFile (from SysUtils will not be available
// unless compiled in Object Pascal mode.

{$mode objfpc}{$H+}

USES StrUtils, SysUtils;

CONST
   InFileDefault = 'STDIN';
//   InFileDefault = '../../testfile.txt';
   MaxParms = 2;
   
TYPE
   NameT = OBJECT
     First,
     Last,
     FNIsPalindrome,
     LNIsPalindrome : String;
  END;	

VAR
   InputStream	    : TextFile;
   Name		    : NameT;
   InString	    : String;
   InputFile	    : String;
   MaxRecords	    : Integer;
   RecordsProcessed : Integer = 0;

FUNCTION GetInputFile(): String;
VAR
   InFile      : String = InFileDefault;

BEGIN
   IF (ParamCount > 0) THEN
      InFile := ParamStr(1);

   GetInputFile := InFile;
END;

FUNCTION GetMaxRecords(): Integer;
VAR
   MaxRecords	    : Integer = 0;

BEGIN
   IF (ParamCount > 1) THEN
      MaxRecords := StrToInt(ParamStr(2));

   GetMaxRecords := MaxRecords;
END;

FUNCTION ExtractString(InString : String; StartIndex : Integer; EndIndex : Integer): String;
VAR 
   Extracted : String;

BEGIN
   Extracted := Copy(InString, StartIndex, EndIndex);
   ExtractString := Extracted;
END;

FUNCTION IsPalindrome(InString: String): Boolean;
BEGIN
   IsPalindrome := UpCase(InString) = UpCase(ReverseString(InString));
END;

PROCEDURE TerminateProgram(ExitCode: Integer);
BEGIN
   halt(ExitCode);
END;

// Main Routine
BEGIN
   IF (ParamCount > MaxParms) THEN BEGIN
      writeln(StdErr, 'ERROR: Too many parameters (', ParamCount, ')', sLineBreak);
      TerminateProgram(1);
   END;
   
   MaxRecords := GetMaxRecords();
   InputFile := GetInputFile();

//   writeln('Input File: ', InputFile, ' Max Records: ', MaxRecords);

   IF CompareText(InputFile, 'STDIN') = 0 THEN BEGIN
      InputStream := Input;	// Set to standard input.
   END ELSE BEGIN
      TRY
         AssignFile(InputStream, InputFile);
         Reset(InputStream);
      EXCEPT ON E: EInOutError DO BEGIN
            writeln(StdErr, 'ERROR: ', E.Message, ' (', InputFile, ')');
	    TerminateProgram(1);
         END
      END;
   END;

   TRY
      WHILE not EOF(InputStream) DO BEGIN
         IF (MaxRecords > 0) and (RecordsProcessed >= MaxRecords) THEN
	    Break;
      
	 readln(InputStream, InString);

	 Name.First := TrimRight(ExtractString(InString, 1, 25));
	 Name.Last := TrimRight(ExtractString(InString, 26, 25));
	 Name.FNIsPalindrome := ' NO';
	 Name.LNIsPalindrome := ' NO';

	 IF (IsPalindrome(Name.First)) THEN
	    Name.FNIsPalindrome := 'YES';

	 IF (IsPalindrome(Name.Last)) THEN
	    Name.LNIsPalindrome := 'YES';

	 writeln(Name.FNIsPalindrome, ' ', PadRight(Name.First, 25), ' ',
		 Name.LNIsPalindrome, ' ', PadRight(Name.Last, 25));

	 inc(RecordsProcessed, 1);
      END;

   EXCEPT
   ON E: EInOutError DO BEGIN
         writeln(StdErr, 'ERROR: ', E.Message, ' (', InputFile, ')');
         TerminateProgram(1);
      END;
   END;

   CloseFile(InputStream);
END.
