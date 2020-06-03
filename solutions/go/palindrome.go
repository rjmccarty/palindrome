package main
import ("strings"; "bufio"; "fmt";  "log";  "flag"; "os")

func reverseString(str string) string {
  runes := []rune(str)  // Convert to runes
  
  for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
    runes[i], runes[j] = runes[j], runes[i]
  }
  
  return string(runes)  // Convert back
}

func isPalindrome(str string) bool {
  var strupcase string = strings.ToUpper(str)
  
  if (reverseString(strupcase) == strupcase) {
    return true
  }

  return false
}

func processRecord(str string) {
  var firstname, lastname string
  var fnispalindrome string = " NO"
  var lnispalindrome string = " NO"

  fmt.Sscanf(str, "%25s%25s", &firstname, &lastname)
  firstname = strings.TrimRight(firstname, " ")
  lastname = strings.TrimRight(lastname, " ")
    
  if (isPalindrome(firstname)) {
    fnispalindrome = "YES"
  }

  if (isPalindrome(lastname)) {
    lnispalindrome = "YES"
  }

  fmt.Printf("%3s %-25s %3s %-25s\n", fnispalindrome, firstname,
    lnispalindrome, lastname)

  return;
}

func main() {
  var infilename string
  var recordsProcessed int = 0
  var err error
  
  maxRecords := flag.Int("c", 0, "Maximum records to process.")
  report := flag.Bool("r", false, "Report number of records processed.")
  flag.Parse();

  if *maxRecords < 0 {
    log.Fatalf("ERROR: Invalid count. Must be >= 0.")
  }

  file := os.Stdin	// Default

  if infilename = flag.Arg(0); infilename != "" {
    file, err = os.Open(infilename)

    if err != nil {
      log.Fatalf("ERROR: %s.", err)
    }
  }
  
  scanner := bufio.NewScanner(file)
  scanner.Split(bufio.ScanLines)

  for (((*maxRecords == 0) || (recordsProcessed < *maxRecords)) && scanner.Scan()) {
    processRecord(scanner.Text())
    recordsProcessed++
  }

	if *report == true {
	  fmt.Println(recordsProcessed, " records processed.")
	}
	
  file.Close()
  os.Exit(0)
}
