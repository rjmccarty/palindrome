      program palindrome
      implicit none
      character, external :: toupper
      character, external :: tolower
      logical :: ispalindrome

      integer :: iocode
      integer :: r_unit = 99
      integer :: w_unit = 6
      character(len=80) :: string
      integer :: i
      character(len=255) :: arg
      character(len=255) :: infilename = ""
      character(len=25) :: firstname = ""
      character(len=25) :: lastname = ""
      character(len=3) :: fnispalindrome = ""
      character(len=3) :: lnispalindrome = ""
      character(len=128) :: maxrecords_in
      integer :: maxrecords = 0
      integer :: recsprocessed = 0
      
       if (iargc() > 0) then
         call getarg(1, infilename)
      end if

      if (len_trim(infilename) <= 0) then
         r_unit = 5
      else
         open (unit=r_unit, file=infilename, iostat=iocode, status="old")

         if (iocode /= 0) then
            stop "ERROR: Cannot open file."
         end if
      end if

      if (iargc() > 1) then
         call getarg(2, maxrecords_in)
         read(maxrecords_in, *) maxrecords
      end if

10    format(a25, a25)
20    format(a3, ' ', a25, ' ', a3, ' ', a25)

      do
         if ((maxrecords > 0) .and. (recsprocessed >= maxrecords)) then
            exit
         end if

         read (r_unit, "(a)", iostat=iocode) string

         if (iocode < 0)  then
            exit
         else if (iocode > 0) then
            print *, "File error. Exiting."
            exit
         end if

         read(string, 10) firstname, lastname
         string = trim(string)
         
         if (ispalindrome(firstname)) then
            fnispalindrome = "YES"
         else
            fnispalindrome = " NO"
         endif

         if (ispalindrome(lastname)) then
            lnispalindrome = "YES"
         else
            lnispalindrome = " NO"
         endif

         write(w_unit, 20) fnispalindrome, firstname, lnispalindrome, lastname

         recsprocessed = recsprocessed + 1
      enddo

!      print *, "Records Processed: ", recsprocessed
      end program palindrome

      logical function ispalindrome(instring)
      implicit none
      character(len=255) :: toupper
      character(len=255) :: tolower
      character(len=*), intent(in) :: instring
      integer :: i, j, length
      logical :: temp = .true.
      character(len=25) :: char_str

      char_str = instring
      char_str = toupper(char_str)
      length = len_trim(char_str)

      do i=1, (length/2)
         j = length - i + 1

         if (char_str(i:i) == char_str(j:j)) then
            temp = .true.
         else
            temp = .false.
            exit
         endif
      end do

      ispalindrome = temp
      end function ispalindrome

      character(*) function toupper(str)
      implicit none
      character(len=*), intent(in out) :: str
      integer :: i

      do i = 1, len(str)
      select case(str(i:i))
         case("a":"z")
           str(i:i) = achar(iachar(str(i:i))-32)
      end select
      end do

      toupper = str
      end function toupper
 
      character(*) function tolower(str)
      implicit none
      character(len=*), intent(in out) :: str
      integer :: i
 
      do i = 1, len(str)
        select case(str(i:i))
          case("A":"Z")
            str(i:i) = achar(iachar(str(i:i))+32)
        end select
      end do  

      tolower = str
      end function tolower
