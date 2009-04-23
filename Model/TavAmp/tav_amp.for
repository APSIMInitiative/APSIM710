* ====================================================================
      program tav_amp
* ====================================================================

      implicit none

*+ Purpose
*      Calculate the AMP (annual amplitude of mean monthly temperature oC)
*      and TAV (Annual average ambient temperature oC)
*      and insert them into the climate file.

*+  Definition
*     This program reads an APSIM climate file, calculates the AMP and TAV
*     site variables and inserts them into the climate file.

*+ Notes
*      This program assumes that the column header contains the substrings
*      'year', ' maxt', and ' mint'. It also assumes that the record
*      immediately following the header record is a units record.

*+ Local Variables
      integer, parameter   :: record_length = 200
      integer, parameter   :: first_year = 1850
      integer, parameter   :: last_year = 2020
      character (len=1), parameter    :: comment = '!'
      character (len=1), parameter    :: blank = ' '
      integer              :: data_end = record_length
      logical              :: record_is_comment = .false.

      character (len=record_length)  :: record  =  blank
      character (len=record_length)  :: record_lc  = blank
      character (len=record_length)  :: record_data_lc  = blank

      character (len=record_length)  :: lower_case ! function
      logical              :: eof = .false.
      character (len=20), dimension(20) :: c_columns
      integer              :: word_count           ! function
      integer              :: num_words            = 0
      integer              :: ios                  = 0
      integer              :: Find_string_in_array ! function
      integer              :: posn_year            = 0
      integer              :: posn_day             = 0
      integer              :: posn_maxt            = 0
      integer              :: posn_mint            = 0
      integer              :: year                 = 0
      integer              :: day                  = 0
      real              :: maxt                    = 0.0
      real              :: mint                    = 0.0
      integer, dimension(3) :: date
      real              :: avet                    = 0.0
      real, dimension(first_year:last_year) :: ampt
      real              :: amp                     = 0.0
      real, dimension(12) :: ave_monthlyt

      real, dimension(1:12, first_year:last_year) :: monthlyt
      integer, dimension(1:12, first_year:last_year) :: monthlyd

      character (len=8)   :: date_str              = blank
      character (len=10)  :: time_str              = blank

      character (len=record_length)
     :        , dimension((last_year-first_year+1)*366) :: recs
      integer              :: nrecs                = 0
      integer              :: nrec_headers         = 0

      integer              ::k                     = 0
      integer              ::i                     = 0

      logical              :: first_data_record    = .false.
      integer              :: start_year           = 0
      integer              :: start_day            = 0
      integer              :: end_year             = 0
      integer              :: end_day              = 0

      character (len=255)  :: infile               = blank
      character (len=255)  :: outfile              = blank

*- Implementation Section -----------------


      call getcl (record)
      num_words = word_count (record)
      if (num_words .eq. 2) then
         read (record, *) infile, outfile
         open (10, infile, status='old')

      else
         print*, ' Incorrect number of arguments'
         print*, ' Must have inputfile outputfile'
         print*, ' Pause... Press Enter to exit'
         read*
         stop
      endif
         ! look for column header line

      print*, 'Reading data from ', trim(infile)

      read (10,'(a)', iostat=ios ) record
      eof = ios.ne.0

      do while  (.not. eof )

         nrecs = 1 + nrecs
         recs(nrecs) = record

         data_end = index (record, comment) - 1
         if (data_end .lt.0) then
            data_end = len_trim(record)
         else
         endif

         if (record .eq. blank
     :      .or. record(:data_end) .eq. blank) then
            data_end = 0
            record_data_lc = blank
            record_is_comment = .true.
         else
            record_data_lc = lower_case (record(:data_end))
            record_is_comment = .false.
         endif

         record_lc = lower_case (record)

         if (index (record_data_lc, 'year') .gt.0
     :   .and. index (record_data_lc, ' maxt') .gt.0
     :   .and. index (record_data_lc, ' mint') .gt.0) then

            print*
            print*, 'Column header record found'
            print*, 'Heading record : ', trim(record)

               ! column headers found; locate columns of interest
            num_words = word_count (record_data_lc)
            read (record_data_lc, *) (c_columns(i), i=1,num_words)
            posn_year = Find_string_in_array('year', c_columns
     :                                       , num_words)
            posn_day = Find_string_in_array('day', c_columns
     :                                       , num_words)
            posn_maxt = Find_string_in_array('maxt', c_columns
     :                                       , num_words)
            posn_mint = Find_string_in_array('mint', c_columns
     :                                       , num_words)
            nrec_headers = nrecs
               ! quit looking for column headers
            exit

         else if (index (record_data_lc, 'amp') .gt.0
     :   .and. index (record_data_lc, '=') .gt.0
     :   .and. index (record_data_lc, '(') .gt.0) then
               ! remove existing amp record
            nrecs = nrecs - 1
            print*, '   AMP record found and removed: '
     :               , '      '//trim(record)


         else if (index (record_data_lc, 'tav') .gt.0
     :   .and. index (record_data_lc, '=') .gt.0
     :   .and. index (record_data_lc, '(') .gt.0) then

               ! remove existing tav record
            nrecs = nrecs - 1
            print*, '   TAV record found and removed: '
     :               , '      '//trim(record)

         else if (record_is_comment
     :   .and. index (record_lc, '"tav_amp"') .gt.0) then

               ! remove existing tav_amp comment record
            nrecs = nrecs - 1
            print*, '   TAV_AMP comment record found and removed: '
     :            , '      '//trim(record)

         else
            ! not column header record
         endif
         read (10,'(a)', iostat=ios ) record
         eof = ios.ne.0

      end do

      if (eof) then
         print*
         print*, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         print*, 'ERROR: Could not find a header record '
     :      //'containing the headers "year", "maxt" and "mint"'
         print*, 'Processing aborted'
         print*, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      else

            ! read units record

         read (10,'(a)', iostat=ios ) record
         nrecs = 1 + nrecs
         recs(nrecs) = record

         print*
         print*, 'Reading temperature data'


            ! now read rest of data collect temperature
         read (10,'(a)', iostat=ios ) record
         print*, 'Starting record: ', trim(record)

         first_data_record = .true.
         eof = ios.ne.0
         monthlyt(:,:) = 0.0
         monthlyd(:,:) = 0

         do while  (.not. eof )
            nrecs = 1 + nrecs
            recs(nrecs) = record

            data_end = index (record, comment) - 1
            if (data_end .lt.0) then
               data_end = len_trim(record)
            else
            endif

            if (record .eq. blank
     :         .or. record(:data_end) .eq. blank) then
               data_end = 0
               record_data_lc = blank
               record_is_comment = .true.
            else
               record_data_lc = lower_case (record(:data_end))
               record_is_comment = .false.
            endif

            record_lc = lower_case (record)

            if (.not. record_is_comment) then
!               read (record_data_lc, *) (c_columns(i), i=1,num_words)
               do i = 1, num_words
                  call get_a_word (record_data_lc, i, c_columns(i))
               enddo
               read (c_columns(posn_year), *) year
               read (c_columns(posn_day), *) day
               read (c_columns(posn_maxt), *) maxt
               read (c_columns(posn_mint), *) mint

            call day_of_year_to_date (day, year, date)
               monthlyt(date(2), year) =  monthlyt(date(2), year)
     :                                 + (maxt + mint) * 0.5
               monthlyd(date(2), year) =  monthlyd(date(2), year) + 1

               if (first_data_record) then
                  start_year = year
                  start_day = day
                  first_data_record = .false.
               else
               endif
            else
            endif

            read (10,'(a)', iostat=ios ) record
            eof = ios.ne.0

         end do
         close(10)
         print*, 'Ending record  : ', trim(record)
         print*, 'Number of records =', nrecs

         end_year = year
         end_day = day

         print*
         print*, 'Calculating TAV and AMP'


            ! zero incomplete years

!         do k = lbound(monthlyd, dim=2), ubound(monthlyd, dim=2)
!            if (sum(monthlyd(:,k), dim = 1) < 365) then
!               monthlyd(:,k) = 0
!            else
!            endif
!         enddo

            ! calculate average monthly temperatures for each year

         where (monthlyd > 0)
            monthlyt = monthlyt/monthlyd
         elsewhere
            monthlyt = -1.0e6
         end where

            ! calculate average monthly temperatures over all years
         ave_monthlyt(:) = -1.0e6
         where (count (monthlyt > -1.0e6, dim=2) > 0)
            ave_monthlyt = sum (monthlyt,dim=2, mask=monthlyt > -1.0e6)
     :                   / count (monthlyt > -1.0e6, dim=2)
         elsewhere
            ave_monthlyt = -1.0e6
         end where

            ! get the average annual temperature
         ampt(:) = -1.0e6
         avet = sum (ave_monthlyt, mask=ave_monthlyt > -1.0e6)
     :         / count(ave_monthlyt > -1.0e6)
            ! get the amplitude for each year
         ampt = maxval (monthlyt, dim=1)
     :        - minval (monthlyt, dim=1, mask=monthlyt > -1.0e6)
            ! now get the average amplitude
         amp = 0.0
         amp = sum (ampt, mask=ampt > -1.0e6) / count (ampt > -1.0e6)

         print*, ' TAV = ', avet, ' AMP = ', amp
         print*
         print*, ' Writing data to file'
         open (20, outfile)
         do k = 1, nrec_headers-1
            write (20, '(a)') trim (recs(k))
         end do

         call date_and_time (date_str, time_str)
         write (20, '(/a, 2(i3, a, i4, a))')
     :                     '   ! TAV and AMP inserted by "tav_amp" on '
     :                  // date_str(7:8) // '/'
     :                  // date_str(5:6) // '/'
     :                  // date_str(1:4) // ' at '
     :                  // time_str(1:2) // ':'
     :                  // time_str(3:4) // ' for period from '
     :                  , start_day, '/', start_year, ' to '
     :                  , end_day, '/', end_year, ' (ddd/yyyy)'
         write (20, '(a, f6.2, a)') ' tav = ', avet, ' (oC) '
     :      // '    ! annual average ambient temperature'
         write (20, '(a, f6.2, a/)') ' amp = ', amp, ' (oC) '
     :      // '    ! annual amplitude in mean monthly temperature'
         do k = nrec_headers, nrecs
            write (20, '(a)') trim (recs(k))
         end do
         close (20)

         print*, ' AMP and TAV variables inserted into weather file: '
     :            //outfile

      endif

      print*
      print*, ' Pause... Press Enter to exit'
      read*

      stop
      end


* ====================================================================
       character*(*) function Lower_case (char_string)
* ====================================================================
      implicit none

*+ Sub-Program Arguments
       character char_string*(*)       ! (INPUT) character string to convert

*+ Purpose
*      Convert a character string to lower case.

*+  Definition
*     This function returns a copy of "char_string" with all upper
*     case characters converted to lower case.  If any non blank
*     trailing characters are truncated in the process, a
*     warning error is flagged.  If any non blank upper case
*     characters are truncated in the process a fatal error is
*     flagged.

*+ Notes
*      This routine does not assume the ASCII character set and should
*      be platform independant.  It does assume that the characters of
*      'A' through 'Z' are sequential, as are 'a' through 'z', in the
*      character set.

*+ Local Variables
       integer   char_code             ! Code of character (ASCII on PC's)
       integer   char_index            ! Index into character string
       character Charact*1             ! Current character
       integer   Code_diff             ! Difference in codes between 'A' - 'a'
       integer   lower_char            ! Lowercase character code
       integer   string_end            ! end of string position

*- Implementation Section -----------------


      ! Calculate the difference between 'A' and 'a' and apply this difference
      ! to each character in the character string.

      Code_diff = ichar ('A') - ichar ('a')

      lower_case = char_string
      string_end = len_trim(lower_case)

      do  char_index = 1, string_end
         Charact = lower_case(char_index:char_index)
         if (Charact .ge. 'A' .and. Charact .le. 'Z') then

            ! Character is uppercase - convert to lowercase

            char_code = ichar (Charact)
            lower_char = char_code - Code_diff
            Lower_case(char_index:char_index) = char (lower_char)

         else
            ! Character is already lowercase.
         endif
      enddo

      return
      end

* ====================================================================
      integer function word_count (string)
* ====================================================================
      implicit none

*+ Sub-Program Arguments
      character string*(*)             ! string to be searched

*+ Purpose
*     count_of_real_vals number of entities in a string, separated by one or
*     more blanks
*     or commas

*+  Definition
*     "string" is a string containing zero or more words where
*     each word is separated from any previous word by one or
*     more blanks, commas or equals signs.  This function
*     returns the number of words in "string".


*+ Constant Values
      character  entity_delim*(*)      ! possible delimiters of entities
      parameter (entity_delim = ' ,')

*+ Local Variables
      logical entity_start             ! flag indicating if starting an entity
      integer indx                     ! character index
      logical on_entity_now            ! flag indicating if on an entity
      logical prev_on_entity           ! flag indicating if previous
                                       ! character was on an entity
      integer string_end               ! position of end  of string

*- Implementation Section -----------------


            ! Take each character in string in turn and check if it
            ! is a delimiter in the list.  Find start of an entity
            ! and count_of_real_vals entity.

      prev_on_entity = .false.
      word_count = 0
      string_end = len_trim(string)

      do indx = 1, string_end

         on_entity_now = index (entity_delim, string(indx:indx)).eq.0
         entity_start = on_entity_now .and. .not.prev_on_entity

         if (entity_start) then
            word_count = word_count + 1
         else
         endif

         prev_on_entity = on_entity_now

      end do

      return
      end

* ====================================================================
       integer function Find_string_in_array(String, Array, numvals)
* ====================================================================
      implicit none

*+ Sub-Program Arguments
      character String*(*)             ! (INPUT) String to find
      character Array(*)*(*)           ! (INPUT) Array of strings to search
      integer Numvals                  ! (INPUT) Number values in array

*+ Purpose
*     Find the string in the specified array.  Return index into
*     array if found or -1 otherwise.

*+  Mission Statement
*

*+ Changes
*     DPH - 8/11/94

*+ Calls

*+ Local Variables
      logical Found                    ! Found string ?
      integer Indx                     ! Index into array.

*- Implementation Section -----------------

      ! Go find the module if possible.

      Indx = 1
10    continue
      if (Indx .gt. numvals) then
         Found = .false.

      else if (Array(Indx) .eq. String) then
         Found = .true.

      else
         Indx = Indx + 1
         goto 10
      endif

      if (Found) then
         Find_string_in_array = Indx

      else
         Find_string_in_array = -1
      endif

      return
      end

*     ===========================================================
      subroutine day_of_year_to_date (dyoyr, iyr, date)
*     ===========================================================
      implicit none

*+ Sub-Program Arguments
      integer    date (3)              ! (OUTPUT) day, month, year
      integer    iyr                   ! (INPUT) year
      integer    dyoyr                 ! (INPUT) day of year

*+ Purpose
*                given the day of year number dyoyr, within year iyr,
*                convert it to calendar day nd and month mo.

*+  Mission Statement
*

*+ Changes
*       020392 jngh specified and programmed
*       270592 jngh changed year and day checking to use of
*                   simple bound checking - cr339
*       290592 jngh corrected calculation of day of month - cr354
*     DPH 19/10/94 changed call to bndchk to bound_check_integer_var

*+ Calls
      logical    leap_year             ! function

*+ Constant Values
      integer    day                   ! subscript number of day in date
      parameter (day = 1)
*
      integer    mnth                  ! subscript number of month in date
      parameter (mnth = 2)
*
*
      integer    year                  ! subscript number of year in date
      parameter (year = 3)
*
*+ Local Variables
      integer    mo                    ! month number
      integer    month (12)            ! days in month
      integer    nd                    ! day number

*+ Initial Data Values
      save      month
*
      data      month(1) / 31/
      data      month(2) / 28/
      data      month(3) / 31/
      data      month(4) / 30/
      data      month(5) / 31/
      data      month(6) / 30/
      data      month(7) / 31/
      data      month(8) / 31/
      data      month(9) / 30/
      data      month(10)/ 31/
      data      month(11)/ 30/
      data      month(12)/ 31/

*- Implementation Section -----------------

              ! check for leap year and set feb and year length accordingly

         if (leap_year (iyr)) then
            month(2) = 29

         else
            month(2) = 28

         endif


                ! calculate day and month

            mo = 1
            nd = dyoyr

100         continue
            if (nd.gt.0) then
               nd = nd - month(mo)
               mo = mo + 1
               goto 100

            else
            endif

            date(mnth) = mo - 1
            date(day) = nd + month(mo-1)
            date(year) = iyr


      return
      end

* ====================================================================
       logical function Leap_year (Year)
* ====================================================================
      implicit none

*+ Sub-Program Arguments
       integer Year                    ! (INPUT) Year

*+ Purpose
*      Returns TRUE if year YEAR is a leap year

*+ Notes
*      One earth orbit around the sun does not take an integral
*      number of days - 365 + a small part of a day.  Since the
*      Gregorian calendar year is measured as 365 days, a correction
*      for this err is made every fourth year by adding one day
*      to the length of the year.  This correction is a little too
*      much, thus in the centesimal years the correction is not made.
*      However this over corrects, so in every fourth centesimal year,
*      the correction of adding one day is made.
*
*      To summarise -
*      If the year is divisible by 4 it is a leap year, unless it is
*      a centesimal year, in which case_z it must be divisible by 400.
*      i.e.  it is a leap year if either of the conditions hold:
*         (1) the year is divisible by 4 but not by 100;
*         (2) the year is divisible by 400.

*+  Mission Statement
*

*+ Changes
*      JNGH 21/1/91 - first programmed in AUSSIM
*      DPH 15/6/92  - put into new system

*+ Calls

*+ Local Variables
      logical Y4                       ! Leap year - 4th year
      logical Y100                     ! Not leap year - century
      logical Y400                     ! Leap year - 4th century

*- Implementation Section -----------------

      ! Divisible by 4 ?

      y4 = mod (Year, 4).eq.0

      ! divisible by 100 - centesimal year?

      y100 = mod (Year, 100).eq.0

      ! divisible by 400 ?

      y400 = mod (Year,400).eq.0

      ! is the year is divisible by 4 but not by 100;
      ! or is the year is divisible by 400.

      Leap_year = (y4 .and. .not. y100) .or. y400

      return
      end

* ====================================================================
      subroutine get_a_word (string, nth, word)
* ====================================================================
      implicit none

*+ Sub-Program Arguments
      character  string*(*)            ! (INPUT) string to be searched
      integer    nth                   ! (INPUT) word number to return
      character  word*(*)              ! (OUTPUT) word found

*+ Purpose
*     returns the n'th word in a string, separated by one or more blanks,
*     commas or equals. If not found, blank is returned.

*+  Definition
*     "string" is a string containing zero or more words where
*     each word is separated from any previous word by  one or
*     more blanks, commas or equals signs.  If there is at
*     least "nth" words in "string", then the "nth"'th word will be
*     assigned to "word".  Otherwise the empty string will be
*     assigned to "word".

*+  Mission Statement
*      Let %3 be the %2'th word in %1

*+ Changes
*       050994 JNGH specified and programmed

*+ Calls

*+ Constant Values
      character  blank_string*(*)
      parameter (blank_string = ' ')
*
      character  word_delim*(*)        ! possible delimiters of wordss
      parameter (word_delim = ' ,=')

*+ Local Variables
      integer counter                  ! count_of_real_vals of words found so
                                       ! far
      integer indx                     ! character index
      logical on_word_now              ! flag indicating if on a word
      logical prev_on_word             ! flag indicating if previous
                                       ! character was on a word
      integer start_pos                ! starting position of word in string
      integer string_end               ! position of end  of string
      logical word_end                 ! flag indicating if ending a word
      logical word_start               ! flag indicating if starting a word

*- Implementation Section ----------------------------------


            ! Take each character in string in turn and check if it
            ! is a delimiter in the list.  Find start of a word
            ! and count_of_real_vals word.

      word = blank_string
      prev_on_word = .false.
      counter = 0
      start_pos = 0
      string_end = len_trim(string)

      do 1000 indx = 1, string_end

         on_word_now = index (word_delim, string(indx:indx)).eq.0
         word_start = on_word_now .and. .not.prev_on_word
         word_end = (prev_on_word .and. .not.on_word_now)
     :           .or. (on_word_now .and. indx.eq.string_end)

         if (word_start) then
            counter = counter + 1
            start_pos = indx
         endif
         if (word_end .and. counter.eq.nth) then
               ! extract nth word
            word = string(start_pos:indx)
         endif

         prev_on_word = on_word_now

1000  continue

      return
      end


