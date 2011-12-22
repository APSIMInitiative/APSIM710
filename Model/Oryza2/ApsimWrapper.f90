! Wrappers for oryza to call the apsim infrastructure. 

! Some of these are stubs that do nothing, others do proper traslations.

      SUBROUTINE FatalERR (MODULE,MESSAG)
      Use infrastructure2
      IMPLICIT NONE
      CHARACTER(LEN=*) :: MODULE, MESSAG
      call Fatal_error(Err_internal, MODULE // ' ' // MESSAG)
      end subroutine

!! Hmmm. These two will create trouble. errormodule::fatal_error calls ::error; which will segfault after a few times. Same end, but not nice
      SUBROUTINE ERROR (ERRKEY,ERRNUM,FILE,LNUM)
      Use infrastructure2
      IMPLICIT      NONE
      CHARACTER*(*) ERRKEY,FILE
      INTEGER       ERRNUM,LNUM
      call Fatal_error(Err_internal, 'Oryza: '//errkey)
      end subroutine

      SUBROUTINE WARNING (numlines, key, msg)
      Use infrastructure2
      IMPLICIT      NONE
      character(len=*) msg(*)
      CHARACTER(len=*) key
      INTEGER       NUMLines, i
      do i = 1, numlines
        call WriteLine(msg(i))
      end do
      end subroutine

      ! Replacement routine for oryza. We need to supply a valid LUN as oryza will close() it later.
      SUBROUTINE RDINIT (IUNIT,IULOG,DATFIL)
      IMPLICIT NONE
      INTEGER IUNIT,IULOG
      CHARACTER*(*) DATFIL
      open (unit = iunit, file = datfil)
      end subroutine

      ! All oryza 'variables' are read from a file. We have to test if each variable is read (from xml),
      ! or "get" (from system). Further, the apsim names are mapped: "SNO3X" == "no3"

! NL = int scalar = number of sol layers
! SANDX, clayx = optional real array (0-1)
! BD = real array                    ( g/cm3  ) 
! SOC = real array  (opt) soil organic C  (kg/ha) 
! SOM = real array  (opt) soil organic matter (kg/ha) 
! SON = real array  (opt) soil organic N (kg/ha) 
! TKL = real array  (opt) soil layer thickness (m)
! SNO3X = real array  (opt)                 (kg/ha)
! SNH4X = real array  (opt)                 (kg/ha)
! SUREA = real array  (opt)                 (kg/ha)
! plowpan = real scalar (opt) depth at which plowpan layer impedes root penetration (m; -ve for missing)
! CO2 = real scalar                        (ppm)

      subroutine getApsimName (xname, apsimName) 
      implicit none
      CHARACTER*(*) XNAME
      CHARACTER*(*) apsimNAME
      apsimName = ' '
      call UPPERC(XNAME)
      if (xname .eq. 'NL') then
         apsimName = 'num_layers'
      elseif (xname .eq. 'BD') then
          apsimName = 'bd'
      elseif (xname .eq. 'SOC') then
          apsimName = 'fom_c'
      elseif (xname .eq. 'SON') then
           apsimName = 'fom_n'
      elseif (xname .eq. 'TKL') then
          apsimName = 'dlayer'
      elseif (xname .eq. 'SANDX') then
          apsimName = 'SAND'
      elseif (xname .eq. 'CLAYX') then
          apsimName = 'CLAY'
      elseif (xname .eq. 'SNO3X') then
          apsimName = 'NO3'
      elseif (xname .eq. 'SNH4X') then
          apsimName = 'NH4'
      elseif (xname .eq. 'SUREA') then
          apsimName = 'UREA'
      elseif (xname .eq. 'PLOWPAN') then
          apsimName = 'PLOWPAN'
      elseif (xname .eq. 'CO2') then
           apsimName = 'CO2'
      elseif (xname .eq. 'SBDUR') then
           apsimName = 'SBDUR'
      elseif (xname .eq. 'NH') then
           apsimName = 'NH'
      elseif (xname .eq. 'NPLH') then
           apsimName = 'NPLH'
      elseif (xname .eq. 'NPLSB') then
           apsimName = 'NPLSB'
      elseif (xname .eq. 'NPLDS') then
           apsimName = 'NPLDS'
      endif
      return
      end subroutine

      ! See if a variable exists. 
      LOGICAL FUNCTION RDINQR (XNAME)
      use Infrastructure2
      IMPLICIT NONE
      CHARACTER*(*) XNAME, apsimName*(10)
      REAL X
	  DIMENSION X(100)
      integer found, numvals
      X(:) = 0.0
      call getApsimName (xname, apsimName)
      if (apsimName .ne. ' ') then
         RDINQR = GetRealArray(apsimName   &   ! Variable Name
            , '()'           &   ! Units
	        , IsOptional     &
            , X, numvals, 100, -1.0E9, 1.0E9)
      else
         RDINQR = ReadRealArray(XNAME   &   ! Variable Name
            , '()'           &   ! Units
	        , IsOptional     &
            , X, numvals, 100, -1.0E9, 1.0E9)
      endif
      RETURN
      END

      ! Read a single real
      SUBROUTINE RDSREA (XNAME,X)
      use Infrastructure2
      use Oryza2Module
      IMPLICIT NONE
      CHARACTER*(*) XNAME
      character*(10) apsimName
      REAL X
      integer found

      found = 0
      call getApsimName (xname, apsimName)
      if (XNAME .eq. 'NL') then
         X = g%SoilProfile%num_dlayer
         found = 1
      else if (apsimName .ne. ' ') then
         if (apsimName .eq. 'SBDUR') then
           X = g%sow_SBDUR; found = 1
         elseif (apsimName .eq. 'NH') then
           X = g%sow_NH; found = 1
         elseif (apsimName .eq. 'NPLH') then
           X = g%sow_NPLH; found = 1
         elseif (apsimName .eq. 'NPLSB') then
           X = g%sow_NPLSB; found = 1
         elseif (apsimName .eq. 'NPLDS') then
           X = g%sow_NPLDS; found = 1
         else
   	       found = Get(apsimName, '()', 1, X, -1.0E9, 1.0E9 )
		 end if
      else
          found = ReadReal(XNAME &   ! Variable Name
            , '()'            &   ! Units
	    , 1     &   !isOptional
            , X, -1.0E9, 1.0E9 )
      end if
      if (found .eq. 0) then
        call FatalERR('oryza', 'Cannot read parameter '//xname)
      end if 
      end subroutine

      ! read a single INTEGER number 
      SUBROUTINE RDSINT (XNAME,X)
      use Infrastructure2
      use Oryza2Module

      IMPLICIT NONE
      CHARACTER*(*) XNAME
      character*(10) apsimName
      INTEGER X
      integer found
      call getApsimName (xname, apsimName)
      if (XNAME .eq. 'NL') then
         X = g%SoilProfile%num_dlayer
         found = 1
      else if (apsimName .ne. ' ') then
         found = Get(apsimName, '()', 1, X, -1000000, 1000000 )
      else
         found = ReadInteger(XNAME &   ! Variable Name
                            , '()'  &   ! Units
	                    , 1     &   !isOptional
                            , X, -1000000, 1000000)
      end if
      if (found .eq. 0) then
        call FatalERR('oryza', 'Cannot read parameter '//xname)
      endif 
      end subroutine

      ! read a single CHARACTER value
      SUBROUTINE RDSCHA (XNAME,X)
      use Infrastructure2
      IMPLICIT NONE
      CHARACTER*(*) XNAME,X
      integer found

      found = ReadParam(XNAME  &   ! Variable Name
            , '()'             &   ! Units
	    , 1                &   !isOptional
            , X)
      if (found .eq. 0) then
        call FatalERR('oryza', 'Cannot read parameter '//xname)
      endif 
      end subroutine

      ! Read an array of reals
      SUBROUTINE RDAREA (XNAME,X,ILDEC,IFND)
      use Infrastructure2
      IMPLICIT NONE
      INTEGER ILDEC,IFND
      REAL X
      DIMENSION X(ILDEC) 
      CHARACTER*(*) XNAME
      character *(10) apsimName
      logical found

      call getApsimName (xname, apsimName)
      found = .false.
      if (apsimName .ne. ' ') then
          found = Get(apsimName, '()', 0, X, IFND, ILDEC, -1.0E9, 1.0E9 )
		  if (xname .eq. 'TKL') then  ! TKL is in m, dlayer is in mm
		      x = x / 1000.0
		  endif
      else
          found =  readParam(XNAME   &   ! Variable Name
            , '()'            &   ! Units
	    , NotOptional     &   !isOptional
            , X               &   ! Variable
	    , IFND            &   ! number returned
	    , ILDEC           &   ! Size of array
            , -1.0E9, 1.0E9)       !lower, upper
      endif
      if (.not. found) then
        call FatalERR('oryza', 'Cannot read parameter '//xname)
      end if 
      end subroutine

      ! Dont know what this one does
      SUBROUTINE RDFREA (XNAME,X,ILDEC,IVALS)
      Use infrastructure2
      IMPLICIT NONE
      INTEGER ILDEC,IVALS
      REAL X
      DIMENSION X(ILDEC)
      CHARACTER*(*) XNAME
      call fatal_error(err_internal,'RDFREA called')
      end subroutine

      ! Translate lower case characters
      SUBROUTINE UPPERC (STRING)
      use Infrastructure2
      IMPLICIT NONE
      CHARACTER(LEN=*) :: STRING
       integer   char_code             ! Code of character (ASCII on PC's)
       integer   char_index            ! Index into character string
       character Charact*1             ! Current character
       integer   Code_diff             ! Difference in codes between 'A' - 'a'
       integer   lower_char            ! Lowercase character code
       integer   string_end            ! end of string position

   !- Implementation Section ----------------------------------

      ! Calculate the difference between 'A' and 'a' and apply this difference
      ! to each character in the character string.

      Code_diff = ichar ('a') - ichar ('A')

      string_end = len_trim(STRING)

      do 10 char_index = 1, string_end
         Charact = STRING(char_index:char_index)
         if (Charact .ge. 'a' .and. Charact .le. 'z') then

            ! Character is lowercase - convert to uppercase

            char_code = ichar (Charact)
            lower_char = char_code - Code_diff
            STRING(char_index:char_index) = char (lower_char)

         else
            ! Character is already lowercase.
         endif
10    continue
      end subroutine

      ! Linear interpolation		 
      REAL FUNCTION LINT2 (TABNAM,TABLE,ILTAB,X)
      Use infrastructure2
      IMPLICIT NONE
      CHARACTER*(*) TABNAM
      INTEGER ILTAB
      REAL TABLE(ILTAB), X
      INTEGER I1, IUP, IL, ILEN
      REAL SLOPE, TINY
      PARAMETER (TINY=1.E-7)
      LOGICAL ERR
      character(256) errmsg
      SAVE
      ERR  = .FALSE.
      IF (MOD(ILTAB,2).NE.0 .OR. ILTAB.LE.2) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (errmsg,*)&
        ' Number of elements in interpolation table: ',TABNAM(1:IL),&
        ' not correct !'
      ERR = .TRUE.
      ELSE
      IUP = 0
      DO 10 I1=3,ILTAB,2
      IF (TABLE(I1).LE.TABLE(I1-2)) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (errmsg,*)&
        ' X-coordinates in interpolation table: ',TABNAM(1:IL),&
        ' not in ascending order at point',I1
      ERR = .TRUE.
      END IF
      IF (IUP.EQ.0 .AND. TABLE(I1).GE.X) IUP = I1
10    CONTINUE
      END IF
      IF (.NOT.ERR .AND. X.LT.TABLE(1)) THEN
      IUP = 3
      IF ((TABLE(1)-X) .GT. ABS(X)*TINY) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (errmsg,*)&
        ' WARNING in LINT2: X-value below defined region at X=',X,&
        ' in interpolation table: ',TABNAM(1:IL)
      END IF
      ELSE IF (.NOT.ERR .AND. X.GT.TABLE(ILTAB-1)) THEN
      IUP = ILTAB-1
      IF ((X-TABLE(ILTAB-1)) .GT. ABS(X)*TINY) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (*,'(A,G13.5,/,2A)')&
        ' WARNING in LINT2: X-value above defined region at X=',X,&
        ' in interpolation table: ',TABNAM(1:IL)
      END IF
      END IF
      IF (ERR) CALL FatalERR ('Oryza2/LINT2',errmsg)
      SLOPE = (TABLE(IUP+1)-TABLE(IUP-1))/(TABLE(IUP)-TABLE(IUP-2))
      LINT2 = TABLE(IUP-1)+(X-TABLE(IUP-2))*SLOPE
      RETURN
      END

      !accessory to prevent divide by zero
      REAL FUNCTION NOTNUL (X)
      IMPLICIT NONE
      REAL X
      IF (X.NE.0.) THEN
         NOTNUL = X
      ELSE
         NOTNUL = 1.
      END IF
      RETURN
      END

      ! integrate a state
      REAL FUNCTION INTGRL (STATE, RATE, DELT)
      IMPLICIT NONE
      REAL STATE, RATE, DELT
      SAVE
      INTGRL = STATE + RATE * DELT
      RETURN
      END

      ! Oryza predicted:observed helper. Unimplmented
      LOGICAL FUNCTION INQOBS (FILEIN,VARNAM)
      IMPLICIT NONE
      CHARACTER (*) FILEIN, VARNAM
      inqobs = .false.
	  return
	  end
	  
      ! Oryza predicted:observed helper. Unimplmented
      REAL FUNCTION GETOBS (FILEIN,VARNAM)
      IMPLICIT NONE
      CHARACTER (*) FILEIN, VARNAM
      getobs = 0.0
	  return
	  end

      ! Oryza predicted:observed helper. Unimplmented
      SUBROUTINE OPSTOR (VARNAM,VARVAL)
      IMPLICIT NONE
      CHARACTER (*) VARNAM
      REAL          VARVAL
	  return
	  end

      ! Limit a variable to min/max values
      REAL FUNCTION LIMIT (MIN,MAX,X)
      IMPLICIT NONE
      REAL MIN,MAX,X
      IF (X.LT.MIN) THEN
      LIMIT = MIN
      ELSE IF (X.LE.MAX) THEN
      LIMIT = X
      ELSE
      LIMIT = MAX
      END IF
      RETURN
      END

      ! ??	     
      SUBROUTINE OUTCOM (STR)
      Use infrastructure2
      IMPLICIT NONE
      CHARACTER*(*) STR
      call WriteLine('oryza2 '// str)
      RETURN
      END

      ! fortran style switch
      REAL FUNCTION INSW (X1,X2,X3)
      IMPLICIT NONE
      REAL X1,X2,X3
      IF (X1.LT.0.) THEN
      INSW = X2
      ELSE
      INSW = X3
      END IF
      RETURN
      END

      REAL FUNCTION INTGR2 (STATE,RATE,DELT,FILEIN,STATNM)
!     Integrates similar to the INTGRL function when no observations
!     and/or forcing was given. When forcing was enabled, the
!     estimated observation is returned

!     STATE  - Old value of state                                    I
!     RATE   - Rate as calculated by model                           I
!     DELT   - Time step to be applied when no forcing takes place   I
!     FILEIN - Name of datafile in which STATNM is possibly forced   I
!     STATNM - Name of state variable                                I

!     INTGR2 - Function result, integrated or forced                 I

      IMPLICIT NONE
!     formal parameters
      REAL          STATE, RATE, DELT
      CHARACTER (*) STATNM, FILEIN

      INTGR2 = STATE+RATE*DELT

      RETURN
      END

      ! Replacement routine for oryza. We need to supply a valid LUN as oryza will close() it later.
      SUBROUTINE GETLUN(FileVarName, LUN)
      IMPLICIT NONE

      CHARACTER*(*), INTENT(IN) :: FileVarName
      INTEGER, INTENT(OUT) :: LUN
      select case (FileVarName) 
        case('ORYZA1'); LUN = 110
        case('ORYZA2'); LUN = 111
        case('OUTPN');  LUN = 112
        case('OUTG');   LUN = 113
        case DEFAULT;   LUN = 0
      end select
      end subroutine
	  
      SUBROUTINE RDDTMP (IUNIT)
      IMPLICIT NONE
      INTEGER IUNIT
      end subroutine

      ! Initialise the oryza observations/ forcing system
      subroutine obsini()
      implicit none
      END SUBROUTINE

!=====================================================================

!=======================================================================
SUBROUTINE SET_ZERO_PV

	  USE PUBLIC_MODULE
	
		pv%CRUN=0;				pv%PROOT_NUTRIENT=.FALSE.
		pv%pond_active ='NO';	pv%PYear=0;pv%Pdoy=0
		pv%Pdae=0;				pv%Pdat=0
		pv%Pno3=0.0;			pv%Pnh4=0.0
		pv%Purea=0.0;			pv%pond_no3=0.0
		pv%pond_nh4=0.0;		pv%pond_urea=0.0
		pv%Psoc=0.0;			pv%Pson=0.0
		pv%pph=7.0;				pv%PFOM_type=0.0
		pv%PFOM_C=0.0;			pv%PFOM_N=0.0
		pv%Pnl=0;				pv%Pdlayer=0.0
		pv%Pbd=0.0;				pv%Psand=0.0
		pv%Pclay=0.0;			pv%Pkst=0.0
		pv%Pwcst=0.0;			pv%Pwcfc=0.0
		pv%Pwcwp=0.0;			pv%Pwcad=0.0
		pv%PplowDepth=0.0;		pv%psoiltx=0.0
		pv%Pwl0=0.0;			pv%Pswc=0.0
		pv%Pwflux=0.0;			pv%PTRWL=0.0
		pv%Prunoff=0.0;			pv%Pdrain=0.0
		pv%Pirrig=0.0;			pv%Plai=0.001
		pv%PResNum=0;			pv%PResName=''
		pv%PResType='';			pv%PResC=0.0
		pv%PResN=0.0;			pv%dlt_res_c_biom=0.0
		pv%dlt_res_c_hum=0.0;	pv%ProotD=0.0
		pv%ProotDen=0.0;		pv%PSROOTL=0.0
		pv%PRMINT=0.0;			pv%PROPTT=0.0
		pv%PRTBS=0.0;			pv%PRCNL=0.0
		pv%PMAXD=0.0;			pv%PSODT=0.0
		pv%PmaxT=0.0;			pv%PminT=0.0
		pv%PRad=0.0;			pv%PRain=0.0
		pv%PPressur=0.0;		pv%Pwind=0.0
		pv%PETp=0.0;			pv%PETa=0.0
		pv%Pevap=0.0;			pv%Ptrans=0.0
		pv%PDt=0.0;				pv%PRdn=0.0;		
	  
END SUBROUTINE SET_ZERO_PV

