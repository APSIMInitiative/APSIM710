      subroutine wrapperDLL(FileName)
      implicit none
      ml_external wrapperDLL
      integer*1 FileName(25)
      FileName(1) = ichar('l')
      FileName(2) = ichar('i')
      FileName(3) = ichar('b')
      FileName(4) = ichar('F')
      FileName(5) = ichar('o')
      FileName(6) = ichar('r')
      FileName(7) = ichar('t')
      FileName(8) = ichar('r')
      FileName(9) = ichar('a')
      FileName(10) = ichar('n')
      FileName(11) = ichar('C')
      FileName(12) = ichar('o')
      FileName(13) = ichar('m')
      FileName(14) = ichar('p')
      FileName(15) = ichar('o')
      FileName(16) = ichar('n')
      FileName(17) = ichar('e')
      FileName(18) = ichar('n')
      FileName(19) = ichar('t')
      FileName(20) = ichar('I')
      FileName(21) = ichar('n')
      FileName(22) = ichar('t')
      FileName(23) = ichar('e')
      FileName(24) = ichar('r')
      FileName(25) = ichar('f')
      FileName(26) = ichar('a')
      FileName(27) = ichar('c')
      FileName(28) = ichar('e')
      FileName(29) = ichar('.')
      FileName(30) = ichar('s')
      FileName(31) = ichar('o')
      FileName(32) = 0
      end subroutine

      function getInstance()
      implicit none
      ml_external getInstance
      integer,pointer :: getInstance

      integer, target :: ID
      common /InstancePointers/ ID

      getInstance => ID
      end function


      subroutine getDescription(InitScript, Description)
      implicit none
      ml_external getDescription, getDescriptionInternal
      integer InitScript, Description
      call getDescriptionInternal(InitScript, Description)
      end subroutine

      subroutine getDescriptionLength(InitScript, Length)
      implicit none
      ml_external getDescriptionLength, getDescriptionLengthInternal
      integer InitScript, Length
      call getDescriptionLengthInternal(InitScript, Length)
      end subroutine
