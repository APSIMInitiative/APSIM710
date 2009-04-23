      subroutine wrapperDLL(FileName)
      implicit none
      ml_external wrapperDLL
      integer*1 FileName(23)
      FileName(1) = ichar('C')
      FileName(2) = ichar('o')
      FileName(3) = ichar('m')
      FileName(4) = ichar('p')
      FileName(5) = ichar('o')
      FileName(6) = ichar('n')
      FileName(7) = ichar('e')
      FileName(8) = ichar('n')
      FileName(9) = ichar('t')
      FileName(10) = ichar('I')
      FileName(11) = ichar('n')
      FileName(12) = ichar('t')
      FileName(13) = ichar('e')
      FileName(14) = ichar('r')
      FileName(15) = ichar('f')
      FileName(16) = ichar('a')
      FileName(17) = ichar('c')
      FileName(18) = ichar('e')
      FileName(19) = ichar('.')
      FileName(20) = ichar('d')
      FileName(21) = ichar('l')
      FileName(22) = ichar('l')
      FileName(23) = 0
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
