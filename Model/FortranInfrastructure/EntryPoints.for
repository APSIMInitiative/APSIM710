      subroutine wrapperDLL(FileName)
      implicit none
      ml_external wrapperDLL
!STDCALL(wrapperDLL)
      integer*1 FileName(30)
      FileName(1) = ichar('F')
      FileName(2) = ichar('o')
      FileName(3) = ichar('r')
      FileName(4) = ichar('t')
      FileName(5) = ichar('r')
      FileName(6) = ichar('a')
      FileName(7) = ichar('n')
      FileName(8) = ichar('C')
      FileName(9) = ichar('o')
      FileName(10) = ichar('m')
      FileName(11) = ichar('p')
      FileName(12) = ichar('o')
      FileName(13) = ichar('n')
      FileName(14) = ichar('e')
      FileName(15) = ichar('n')
      FileName(16) = ichar('t')
      FileName(17) = ichar('I')
      FileName(18) = ichar('n')
      FileName(19) = ichar('t')
      FileName(20) = ichar('e')
      FileName(21) = ichar('r')
      FileName(22) = ichar('f')
      FileName(23) = ichar('a')
      FileName(24) = ichar('c')
      FileName(25) = ichar('e')
      FileName(26) = ichar('.')
      FileName(27) = ichar('d')
      FileName(28) = ichar('l')
      FileName(29) = ichar('l')
      FileName(30) = 0
      end subroutine

      function getInstance()
      implicit none
      ml_external getInstance
!STDCALL(getInstance)
      integer,pointer :: getInstance

      integer, target :: ID
      common /InstancePointers/ ID

      getInstance => ID
      end function


      subroutine getDescription(InitScript, Description)
      use ComponentInterfaceModule
      implicit none
      ml_external getDescription, getDescriptionInternal
!STDCALL(getDescription)
      integer InitScript, Description
      call getDescriptionInternal(InitScript, Description)
      end subroutine

      subroutine getDescriptionLength(InitScript, Length)
      use ComponentInterfaceModule
      implicit none
      ml_external getDescriptionLength, getDescriptionLengthInternal
!STDCALL(getDescriptionLength)
      integer InitScript, Length
      call getDescriptionLengthInternal(InitScript, Length)
      end subroutine

      subroutine Dispatch (Action, Data)
      implicit none
      ml_external Main, Dispatch
!STDCALL(Dispatch)
*+  Sub-Program Arguments
      character Action*(*)            ! Message action to perform
      character Data*(*)              ! Message data
  
      call Main(Action, Data)  

      return
      end subroutine
