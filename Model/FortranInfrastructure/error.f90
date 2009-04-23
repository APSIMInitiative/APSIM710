module ErrorModule

   interface
      subroutine push_routine(name)
      ml_external push_routine
      character(len=*), intent(in) :: name
      end subroutine

      subroutine pop_routine(name)
      ml_external pop_routine
      character(len=*), intent(in) :: name
      end subroutine

      subroutine Warning(msg)
      ml_external Warning
      character(len=*), intent(in) :: msg
      end subroutine
      subroutine Fatal(msg)
      ml_external Fatal
      character(len=*), intent(in) :: msg
      end subroutine
   end interface

   contains

      subroutine warning_Error (dummy,msg)
      use ComponentInterfaceModule
      integer dummy
      character*(*) msg
      call Warning(msg)
      return
      end subroutine
      subroutine fatal_Error (dummy,msg)
      use ComponentInterfaceModule
      integer dummy
      character*(*) msg
      call Fatal(msg)
      return
      end subroutine

end module errorModule

