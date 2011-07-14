module DOTNETModule
   implicit none

   interface
      function CreateClass(DllFileName, ClassName)
      ml_external CreateClass
!STDCALL(CreateClass)
      character (len=*), intent(in) :: DllFileName
      character (len=*), intent(in) :: ClassName
      integer                       :: CreateClass
      end function CreateClass 

!      subroutine CallMethod(ClassHandle, MethodName, tick)
!      ml_external CallMethod
!      integer,           intent(in) :: ClassHandle
!      character (len=*), intent(in) :: MethodName
!      type(timeType) :: tick
!      end subroutine CallMethod
      
   end interface

end module DOTNETModule
