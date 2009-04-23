module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: externalmassflow
      integer :: create
      integer :: sysinit
      integer :: apply
      integer :: fertilize
      integer :: tick
      integer :: new_profile
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

!         id%externalmassflow = add_registration(eventReg, 'externalmassflow', ExternalMassFlowTypeDDML, '')
!         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
!         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
!         id%apply = add_registration(respondToEventReg, 'apply', nullTypeDDML, '')
!         id%fertilize = add_registration(respondToEventReg, 'fertilize', nullTypeDDML, '')
!         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
!         id%new_profile = add_registration(respondToEventReg, 'new_profile', NewProfileTypeDDML, '')
!         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
end module Registrations

