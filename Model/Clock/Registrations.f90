module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: tick
      integer :: prepare
      integer :: process
      integer :: post
      integer :: report
      integer :: sysinit
      integer :: start
      integer :: pause
      integer :: cont
      integer :: end_run

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%tick = add_registration(eventReg, 'tick', TimeTypeDDML, '')
         id%prepare = add_registration(eventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(eventReg, 'process', nullTypeDDML, '')
         id%post = add_registration(eventReg, 'post', nullTypeDDML, '')
         id%report = add_registration(eventReg, 'report', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%start = add_registration(respondToEventReg, 'start', nullTypeDDML, '')
         id%pause = add_registration(respondToEventReg, 'pause', nullTypeDDML, '')
         id%cont = add_registration(respondToEventReg, 'cont', nullTypeDDML, '')
         id%end_run = add_registration(respondToEventReg, 'end_run', nullTypeDDML, '')
      end subroutine
end module Registrations

