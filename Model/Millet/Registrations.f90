module Registrations
   use infrastructure
   use DataTypes
   type IDsType
      sequence
      integer :: incorp_fom
      integer :: biomass_removed
      integer :: sowing
      integer :: harvesting
      integer :: externalmassflow
      integer :: create
      integer :: sysinit
      integer :: sow
      integer :: harvest
      integer :: end_crop
      integer :: initiate_crop
      integer :: kill_crop
      integer :: stop_growth
      integer :: tick
      integer :: newmet
      integer :: prepare
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         type(IDsType) :: id

         id%incorp_fom = add_registration(eventReg, 'incorpfom', FomLayerTypeDDML, '')
         id%biomass_removed = add_registration(eventReg, 'BiomassRemoved', BiomassRemovedTypeDDML, '')
         id%sowing = add_registration(eventReg, 'sowing', nullTypeDDML, '')
         id%harvesting = add_registration(eventReg, 'harvesting', nullTypeDDML, '')
         id%externalmassflow = add_registration(eventReg, 'externalmassflow', ExternalMassFlowTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%sow = add_registration(respondToEventReg, 'sow', sowTypeDDML, '')
         id%harvest = add_registration(respondToEventReg, 'harvest', nullTypeDDML, '')
         id%end_crop = add_registration(respondToEventReg, 'end_crop', nullTypeDDML, '')
         id%initiate_crop = add_registration(respondToEventReg, 'initiate_crop', nullTypeDDML, '')
         id%kill_crop = add_registration(respondToEventReg, 'kill_crop', nullTypeDDML, '')
         id%stop_growth = add_registration(respondToEventReg, 'stop_growth', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
	  
! ====================================================================
! This routine registers variables needec by sysbal
! ====================================================================
      subroutine doSysbalRegistrations()
      integer :: id
      character DDML*128

      DDML = '<type kind="single" />'

         ! WATER
      id = add_registration(respondToGetReg, 'ep', floatTypeDDML, '') ! 'mm')

         ! P
!      id = add_registration(respondToGetReg, 'p_green', DDML , 'g/m^2', 'P in green')
!      id = add_registration(respondToGetReg, 'p_senesced', DDML, 'g/m^2', 'P in senesced')
!      id = add_registration(respondToGetReg, 'p_dead', DDML, 'g/m^2', 'P in dead')

         ! N
      id = add_registration(respondToGetReg, 'n_green', DDML, '') ! 'g/m^2')
      id = add_registration(respondToGetReg, 'n_senesced', DDML, '') ! 'g/m^2')
      id = add_registration(respondToGetReg, 'n_dead', DDML, '') ! 'g/m^2')

         ! DM
      id = add_registration(respondToGetReg, 'dm_green', DDML, '') ! 'g/m^2')
      id = add_registration(respondToGetReg, 'dm_senesced', DDML, '') ! 'g/m^2')
      id = add_registration(respondToGetReg, 'dm_dead', DDML, '') ! 'g/m^2')
      id = add_registration(respondToGetReg, 'dlt_dm_green', DDML, '') ! 'g/m^2'

      return
      end subroutine
end module Registrations

