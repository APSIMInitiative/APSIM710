module PublishEventsModule

   contains

   subroutine ProcessEvent(ModuleName, EventName, DataString)
   ! ---------------------------------------------------------------
   ! Process the action line passed in.
   ! ---------------------------------------------------------------
   use ConstantsModule
   use ComponentInterfaceModule
   implicit none

   character ModuleName*(*)        ! (INPUT) Name of module to send event to.
   character EventName*(*)         ! (INPUT) Name of event to process
   character DataString*(*)        ! (INPUT) Should be blank or have a plants_kill_fraction
   logical ok
   integer ModuleNameID
   character QualifiedEventName*(500)

   if (ModuleName == All_active_modules .or. ModuleName == 'publish') then
      ModuleName = 'publish'
      QualifiedEventName = EventName
   else
      QualifiedEventName = trim(ModuleName) // '.' // EventName
   endif

   if (EventName == 'kill_crop') then
      call PublishKillCrop(QualifiedEventName, DataString)
   elseif (EventName == 'incorpfom') then
      call PublishIncorpFOM(QualifiedEventName, DataString)
   elseif (EventName == 'apply2') then
      call PublishApply2(QualifiedEventName, DataString)
   elseif (EventName == 'sow2' .or. EventName == 'prune') then
      ! Eventually all manager events are to use this.
      call PublishEvent(QualifiedEventName, DataString)
   else
      ! some other non protocol event - use the old postbox method.
      if (ModuleName == 'publish') then
         ModuleNameID = -1
      else
         ok = component_name_to_id(ModuleName, ModuleNameID);
      endif

      call New_postbox ()
      call StoreEventDataInPostBox(DataString)
      call Event_send (ModuleNameID, EventName)
      call Delete_postbox ()
   endif
   end subroutine

   subroutine SplitEventLine(DataString, KeyName, KeyUnits, KeyValues, NumValues)
   ! ---------------------------------------------------------------
   ! Return the next keyword = keyvalues (units) line, split into
   ! it's components.
   !    On Entry if DataString =
   !       Amount=11 22, N=33 44, P=55 55x
   !    On Exit:
   !       DataString = N=33 44, P=55 55x
   !       KeyName= Amount
   !       KeyUnits =
   !       KeyValues = 11, 22
   !       NumValues = 2
   ! ---------------------------------------------------------------
   use ErrorModule
   use StringModule
   use DataStrModule

   implicit none

   character DataString*(*)        ! (INPUT & OUTPUT) Data string to store into postbox.
   character KeyName*(*)           ! (OUTPUT) Keyname passed back.
   character KeyUnits*(*)          ! (OUTPUT) units passed back.
   character KeyValues(*)*(*)      ! (OUTPUT) Split values to pass back.
   integer NumValues               ! (OUTPUT) Number of values returned
   character KeyValue*500

   ! Loop through each variable on data string and store in postbox.
   call Get_next_variable (DataString, KeyName, KeyValue)

   if (KeyValue <> ' ') then
      ! Found a variable all right.  Extract units and store variable.

      call Split_off_units(KeyValue, KeyUnits)
      NumValues = 1

      ! Loop through all values and store in KeyValues.
      call Split_line_with_quotes(KeyValue, KeyValues(NumValues), KeyValue, ' ')
      do while (KeyValue <> ' ')
         NumValues = NumValues + 1
         call Split_line_with_quotes(KeyValue, KeyValues(NumValues), KeyValue, ' ')
      enddo
   else
      NumValues = 0
   endif
   end subroutine

   subroutine StoreEventDataInPostBox(DataString)
   ! ---------------------------------------------------------------
   ! Break the datastring passed in into words and store in a
   ! postbox - used by old style APSIM events.
   ! ---------------------------------------------------------------
   use StringModule
   use DataStrModule
   use ComponentInterfaceModule

   implicit none

   character DataString*(*)        ! (INPUT & OUTPUT) Data string to store into postbox.
   character KeyName*(500)
   character KeyUnits*(500)
   character KeyValues(500)*500
   integer NumValues

   ! Loop through each variable on data string and store in postbox.
   call SplitEventLine(DataString, KeyName, KeyUnits, KeyValues, NumValues)

   do while (NumValues > 0)
      if (NumValues > 1) then
         call Post_char_array (KeyName, KeyUnits, KeyValues, NumValues)
      else if (NumValues == 1) then
         call Post_char_var(KeyName, KeyUnits, KeyValues(1))
      endif
      call SplitEventLine(DataString, KeyName, KeyUnits, KeyValues, NumValues)
   end do
   end subroutine

   subroutine PublishKillCrop(QualifiedEventName, DataString)
   ! ---------------------------------------------------------------
   ! Publishes a killCrop event using the data on the specified
   ! DataString
   ! ---------------------------------------------------------------

   use ConstantsModule
   use ErrorModule
   use StringModule
   use DataStrModule
   use DataTypes
   use ComponentInterfaceModule
   use DataTypesInterface

   implicit none

   character DataString*(*)          ! (INPUT) Should be blank or have a plants_kill_fraction
   character QualifiedEventName*(*)  ! (INPUT) Fully qualified event name (e.g. wheat.kill_crop)
   character KeyName*(500)
   character KeyValues(500)*(500)
   character KeyUnits*(500)
   integer NumValues
   type(KillCropType) :: Kill
   integer KillCropID

   call SplitEventLine(DataString, KeyName, KeyUnits, KeyValues, NumValues)

   if (NumValues == 1 .and. strings_equal(KeyName, 'plants_kill_fraction')) then
      Kill%KillFraction = StringToReal(KeyValues(1))
   else
      Kill%KillFraction = 1.0
   endif
   KillCropID = add_registration(eventReg, QualifiedEventName, KillCropTypeDDML, blank)
   call publish_KillCrop(KillCropID, Kill)

   end subroutine

   subroutine PublishIncorpFOM(QualifiedEventName, DataString)
   ! ---------------------------------------------------------------
   ! Publishes an IncorpFOM event using the data on the specified
   ! DataString which should look like:
   !    Type=wheat, Amount=xx xx, N=xx xx, P=xx xx, CNR=xx xx, LabileP=xx xx
   ! ---------------------------------------------------------------

   use ConstantsModule
   use ErrorModule
   use StringModule
   use DataStrModule
   use DataTypes
   use ComponentInterfaceModule
   use DataTypesInterface

   implicit none

   character DataString*(*)          ! (INPUT) Should be blank or have a plants_kill_fraction
   character QualifiedEventName*(*)  ! (INPUT) Fully qualified event name (e.g. wheat.kill_crop)
   character KeyName*(500)
   character KeyValues(500)*(500)
   character KeyUnits*500
   integer NumValues
   type(FOMLayerType) :: IncorpFOM
   real Values(500)
   integer IncorpFOMID
   integer i

   call SplitEventLine(DataString, KeyName, KeyUnits, KeyValues, NumValues)

   do while (NumValues > 0)
      if (strings_equal(KeyName, 'type')) then
         IncorpFOM%Type = KeyValues(1)

      else
         if (IncorpFOM%num_layer == 0) then
            IncorpFOM%num_layer = NumValues
            IncorpFOM%Type = ' '
            do i = 1, IncorpFOM%num_layer
               IncorpFOM%layer(i)%FOM%Amount = 0
               IncorpFOM%layer(i)%FOM%C = 0
               IncorpFOM%layer(i)%FOM%N = 0
               IncorpFOM%layer(i)%FOM%P = 0
               IncorpFOM%layer(i)%FOM%AshAlk = 0
               IncorpFOM%layer(i)%CNR = 0
               IncorpFOM%layer(i)%LabileP = 0
            end do
         endif
         if (NumValues <> IncorpFOM%num_layer) then
            call fatal_error(1, 'Invalid number of values on line: ' // trim(DataString))
         endif

         do i = 1, NumValues
            if (strings_equal(KeyName, 'amount')) then
               IncorpFOM%Layer(i)%Fom%Amount = StringToReal(KeyValues(i))
            elseif (strings_equal(KeyName, 'N')) then
               IncorpFOM%Layer(i)%Fom%N = StringToReal(KeyValues(i))
            elseif (strings_equal(KeyName, 'P')) then
               IncorpFOM%Layer(i)%Fom%P = StringToReal(KeyValues(i))
            elseif (strings_equal(KeyName, 'CNR')) then
               IncorpFOM%Layer(i)%CNR = StringToReal(KeyValues(i))
            elseif (strings_equal(KeyName, 'LabileP')) then
               IncorpFOM%Layer(i)%LabileP = StringToReal(KeyValues(i))
            endif
         enddo
      endif

      call SplitEventLine(DataString, KeyName, KeyUnits, KeyValues, NumValues)
   end do
   IncorpFOMID = add_registration(eventReg, QualifiedEventName, FOMLayerTypeDDML, blank)
   call publish_FOMLayer(IncorpFOMID, IncorpFOM)

   end subroutine

   subroutine PublishApply2(QualifiedEventName, DataString)
   ! ---------------------------------------------------------------
   ! Publishes an apply2 event to irrigate using the data on the specified
   ! DataString
   ! ---------------------------------------------------------------

   use ConstantsModule
   use ErrorModule
   use StringModule
   use DataStrModule
   use DataTypes
   use ComponentInterfaceModule
   use DataTypesInterface

   implicit none

   character DataString*(*)          ! (INPUT) Should be blank or have a plants_kill_fraction
   character QualifiedEventName*(*)  ! (INPUT) Fully qualified event name (e.g. wheat.kill_crop)
   character KeyName*(500)
   character KeyValues(500)*(500)
   character KeyUnits*(500)
   integer NumValues
   type(IrrigationApplicationType) :: Irrigation
   integer IrrigationID

   call SplitEventLine(DataString, KeyName, KeyUnits, KeyValues, NumValues)

   if (NumValues == 1 .and. strings_equal(KeyName, 'amount')) then
      Irrigation%Amount = StringToReal(KeyValues(1))
      IrrigationID = add_registration(eventReg, QualifiedEventName, IrrigationApplicationTypeDDML, blank)
      call publish_IrrigationApplication(IrrigationID, Irrigation)
   endif
   end subroutine
   
   subroutine PublishEvent(QualifiedEventName, DataString)
   ! ---------------------------------------------------------------
   ! Publishes a general event as a string
   ! ---------------------------------------------------------------
   use DataTypes
   use ComponentInterfaceModule
   use dataTypesInterface
   implicit none

   character QualifiedEventName*(*)  ! (INPUT) Fully qualified event name (e.g. wheat.kill_crop)
   character DataString*(*)          ! (INPUT) Should be blank or have a plants_kill_fraction

   integer EventID;
   type (ManagerEventType) :: EventData
   integer :: NumKeys

   EventData%num_Key = 0
   NumKeys = 1
   call SplitEventLine(DataString,  &
                       EventData%Key(NumKeys)%Name, &
                       EventData%Key(NumKeys)%Units, &
                       EventData%Key(NumKeys)%Values, &
                       EventData%Key(NumKeys)%Num_Values)
   do while (EventData%Key(NumKeys)%Num_Values > 0 .and. DataString .ne. ' ')
      NumKeys = NumKeys + 1
      call SplitEventLine(DataString,  &
                          EventData%Key(NumKeys)%Name, &
                          EventData%Key(NumKeys)%Units, &
                          EventData%Key(NumKeys)%Values, &
                          EventData%Key(NumKeys)%Num_Values)
   enddo
   EventData%num_Key = NumKeys

   EventID = add_registration(eventReg, QualifiedEventName, ManagerEventTypeDDML, ' ')
   call publish_ManagerEvent(EventID, EventData)


   end subroutine

end module PublishEventsModule

