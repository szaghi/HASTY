!< HASTY test hash table homogeneous.
program hasty_test_hash_table_homo
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test hash table homogeneous.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32
use hasty
use tester
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(tester_t)    :: hasty_tester !< Tests handler.
class(*), pointer :: a_content    !< A content.
type(hash_table)  :: a_table      !< A table.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
call hasty_tester%init

call a_table%initialize(homogeneous=.true.)

call hasty_tester%assert_equal(a_table%is_homogeneous(), .true.)

call a_table%add_clone(key=5_int32, content=13_int32)
a_content => a_table%get_pointer(key=5_int32)
if (associated(a_content)) then
  select type(a_content)
  type is(integer(int32))
    call hasty_tester%assert_equal(a_content, 13_int32)
  endselect
endif

call a_table%destroy
call a_table%initialize(homogeneous=.true.)

a_content => null()
allocate(a_content, source=16_int32)
call a_table%add_pointer(key=3_int32, content=a_content)
a_content => a_table%get_pointer(key=3_int32)
if (associated(a_content)) then
  select type(a_content)
  type is(integer(int32))
    call hasty_tester%assert_equal(a_content, 16_int32)
  endselect
endif

call hasty_tester%print
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram hasty_test_hash_table_homo
