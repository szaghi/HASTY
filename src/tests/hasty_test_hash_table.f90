!< HASTY test hash table.
program hasty_test_hash_table
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test hash table.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32
use hasty
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
! class(*), allocatable :: a_key          !< A key.
class(*), pointer     :: a_content      !< A content.
type(hash_table)      :: a_table        !< A table.
logical               :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

call a_table%add_clone(key=5_int32, content=13_int32)
a_content => a_table%get(key=5_int32)
if (associated(a_content)) then
  select type(a_content)
  type is(integer(int32))
    test_passed(1) = a_content==13_int32
  endselect
endif
print "(A,L1)", 'a_list(5) = 13, is correct? ', test_passed(1)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram hasty_test_hash_table
