!< HASTY test hash table homogeneous.
program hasty_test_hash_table_homo
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test hash table homogeneous.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32
use hasty
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
class(*), pointer :: a_content      !< A content.
type(hash_table)  :: a_table        !< A table.
logical           :: test_passed(3) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

call a_table%initialize(homogeneous=.true.)

test_passed(1) = a_table%is_homogeneous()
print "(A,L1)", 'a_table is homogeneous, is correct? ', test_passed(1)

call a_table%add_clone(key=5_int32, content=13_int32)
a_content => a_table%get_pointer(key=5_int32)
if (associated(a_content)) then
  select type(a_content)
  type is(integer(int32))
    test_passed(2) = a_content==13_int32
  endselect
endif
print "(A,L1)", 'a_table(5) = 13, is correct? ', test_passed(2)

call a_table%destroy
call a_table%initialize(homogeneous=.true.)

a_content => null()
allocate(a_content, source=16_int32)
call a_table%add_pointer(key=3_int32, content=a_content)
a_content => a_table%get_pointer(key=3_int32)
if (associated(a_content)) then
  select type(a_content)
  type is(integer(int32))
    test_passed(3) = a_content==16_int32
  endselect
endif
print "(A,L1)", 'a_table(3) = 16, is correct? ', test_passed(3)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram hasty_test_hash_table_homo
