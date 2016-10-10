!< HASTY test hash table homogeneous key failure.
program hasty_test_hash_table_homokey_failure
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test hash table homogeneous key failure.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32
use hasty
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(hash_table) :: a_table        !< A table.
logical          :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.
print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)

call a_table%initialize(homogeneous=.true., typeguard_key='a string', typeguard_content=1_int32)
print "(A)", 'An error will be raised (if all go rigth)'
call a_table%add_clone(key=5_int32, content=13_int32)
test_passed(1) = len(a_table)==1
print "(A,L1)", 'len(a_table) = 1, is correct? ', test_passed(1)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram hasty_test_hash_table_homokey_failure
