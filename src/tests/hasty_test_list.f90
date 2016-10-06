!< HASTY test list.
program hasty_test_list
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test list.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32
use hasty
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
class(*), allocatable :: a_key           !< A key.
class(*), pointer     :: a_content       !< A content.
class(*), pointer     :: another_content !< Another content.
type(list)            :: a_list          !< A list.
logical               :: test_passed(8)  !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

call list_finalize

allocate(a_key, source=3_int32)
allocate(a_content, source=12_int32)

call a_list%add_pointer(key=a_key, content=a_content)
call a_list%add_clone(key=5_int32, content=13_int32)
print '(A)', 'Keys in list:'
call a_list%print_keys

a_content => a_list%get(key=3_int32)
if (associated(a_content)) then
  select type(a_content)
  type is(integer(int32))
    test_passed(1) = a_content==12_int32
  endselect
endif
print "(A,L1)", 'a_list(3) = 12, is correct? ', test_passed(1)

a_content => a_list%get(key=5_int32)
if (associated(a_content)) then
  select type(a_content)
  type is(integer(int32))
    test_passed(2) = a_content==13_int32
  endselect
endif
print "(A,L1)", 'a_list(5) = 13, is correct? ', test_passed(2)

a_content => a_list%get(key=3_int32)
another_content => a_list%get(key=3_int32)
if (associated(a_content).and.associated(another_content)) then
  select type(a_content)
  type is(integer(int32))
    a_content = 10_int32
  endselect
  select type(another_content)
  type is(integer(int32))
    test_passed(3) = another_content==10_int32
  endselect
endif
print "(A,L1)", 'a_list(3) = 10, is correct? ', test_passed(3)

test_passed(4) = len(a_list)==2
print "(A,L1)", 'len(a_list) = 2, is correct? ', test_passed(4)

test_passed(5) = a_list%has_key(3_int32)
print "(A,L1)", 'a_list has key "3", is correct? ', test_passed(5)

test_passed(6) = .not.a_list%has_key(4_int32)
print "(A,L1)", 'a_list has not key "4", is correct? ', test_passed(6)

call a_list%remove(key=3_int32)
test_passed(7) = .not.a_list%has_key(3_int32)
print "(A,L1)", 'a_list has not key "3", is correct? ', test_passed(7)

call a_list%destroy
test_passed(8) = len(a_list)==0
print "(A,L1)", 'a_list destroyed, is correct? ', test_passed(8)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine list_finalize
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Test [[list:finalize]].
  !---------------------------------------------------------------------------------------------------------------------------------
  type(list) :: local_list !< A list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call local_list%add_clone(key=5_int32, content=13_int32)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine list_finalize
endprogram hasty_test_list