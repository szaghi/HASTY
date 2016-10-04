!< HASTY test basic.
program hasty_test_basic
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test basic.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32
use hasty
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
class(*), allocatable :: a_key             !< A key.
class(*), pointer     :: a_content         !< A content.
class(*), pointer     :: another_content   !< Another content.
type(list)            :: a_list            !< A list.
logical               :: test_passed(4)    !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

allocate(a_key, source=3_int32)
allocate(a_content, source=12_int32)

! the content has not an actual container, put content directly into container
call a_list%add(key=a_key, container=a_content)
call a_list%add_clone(key=5_int32, container=13_int32)

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

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram hasty_test_basic

