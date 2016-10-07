!< HASTY test dictionary.
program hasty_test_dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test dictionary.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32
use hasty
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
class(*), allocatable :: a_key           !< A key.
class(*), pointer     :: a_content       !< A content.
class(*), pointer     :: another_content !< Another content.
type(dictionary)      :: a_dictionary    !< A dictionary.
integer(int32)        :: max_content     !< Maximum content value.
logical               :: test_passed(10) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

call dictionary_finalize

allocate(a_key, source=3_int32)
allocate(a_content, source=12_int32)

call a_dictionary%add_pointer(key=a_key, content=a_content)
call a_dictionary%add_clone(key=5_int32, content=13_int32)
print '(A)', 'Keys in dictionary:'
call a_dictionary%print_keys

print "(A)", 'Explicit loop over all nodes (keys)'
do while(a_dictionary%loop_key(key=a_key))
  if (allocated(a_key)) then
    select type(a_key)
    type is(integer(int32))
      print "(A,I3)", 'key ', a_key
    endselect
  endif
enddo

print "(A)", 'Repeating the loop to check saved values (keys)'
do while(a_dictionary%loop_key(key=a_key))
  if (allocated(a_key)) then
    select type(a_key)
    type is(integer(int32))
      print "(A,I3)", 'key ', a_key
    endselect
  endif
enddo

print "(A)", 'Explicit loop over all nodes (contents)'
do while(a_dictionary%loop_content(content=a_content))
  if (associated(a_content)) then
    select type(a_content)
    type is(integer(int32))
      print "(A,I3)", 'content ', a_content
    endselect
  endif
enddo

print "(A)", 'Repeating the loop to check saved values (contents)'
do while(a_dictionary%loop_content(content=a_content))
  if (associated(a_content)) then
    select type(a_content)
    type is(integer(int32))
      print "(A,I3)", 'content ', a_content
    endselect
  endif
enddo

print "(A)", 'Explicit loop over all nodes (key & contents)'
do while(a_dictionary%loop(key=a_key, content=a_content))
  if (allocated(a_key)) then
    select type(a_key)
    type is(integer(int32))
      print "(A,I3)", 'key ', a_key
    endselect
  endif
  if (associated(a_content)) then
    select type(a_content)
    type is(integer(int32))
      print "(A,I3)", 'content ', a_content
    endselect
  endif
enddo

print "(A)", 'Repeating the loop to check saved values (key & contents)'
do while(a_dictionary%loop(key=a_key, content=a_content))
  if (allocated(a_key)) then
    select type(a_key)
    type is(integer(int32))
      print "(A,I3)", 'key ', a_key
    endselect
  endif
  if (associated(a_content)) then
    select type(a_content)
    type is(integer(int32))
      print "(A,I3)", 'content ', a_content
    endselect
  endif
enddo

a_content => a_dictionary%get_pointer(key=3_int32)
if (associated(a_content)) then
  select type(a_content)
  type is(integer(int32))
    test_passed(1) = a_content==12_int32
  endselect
endif
print "(A,L1)", 'a_dictionary(3) = 12, is correct? ', test_passed(1)

a_content => a_dictionary%get_pointer(key=5_int32)
if (associated(a_content)) then
  select type(a_content)
  type is(integer(int32))
    test_passed(2) = a_content==13_int32
  endselect
endif
print "(A,L1)", 'a_dictionary(5) = 13, is correct? ', test_passed(2)

a_content => a_dictionary%get_pointer(key=3_int32)
another_content => a_dictionary%get_pointer(key=3_int32)
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
print "(A,L1)", 'a_dictionary(3) = 10, is correct? ', test_passed(3)

test_passed(4) = len(a_dictionary)==2
print "(A,L1)", 'len(a_dictionary) = 2, is correct? ', test_passed(4)

test_passed(5) = a_dictionary%has_key(3_int32)
print "(A,L1)", 'a_dictionary has key "3", is correct? ', test_passed(5)

test_passed(6) = .not.a_dictionary%has_key(4_int32)
print "(A,L1)", 'a_dictionary has not key "4", is correct? ', test_passed(6)

call a_dictionary%remove(key=3_int32)
test_passed(7) = .not.a_dictionary%has_key(3_int32)
print "(A,L1)", 'a_dictionary has not key "3", is correct? ', test_passed(7)

max_content = 0
call a_dictionary%traverse(iterator=iterator_max)
test_passed(8) = max_content==13
print "(A,L1)", 'max(a_dictionary) = 13, is correct? ', test_passed(8)

call a_dictionary%get_clone(key=5_int32, content=a_key)
if (allocated(a_key)) then
  select type(a_key)
  type is(integer(int32))
    test_passed(9) = a_key==13_int32
  endselect
endif
print "(A,L1)", 'a_dictionary(5) = 13, is correct? ', test_passed(9)

call a_dictionary%destroy
test_passed(10) = len(a_dictionary)==0
print "(A,L1)", 'a_dictionary destroyed, is correct? ', test_passed(10)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine dictionary_finalize
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Test [[dictionary:finalize]].
  !---------------------------------------------------------------------------------------------------------------------------------
  type(dictionary) :: local_dictionary !< A dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call local_dictionary%add_clone(key=5_int32, content=13_int32)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine dictionary_finalize

  subroutine iterator_max(key, content, done)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Iterator that computes the max of contents.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),          intent(in)  :: key     !< The node key.
  class(*), pointer, intent(in)  :: content !< The generic content.
  logical,           intent(out) :: done    !< Flag to set to true to stop traversing.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(content)) then
    select type(content)
    type is(integer(int32))
      max_content = max(max_content, content)
    endselect
  endif
  done = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine iterator_max
endprogram hasty_test_dictionary
