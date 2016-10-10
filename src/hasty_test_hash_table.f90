!< HASTY test hash table.
program hasty_test_hash_table
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test hash table.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32
use hasty
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
class(*), allocatable :: a_key           !< A key.
class(*), pointer     :: a_content       !< A content.
class(*), allocatable :: another_content !< Another content.
type(hash_table)      :: a_table         !< A table.
integer(int32)        :: max_content     !< Maximum content value.
logical               :: test_passed(8)  !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

call hash_table_finalize

allocate(a_key, source=3_int32)
allocate(a_content, source=12_int32)

call a_table%add_pointer(key=a_key, content=a_content)
call a_table%add_clone(key=5_int32, content=13_int32)
print '(A)', 'Keys in table:'
call a_table%print_keys

call a_table%add_clone(key=5_int32, content=13_int32)
a_content => a_table%get_pointer(key=5_int32)
if (associated(a_content)) then
  select type(a_content)
  type is(integer(int32))
    test_passed(1) = a_content==13_int32
  endselect
endif
print "(A,L1)", 'a_table(5) = 13, is correct? ', test_passed(1)

test_passed(2) = a_table%has_key(3_int32)
print "(A,L1)", 'a_table has key "3", is correct? ', test_passed(2)

test_passed(3) = len(a_table)==2
print "(A,L1)", 'len(a_table) = 2, is correct? ', test_passed(3)

max_content = 0
call a_table%traverse(iterator=iterator_max)
test_passed(4) = max_content==13
print "(A,L1)", 'max(a_table) = 13, is correct? ', test_passed(4)

call a_table%remove(key=3_int32)
test_passed(5) = .not.a_table%has_key(3_int32)
print "(A,L1)", 'a_table has not key "3", is correct? ', test_passed(5)

call a_table%get_clone(key=5_int32, content=another_content)
if (allocated(another_content)) then
  select type(another_content)
  type is(integer(int32))
    test_passed(6) = another_content==13_int32
  endselect
endif
print "(A,L1)", 'a_table(5) = 13, is correct? ', test_passed(6)

call a_table%destroy
call a_table%initialize(buckets_number=11, homogeneous=.true., typeguard_key='a string', typeguard_content=1_int32)
test_passed(7) = a_table%is_initialized()
print "(A,L1)", 'a_table is initialized, is correct? ', test_passed(7)
test_passed(8) = a_table%is_homogeneous()
print "(A,L1)", 'a_table is homogeneous, is correct? ', test_passed(8)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine hash_table_finalize
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Test [[hash_table:finalize]].
  !---------------------------------------------------------------------------------------------------------------------------------
  type(hash_table) :: local_hash_table !< A hash_table.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call local_hash_table%add_clone(key=5_int32, content=13_int32)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine hash_table_finalize

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
endprogram hasty_test_hash_table
