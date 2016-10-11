!< HASTY test dictionary.
program hasty_test_dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test dictionary.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32, output_unit
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

call initialize

call tests_non_captured

a_content => a_dictionary%get_pointer(key=3_int32)
call test_assert_equal(message='a_dictionary(3) = 12', content=a_content, reference=12_int32, is_test_passed=test_passed(1))

a_content => a_dictionary%get_pointer(key=5_int32)
call test_assert_equal(message='a_dictionary(5) = 13', content=a_content, reference=13_int32, is_test_passed=test_passed(2))

a_content => a_dictionary%get_pointer(key=3_int32)
another_content => a_dictionary%get_pointer(key=3_int32)
if (associated(a_content).and.associated(another_content)) then
  select type(a_content)
  type is(integer(int32))
    a_content = 10_int32
  endselect
endif
call test_assert_equal(message='a_dictionary(3) = 10', content=another_content, reference=10_int32, is_test_passed=test_passed(3))

a_content => null()
allocate(a_content, source=int(len(a_dictionary), int32))
call test_assert_equal(message='len(a_dictionary) = 2', content=a_content, reference=2_int32, is_test_passed=test_passed(4))

call test_assert_true(message='a_dictionary has key "3"', is_true=a_dictionary%has_key(3_int32), is_test_passed=test_passed(5))

call test_assert_false(message='a_dictionary has not key "4"', &
                       is_false=a_dictionary%has_key(4_int32), is_test_passed=test_passed(6))

call a_dictionary%remove(key=3_int32)
call test_assert_false(message='a_dictionary has not (more) key "3"', &
                       is_false=a_dictionary%has_key(3_int32), is_test_passed=test_passed(7))

max_content = 0
call a_dictionary%traverse(iterator=iterator_max)
a_content => null()
allocate(a_content, source=max_content)
call test_assert_equal(message='max(a_dictionary) = 13', content=a_content, reference=13_int32, is_test_passed=test_passed(8))

call a_dictionary%get_clone(key=5_int32, content=a_key)
call test_assert_equal(message='a_dictionary(5) = 13', content=a_key, reference=13_int32, is_test_passed=test_passed(9))

call a_dictionary%destroy
a_content => null()
allocate(a_content, source=int(len(a_dictionary), int32))
call test_assert_equal(message='a_dictionary destroyed', content=a_content, reference=0_int32, is_test_passed=test_passed(10))

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! auxiliary procedures
  subroutine initialize
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize tests.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(a_key, source=3_int32)
  allocate(a_content, source=12_int32)

  call a_dictionary%add_pointer(key=a_key, content=a_content)
  call a_dictionary%add_clone(key=5_int32, content=13_int32)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize

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

  ! tests
  subroutine test_assert_equal(message, content, reference, is_test_passed)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Test `content==reference`.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*),   intent(in)  :: message        !< Message to print.
  class(*),       intent(in)  :: content        !< Content value.
  integer(int32), intent(in)  :: reference      !< Reference value.
  logical,        intent(out) :: is_test_passed !< Test result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_test_passed = .false.
  select type(content)
  type is(integer(int32))
    is_test_passed = content==reference
  endselect
  print "(A,L1)", message//', is correct? ', is_test_passed
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_assert_equal

  subroutine test_assert_false(message, is_false, is_test_passed)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Test `is_false==.false.`.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in)  :: message        !< Message to print.
  logical,      intent(in)  :: is_false       !< Test value.
  logical,      intent(out) :: is_test_passed !< Test result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_test_passed = is_false.eqv..false.
  print "(A,L1)", message//', is correct? ', is_test_passed
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_assert_false

  subroutine test_assert_true(message, is_true, is_test_passed)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Test `is_true==.true.`.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in)  :: message        !< Message to print.
  logical,      intent(in)  :: is_true        !< Test value.
  logical,      intent(out) :: is_test_passed !< Test result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_test_passed = is_true.eqv..true.
  print "(A,L1)", message//', is correct? ', is_test_passed
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_assert_true

  subroutine test_dictionary_finalize
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Test [[dictionary:finalize]].
  !---------------------------------------------------------------------------------------------------------------------------------
  type(dictionary) :: local_dictionary !< A dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call local_dictionary%add_clone(key=5_int32, content=13_int32)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_dictionary_finalize

  subroutine tests_non_captured
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Tests with non-captured results.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  print '(A)', 'Keys in dictionary:'
  call a_dictionary%print_keys

  print "(A)", 'Explicit loop over all nodes (key & contents)'
  do while(a_dictionary%loop(key=a_key, content=a_content))
    if (allocated(a_key)) then
      select type(a_key)
      type is(integer(int32))
        write(output_unit, "(A,I3)", advance='no') 'key ', a_key
      endselect
    endif
    if (associated(a_content)) then
      select type(a_content)
      type is(integer(int32))
        print "(A,I3)", ' content ', a_content
      endselect
    endif
  enddo

  print "(A)", 'Repeating the loop to check saved values (key & contents)'
  do while(a_dictionary%loop(key=a_key, content=a_content))
    if (allocated(a_key)) then
      select type(a_key)
      type is(integer(int32))
        write(output_unit, "(A,I3)", advance='no') 'key ', a_key
      endselect
    endif
    if (associated(a_content)) then
      select type(a_content)
      type is(integer(int32))
        print "(A,I3)", ' content ', a_content
      endselect
    endif
  enddo

  call test_dictionary_finalize
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine tests_non_captured
endprogram hasty_test_dictionary
