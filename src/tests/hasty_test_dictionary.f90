!< HASTY test dictionary.
program hasty_test_dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test dictionary.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32, int64, output_unit
use hasty
use tester
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(tester_t)        :: hasty_tester    !< Tests handler.
class(*), allocatable :: a_key           !< A key.
class(*), pointer     :: a_content       !< A content.
class(*), pointer     :: another_content !< Another content.
type(dictionary)      :: a_dictionary    !< A dictionary.
integer(int32)        :: max_content     !< Maximum content value.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
call hasty_tester%init

call initialize

call tests_non_captured

a_content => a_dictionary%get_pointer(key=3_int32)
call test_assert_equal(content=a_content, reference=12_int32)

a_content => a_dictionary%get_pointer(key=5_int32)
call test_assert_equal(content=a_content, reference=13_int32)

a_content => a_dictionary%get_pointer(key=3_int32)
another_content => a_dictionary%get_pointer(key=3_int32)
if (associated(a_content).and.associated(another_content)) then
  select type(a_content)
  type is(integer(int32))
    a_content = 10_int32
  endselect
endif
call test_assert_equal(content=another_content, reference=10_int32)

a_content => null()
allocate(a_content, source=int(len(a_dictionary), int32))
call test_assert_equal(content=a_content, reference=3_int32)

call hasty_tester%assert_equal(a_dictionary%has_key(3_int32), .true.)

call hasty_tester%assert_equal(a_dictionary%has_key(4_int32), .false.)

call a_dictionary%remove(key=3_int32)
call hasty_tester%assert_equal(a_dictionary%has_key(3_int32), .false.)

max_content = 0
call a_dictionary%traverse(iterator=iterator_max)
a_content => null()
allocate(a_content, source=max_content)
call test_assert_equal(content=a_content, reference=13_int32)

call a_dictionary%get_clone(key=5_int32, content=a_key)
call test_assert_equal(content=a_key, reference=13_int32)

call a_dictionary%remove(key='foo')
call hasty_tester%assert_equal(a_dictionary%ids(), [5_int64, 5_int64])

call a_dictionary%remove(key=5_int64)
call hasty_tester%assert_equal(a_dictionary%ids(), [0_int64, 0_int64])

call a_dictionary%destroy
a_content => null()
allocate(a_content, source=int(len(a_dictionary), int32))
call test_assert_equal(content=a_content, reference=0_int32)

call hasty_tester%print
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
  call a_dictionary%add_clone(key='foo', content=0_int32)
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
  subroutine test_assert_equal(content, reference)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Test `content==reference`.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),       intent(in) :: content   !< Content value.
  integer(int32), intent(in) :: reference !< Reference value.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select type(content)
  type is(integer(int32))
    call hasty_tester%assert_equal(content, reference)
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_assert_equal

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
