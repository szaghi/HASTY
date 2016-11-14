!< HASTY test hash table.
program hasty_test_hash_table
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test hash table.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32
use hasty
use tester
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(tester_t)        :: hasty_tester    !< Tests handler.
class(*), allocatable :: a_key           !< A key.
class(*), pointer     :: a_content       !< A content.
class(*), allocatable :: another_content !< Another content.
type(hash_table)      :: a_table         !< A table.
integer(int32)        :: max_content     !< Maximum content value.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
call hasty_tester%init

call initialize

call tests_non_captured

call a_table%add_clone(key=5_int32, content=13_int32)
a_content => a_table%get_pointer(key=5_int32)
call test_assert_equal(content=a_content, reference=13_int32)

call hasty_tester%assert_equal(a_table%has_key(3_int32), .true.)

call hasty_tester%assert_equal(len(a_table), 2_int32)

max_content = 0
call a_table%traverse(iterator=iterator_max)
call hasty_tester%assert_equal(max_content, 13)

call a_table%remove(key=3_int32)
call hasty_tester%assert_equal(a_table%has_key(3_int32), .false.)

call a_table%get_clone(key=5_int32, content=another_content)
if (allocated(another_content)) then
  call test_assert_equal(content=another_content, reference=13_int32)
endif

call hasty_tester%assert_equal(int(a_table%ids(), int32), [5_int32, 5_int32])

call a_table%destroy
call a_table%initialize(buckets_number=11, homogeneous=.true., typeguard_key='a string', typeguard_content=1_int32)
call hasty_tester%assert_equal(a_table%is_initialized(), .true.)
call hasty_tester%assert_equal(a_table%is_homogeneous(), .true.)

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

  call a_table%add_pointer(key=a_key, content=a_content)
  call a_table%add_clone(key=5_int32, content=13_int32)
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

  subroutine tests_non_captured
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Tests with non-captured results.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  print '(A)', 'Keys in table:'
  call a_table%print_keys

  call hash_table_finalize
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine tests_non_captured
endprogram hasty_test_hash_table
