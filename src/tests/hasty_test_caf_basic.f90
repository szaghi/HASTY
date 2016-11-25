!< HASTY test hash table CAF basic.
program hasty_test_caf_basic
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test hash table CAF basic.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32, int64
use hasty
use tester
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(tester_t)    :: hasty_tester !< Tests handler.
type(hash_table)  :: a_table      !< A table.
class(*), pointer :: a_content    !< A content.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
call hasty_tester%init

call a_table%initialize(buckets_number=5)

#ifdef CAF
call a_table%add_clone(key=3_int32, content=int(this_image(), int32))
call a_table%add_clone(key=10_int32, content=int(this_image(), int32))
call a_table%add_clone(key=13_int32, content=int(this_image(), int32))
call a_table%add_clone(key=16_int32, content=int(this_image(), int32))
call a_table%add_clone(key=22_int32, content=int(this_image(), int32))
call a_table%add_clone(key=27_int32, content=int(this_image(), int32))
call a_table%add_clone(key=31_int32, content=int(this_image(), int32))
call a_table%add_clone(key=40_int32, content=int(this_image(), int32))

! a_content => a_table%get_pointer(key=1_int32)
! if (associated(a_content)) call test_assert_equal(content=a_content, reference=int(this_image(), int32))

call a_table%traverse(iterator=print_content_iterator)
#endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
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

  subroutine print_content_iterator(key, content, done)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Iterator that print contents.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),          intent(in)  :: key     !< The node key.
  class(*), pointer, intent(in)  :: content !< The generic content.
  logical,           intent(out) :: done    !< Flag to set to true to stop traversing.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(content)) then
    select type(content)
    type is(integer(int32))
      select type(key)
      type is(key_base)
#ifdef CAF
        print*, 'image: ', this_image(), 'key: ', key%stringify(), ' content: ', content
#endif
      endselect
    endselect
  endif
  done = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_content_iterator
endprogram hasty_test_caf_basic
