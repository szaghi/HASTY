!< HASTY test hash table CAF basic.
program hasty_test_caf_basic
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test hash table CAF basic.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32, int64, error_unit
use hasty
use tester
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(tester_t)        :: hasty_tester  !< Tests handler.
type(hash_table)      :: a_table       !< A table.
class(*), pointer     :: a_content     !< A content.
class(*), allocatable :: a_new_content !< A content.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
call hasty_tester%init

call a_table%initialize(buckets_number=4, use_prime=.true.)

#ifdef CAF

call a_table%add_clone(key=1_int32, content=int(this_image(), int32))
call a_table%add_clone(key=3_int32, content=int(this_image(), int32))
call a_table%add_clone(key=10_int32, content=int(this_image(), int32))
call a_table%add_clone(key=13_int32, content=int(this_image(), int32))
call a_table%add_clone(key=16_int32, content=int(this_image(), int32))
call a_table%add_clone(key=22_int32, content=int(this_image(), int32))
call a_table%add_clone(key=27_int32, content=int(this_image(), int32))
call a_table%add_clone(key=31_int32, content=int(this_image(), int32))
call a_table%add_clone(key=40_int32, content=int(this_image(), int32))

sync all
a_content => a_table%get_pointer(key=3_int32)
if (associated(a_content)) then
  call test_assert_equal(content=a_content, reference=int(this_image(), int32))
  call hasty_tester%print
  write(error_unit, *) '  I am image: ', this_image(), ' and I HAVE key=3'
else
  write(error_unit, *) '  I am image: ', this_image(), ' and I have NOT key=3'
endif
sync all

sync all
critical
call a_table%get_clone(key=3_int32, content=a_new_content)
end critical
! if (allocated(a_new_content)) then
!   call test_assert_equal(content=a_content, reference=int(mod(mod(3, 5) + 1, num_images())+1, int32))
!   call hasty_tester%print
!   write(error_unit, *) '  I am image: ', this_image(), ' and I HAVE a copy key=3'
! else
!   write(error_unit, *) '  I am image: ', this_image(), ' and I have NOT a copy key=3'
! endif
sync all

if (this_image()==1) write(error_unit, *) 'Keys/Contents'
sync all
critical
call a_table%traverse(iterator=print_content_iterator)
end critical
sync all

if (this_image()==1) write(error_unit, *) 'IDs'
sync all
critical
write(error_unit, *) '  image: ', this_image(), ' ids = ', a_table%ids()
end critical
sync all

if (this_image()==1) write(error_unit, *) 'IDs global'
sync all
critical
write(error_unit, *) '  image: ', this_image(), ' ids global = ', a_table%ids(global=.true.)
end critical
sync all

if (this_image()==1) write(error_unit, *) 'Length'
sync all
critical
write(error_unit, *) '  image: ', this_image(), ' table length ', len(a_table)
end critical
sync all

if (this_image()==1) write(error_unit, *) 'Global length', len(a_table, global=.true.)

call a_table%destroy

if (this_image()==1) write(error_unit, *) 'Length'
sync all
critical
write(error_unit, *) '  image: ', this_image(), ' table length ', len(a_table)
end critical
sync all

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
        write(error_unit, *) '  image: ', this_image(), 'key: ', key%stringify(), ' content: ', content
#endif
      endselect
    endselect
  endif
  done = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_content_iterator
endprogram hasty_test_caf_basic
