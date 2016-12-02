!< HASTY test hash table CAF basic.
program hasty_test_caf_get_clone
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test hash table CAF basic.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32, int64, error_unit
use hasty
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(hash_table)      :: a_table   !< A table.
class(*), allocatable :: a_content !< A content.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
call a_table%initialize(buckets_number=13)

#ifdef CAF

sync all
if (mod(this_image(), 2) /= 0) call a_table%add_clone(key=1_int32, content=int(this_image(), int32))
if (mod(this_image(), 2) == 0) call a_table%add_clone(key=2_int32, content=int(this_image(), int32))
if (mod(this_image(), 2) /= 0) call a_table%add_clone(key=3_int32, content=int(this_image(), int32))
if (mod(this_image(), 2) == 0) call a_table%add_clone(key=4_int32, content=int(this_image(), int32))
if (mod(this_image(), 2) /= 0) call a_table%add_clone(key=5_int32, content=int(this_image(), int32))
if (mod(this_image(), 2) == 0) call a_table%add_clone(key=6_int32, content=int(this_image(), int32))
sync all

print*, 'hello from image: ', this_image()
call a_table%traverse(iterator=print_content_iterator)

if (this_image()==2) then
  print*, 'getting key=3 from image 2'
  call a_table%get_clone(key=3_int32, content=a_content)
  if (allocated(a_content)) then
    select type(a_content)
    type is(integer(int32))
      print*, 'content: ', a_content
    endselect
  else
    print*, 'getting failed!'
  endif
endif

#endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
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
endprogram hasty_test_caf_get_clone
