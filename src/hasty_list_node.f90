!< HASTY class of linked list node.
module hasty_list_node
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY class of linked list node.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_content_adt
use hasty_key_adt
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: destroy_list_node
public :: list_node
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: list_node
  !< **List node** class to storage any contents as a node of a linked list.
  class(*),        allocatable :: key              !< The key ID.
  class(*),        pointer     :: content=>null()  !< The generic  content.
  type(list_node), pointer     :: next=>null()     !< The next node in the list.
  type(list_node), pointer     :: previous=>null() !< The previous node in the list.
  contains
    ! public methods
    procedure, pass(self) :: destroy_contents !< Destroy the node contents (key & content).
    procedure, pass(self) :: is_filled        !< Return storage status.
    procedure, pass(self) :: set_pointer      !< Set the node pointer-associating content.
    procedure, pass(self) :: set_clone        !< Set the node cloning content.
    ! private methods
    procedure, pass(self), private :: destroy_key     !< Destroy the node key.
    procedure, pass(self), private :: destroy_content !< Destroy the node content.
    ! finalizer
    ! final :: finalize !< Finalize the node.
endtype list_node
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non TBP
  recursive subroutine destroy_list_node(node)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the node and its subsequent ones in the list.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(list_node), pointer, intent(inout) :: node !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(node)) then
    call node%destroy_contents
    call destroy_list_node(node=node%next)
    node%previous => null()
    deallocate(node)
    node => null()
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy_list_node

  ! public methods
  subroutine destroy_contents(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the node contents (key & content).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list_node), intent(inout) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy_key
  call self%destroy_content
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy_contents

  elemental logical function is_filled(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return storage status.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list_node), intent(in) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_filled = .false.
  if (allocated(self%key)) then
    if (associated(self%content)) then
      is_filled = .true.
      associate(content=>self%content)
        select type(content)
        class is(content_adt)
          is_filled = content%is_filled()
        endselect
      endassociate
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_filled

  subroutine set_pointer(self, key, content, next, previous)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the node pointer-associating content.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list_node), intent(inout)                 :: self     !< The node.
  class(*),         intent(in)                    :: key      !< The key ID.
  class(*),         intent(in), pointer           :: content  !< The content.
  type(list_node),  intent(in), pointer, optional :: next     !< The next node in the list.
  type(list_node),  intent(in), pointer, optional :: previous !< The previous node in the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy_key
  allocate(self%key, source=key)

  call self%destroy_content
  self%content => content

  if (present(next)) then
    ! TODO implement update pointers
  endif

  if (present(previous)) then
    ! TODO implement update pointers
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_pointer

  subroutine set_clone(self, key, content, next, previous)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the node cloning content.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list_node), intent(inout)                 :: self     !< The node.
  class(*),         intent(in)                    :: key      !< The key ID.
  class(*),         intent(in)                    :: content  !< The content.
  type(list_node),  intent(in), pointer, optional :: next     !< The next node in the list.
  type(list_node),  intent(in), pointer, optional :: previous !< The previous node in the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy_key
  allocate(self%key, source=key)

  call self%destroy_content
  allocate(self%content, source=content)

  if (present(next)) then
    ! TODO implement update pointers
  endif

  if (present(previous)) then
    ! TODO implement update pointers
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_clone

  ! private methods
  subroutine destroy_key(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the node key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list_node), intent(inout) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%key)) deallocate(self%key)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy_key

  subroutine destroy_content(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the node content.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list_node), intent(inout) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(self%content)) then
    associate(content=>self%content)
      select type(content)
      class is(content_adt)
        call content%destroy
      endselect
    endassociate
    deallocate(self%content)
    self%content => null()
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy_content

  ! finalizer
  subroutine finalize(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize the list node.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(list_node), intent(inout) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy_contents
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize
endmodule hasty_list_node
