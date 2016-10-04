!< HASTY class of linked list node.
module hasty_list_node
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY class of linked list node.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use hasty_container_adt
! use hasty_key_adt
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
  class(*), allocatable    :: key              !< The key ID.
  class(*),        pointer :: container        !< The container for generic contents.
  type(list_node), pointer :: next=>null()     !< The next node in the list.
  type(list_node), pointer :: previous=>null() !< The previous node in the list.
  contains
    ! public methods
    procedure, pass(self) :: destroy_contents !< Destroy the node contents (key & container).
    procedure, pass(self) :: is_filled        !< Return storage status.
    procedure, pass(self) :: set              !< Set the node.
    procedure, pass(self) :: set_clone        !< Set the node cloning contents.
    ! private methods
    procedure, pass(self) :: destroy_key       !< Destroy the node key.
    procedure, pass(self) :: destroy_container !< Destroy the node container.
endtype list_node
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non TBP
  recursive subroutine destroy_list_node(node)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the node and its subsequent ones in the list.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(list_node), intent(inout), pointer :: node !< The node.
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
  !< Destroy the node contents (key & container).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list_node), intent(inout) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy_key
  call self%destroy_container
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
    if (associated(self%container)) then
      is_filled = .true.
      associate(container=>self%container)
        select type(container)
        class is(container_adt)
          is_filled = container%is_filled()
        endselect
      endassociate
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_filled

  subroutine set(self, key, container, content, next, previous)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the node.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list_node), intent(inout)                 :: self      !< The node.
  class(*),         intent(in),          optional :: key       !< The key ID.
  class(*),         intent(in), pointer, optional :: container !< The container.
  class(*),         intent(in), pointer, optional :: content   !< The content.
  type(list_node),  intent(in), pointer, optional :: next      !< The next node in the list.
  type(list_node),  intent(in), pointer, optional :: previous  !< The previous node in the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(key)) then
    call self%destroy_key
    allocate(self%key, source=key)
  endif

  if (present(container)) then
    call self%destroy_container
    self%container => container
  endif

  if (present(content).and.associated(self%container)) then
    associate(container=>self%container)
      select type(container)
      class is(container_adt)
        call container%set(content=content)
      endselect
    endassociate
  endif

  if (present(next)) then
    ! TODO implement update pointers
  endif

  if (present(previous)) then
    ! TODO implement update pointers
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  subroutine set_clone(self, key, container, content, next, previous)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the node.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list_node), intent(inout)                 :: self      !< The node.
  class(*),         intent(in),          optional :: key       !< The key ID.
  class(*),         intent(in),          optional :: container !< The container.
  class(*),         intent(in),          optional :: content   !< The content.
  type(list_node),  intent(in), pointer, optional :: next      !< The next node in the list.
  type(list_node),  intent(in), pointer, optional :: previous  !< The previous node in the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(key)) then
    call self%destroy_key
    allocate(self%key, source=key)
  endif

  if (present(container)) then
    call self%destroy_container
    allocate(self%container, source=container)
  endif

  if (present(content).and.associated(self%container)) then
    associate(container=>self%container)
      select type(container)
      class is(container_adt)
        call container%set(content=content)
      endselect
    endassociate
  endif

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

  subroutine destroy_container(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the node container.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list_node), intent(inout) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(self%container)) then
    associate(container=>self%container)
      select type(container)
      class is(container_adt)
        call container%destroy
      endselect
    endassociate
    deallocate(self%container)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy_container
endmodule hasty_list_node
