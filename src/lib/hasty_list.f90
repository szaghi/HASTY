!< HASTY class of linked list.
module hasty_list
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY class of linked list.
!-----------------------------------------------------------------------------------------------------------------------------------
! use hasty_container_adt
use hasty_key_adt
use hasty_list_node
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: len
public :: list
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: list
  !< **List** class to storage any contents by means abstract container nodes.
  integer(I4P)             :: nodes_number=0 !< Number of nodes in the list.
  type(list_node), pointer :: head=>null()   !< The first node in the list.
  type(list_node), pointer :: tail=>null()   !< The last node in the list.
  contains
    ! public methods
    procedure, pass(self) :: add       !< Add a node to the list.
    procedure, pass(self) :: add_clone !< Add a clone of the node to the list (non pointer add).
    procedure, pass(self) :: destroy   !< Destroy the list.
    procedure, pass(self) :: get       !< Return a pointer to a node's container in the list.
    procedure, pass(self) :: has_key   !< Check if the key is present in the list.
    procedure, pass(self) :: node      !< Return a pointer to a node in the list.
    procedure, pass(self) :: remove    !< Remove a node from the list, given the key.
    ! private methods
    procedure, pass(self), private :: remove_by_pointer !< Remove node from list, given pointer to it.
    final                          :: finalize          !< Finalize the list.
endtype list

abstract interface
  !< Iterator procedure for traversing all nodes in a list.
  subroutine iterator_interface(node, done)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Iterator procedure for traversing all nodes in a list.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: list_node
  type(list_node), intent(in), pointer :: node !< Actual node pointer in the list.
  logical,         intent(out)         :: done !< Flag to set to true to stop traversing.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine iterator_interface
endinterface

abstract interface
  !< Iterator procedure for traversing all nodes in a list by keys.
  subroutine key_iterator(key, done)
  !< Iterator procedure for traversing all nodes in a list by keys.
  import :: key_adt
  class(key_adt), intent(in)  :: key  !< The node key.
  logical,        intent(out) :: done !< Flag to set to true to stop traversing.
  end subroutine key_iterator
endinterface

interface len
  module procedure list_len
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non TBP
  elemental function list_len(self) result(length)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the number of nodes of the list, namely the list length.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(list), intent(in) :: self   !< The list.
  integer(I4P)           :: length !< The list length.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  length = self%nodes_number
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction list_len

  ! public methods
  subroutine add(self, key, container, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node to the list.
  !<
  !< @note If a node with the same key is already in the list, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(inout)                 :: self      !< The list.
  class(*),    intent(in)                    :: key       !< The key ID.
  class(*),    intent(in), pointer           :: container !< The container.
  class(*),    intent(in), pointer, optional :: content   !< The content.
  type(list_node), pointer                   :: p         !< Pointer to scan the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! check key type
  select type (key)
  type is(integer(I1P))
    ! allowed
  type is(integer(I2P))
    ! allowed
  type is(integer(I4P))
    ! allowed
  type is(integer(I8P))
    ! allowed
  type is(character(len=*))
    if (len_trim(key)<1) error stop 'error: key-string must be nonblank'
  class is (key_adt)
    ! allowed
  class default
      error stop 'error: key must be a concrete extension of key_adt abstract class'
  endselect

  ! if the node is already there, then remove it
  ! associate p to the pointer allocated
  p => self%node(key=key)
  if (associated(p)) then
    call self%remove_by_pointer(p)
  else ! a new node is inserted, the count must be updated
    self%nodes_number = self%nodes_number + 1
  endif

  ! update next/previous pointers
  if (associated(self%tail)) then ! insert new node at the end
    allocate(self%tail%next)
    p => self%tail%next
    p%previous => self%tail
  else
    allocate(self%head) ! insert new node as first node
    p => self%head
  end if
  self%tail => p

  ! fill the new node with provided contents
  call p%set(key=key, container=container, content=content)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add

  subroutine add_clone(self, key, container, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a clone of the node to the list (non pointer add).
  !<
  !< @note If a node with the same key is already in the list, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(inout)        :: self      !< The list.
  class(*),    intent(in)           :: key       !< The key ID.
  class(*),    intent(in)           :: container !< The container.
  class(*),    intent(in), optional :: content   !< The content.
  type(list_node), pointer          :: p         !< Pointer to scan the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! check key type
  select type (key)
  type is(integer(I1P))
    ! allowed
  type is(integer(I2P))
    ! allowed
  type is(integer(I4P))
    ! allowed
  type is(integer(I8P))
    ! allowed
  type is(character(len=*))
    if (len_trim(key)<1) error stop 'error: key-string must be nonblank'
  class is (key_adt)
    ! allowed
  class default
      error stop 'error: key must be a concrete extension of key_adt abstract class'
  endselect

  ! if the node is already there, then remove it
  ! associate p to the pointer allocated
  p => self%node(key=key)
  if (associated(p)) then
    call self%remove_by_pointer(p)
  else ! a new node is inserted, the count must be updated
    self%nodes_number = self%nodes_number + 1
  endif

  ! update next/previous pointers
  if (associated(self%tail)) then ! insert new node at the end
    allocate(self%tail%next)
    p => self%tail%next
    p%previous => self%tail
  else
    allocate(self%head) ! insert new node as first node
    p => self%head
  end if
  self%tail => p

  ! fill the new node with provided contents
  call p%set_clone(key=key, container=container, content=content)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_clone

  subroutine destroy(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(inout) :: self !< The list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(self%head)) call destroy_list_node(node=self%head)
  self%head => null()
  self%tail => null()
  self%nodes_number = 0
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy

  function get(self, key) result(container)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a pointer to a node's container in the list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(in)  :: self      !< The list.
  class(*),    intent(in)  :: key       !< The key ID.
  class(*), pointer        :: container !< Container pointer of the queried node.
  type(list_node), pointer :: p         !< Pointer to scan the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  p => self%node(key=key)
  if (associated(p)) container => p%container
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get

  function has_key(self, key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the key is present in the list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(in)  :: self    !< The list.
  class(*),    intent(in)  :: key     !< The key ID.
  logical                  :: has_key !< Check result.
  type(list_node), pointer :: p       !< Pointer to scan the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_key = .false.
  p => self%head
  do
    if (associated(p)) then
      if (allocated(p%key)) then
        if (are_keys_equal(lhs=p%key, rhs=key)) then
          has_key = .true.
          exit
        endif
      endif
      p => p%next
    else
      exit
    endif
  enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_key

  function node(self, key) result(p)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a pointer to a node in the list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(in)  :: self !< The list.
  class(*),    intent(in)  :: key  !< The key ID.
  type(list_node), pointer :: p    !< Pointer to node queried.
  type(list_node), pointer :: pp   !< Pointer to scan the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  p => null()
  pp => self%head
  do
    if (associated(pp)) then
      if (allocated(pp%key)) then
        if (are_keys_equal(lhs=pp%key, rhs=key)) then
          p => pp
          return
        endif
      endif
      pp => pp%next
    else
      return ! not found
    endif
  enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction node

  subroutine remove(self, key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove a node from the list, given the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list),    intent(inout) :: self !< The list.
  class(key_adt), intent(in)    :: key  !< The key ID, can be any [[key_adt]] concrete extensions.
  type(list_node), pointer      :: p    !< Pointer to scan the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  p => self%node(key=key)
  call self%remove_by_pointer(p=p)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine remove

  ! private methods
  subroutine remove_by_pointer(self, p)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove node from list, given pointer to it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list),     intent(inout)          :: self         !< The list.
  type(list_node), intent(inout), pointer :: p            !< Pointer to the node to remove.
  logical                                 :: has_next     !< Check if list node has a next item.
  logical                                 :: has_previous !< Check if list node has a previous item.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(p)) then
    call p%destroy_contents ! destroy the node contents
    has_next     = associated(p%next)
    has_previous = associated(p%previous)
    if (has_next.and.has_previous) then ! neither first nor last in a list
      p%previous%next => p%next
      p%next%previous => p%previous
    elseif (has_next.and.(.not.has_previous)) then ! first one in a list
      self%head          => p%next
      self%head%previous => null()
    elseif (has_previous.and.(.not.has_next)) then ! last one in a list
        self%tail      => p%previous
        self%tail%next => null()
    elseif ((.not.has_previous).and.(.not.has_next)) then  ! only one in the list
        self%head => null()
        self%tail => null()
    endif
    deallocate(p)
    p => null()
    self%nodes_number = self%nodes_number - 1
  endif
  endsubroutine remove_by_pointer

  subroutine finalize(self)
  !< Finalize the list.
  !<
  !< A wrapper for [[list:destroy]]
  !---------------------------------------------------------------------------------------------------------------------------------
  type(list),  intent(inout) :: self !< The list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize
endmodule hasty_list
