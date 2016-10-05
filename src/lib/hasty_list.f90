!< HASTY class of linked list.
module hasty_list
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY class of linked list.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_key_adt
use hasty_list_node
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: key_iterator_interface
public :: len
public :: list
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: list
  !< **List** class to storage any contents by means of abstract container nodes.
  integer(I4P)             :: nodes_number=0 !< Number of nodes in the list.
  type(list_node), pointer :: head=>null()   !< The first node in the list.
  type(list_node), pointer :: tail=>null()   !< The last node in the list.
  contains
    ! public methods
    procedure, pass(self) :: add_pointer !< Add a node pointer to the list.
    procedure, pass(self) :: add_clone   !< Add a node to the list cloning contents (non pointer add).
    procedure, pass(self) :: destroy     !< Destroy the list.
    procedure, pass(self) :: get         !< Return a pointer to a node's container in the list.
    procedure, pass(self) :: has_key     !< Check if the key is present in the list.
    procedure, pass(self) :: node        !< Return a pointer to a node in the list.
    procedure, pass(self) :: print_keys  !< Print the list of keys.
    procedure, pass(self) :: remove      !< Remove a node from the list, given the key.
    procedure, pass(self) :: traverse    !< Traverse list from head to tail calling the iterator procedure.
    ! private methods
    procedure, pass(self), private :: remove_by_pointer !< Remove node from list, given pointer to it.
    procedure, pass(self), private :: traverse_iterator !< Traverse list from head to tail calling the iterator procedure.
    ! finalizer
    final :: finalize !< Finalize the list.
endtype list

! public interfaces
abstract interface
  !< Iterator procedure for traversing all nodes in a list by keys.
  subroutine key_iterator_interface(key, done, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Iterator procedure for traversing all nodes in a list by keys.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),          intent(in)            :: key     !< The node key.
  logical,           intent(out)           :: done    !< Flag to set to true to stop traversing.
  class(*), pointer, intent(out), optional :: content !< The generic  content.
  !---------------------------------------------------------------------------------------------------------------------------------
  end subroutine key_iterator_interface
endinterface

interface len
  module procedure list_len
endinterface

! private interfaces
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
  subroutine add_pointer(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node pointer to the list.
  !<
  !< @note If a node with the same key is already in the list, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(inout)       :: self    !< The list.
  class(*),    intent(in)          :: key     !< The key ID.
  class(*),    intent(in), pointer :: content !< The content.
  type(list_node), pointer         :: p       !< Pointer to scan the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'

  ! if the node is already there, then remove it
  p => self%node(key=key) ! associate p to the pointer allocated
  if (associated(p)) call self%remove_by_pointer(p)

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
  call p%set_pointer(key=key, content=content)

  self%nodes_number = self%nodes_number + 1
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_pointer

  subroutine add_clone(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node to the list cloning content (non pointer add).
  !<
  !< @note If a node with the same key is already in the list, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(inout) :: self    !< The list.
  class(*),    intent(in)    :: key     !< The key ID.
  class(*),    intent(in)    :: content !< The content.
  type(list_node), pointer   :: p       !< Pointer to scan the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'

  ! if the node is already there, then remove it
  p => self%node(key=key) ! associate p to the pointer allocated
  if (associated(p)) call self%remove_by_pointer(p)

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
  call p%set_clone(key=key, content=content)

  self%nodes_number = self%nodes_number + 1
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

  function get(self, key) result(content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a pointer to a node's content in the list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(in)  :: self    !< The list.
  class(*),    intent(in)  :: key     !< The key ID.
  class(*), pointer        :: content !< content pointer of the queried node.
  type(list_node), pointer :: p       !< Pointer to scan the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  content => null()
  p => self%node(key=key)
  if (associated(p)) content => p%content
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get

  function has_key(self, key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the key is present in the list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(in)  :: self    !< The list.
  class(*),    intent(in)  :: key     !< The key ID.
  logical                  :: has_key !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_key = .false.
  call self%traverse_iterator(iterator=key_iterator_search)
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine key_iterator_search(node, done)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Iterator procedure for searching a key.
    !-------------------------------------------------------------------------------------------------------------------------------
    type(list_node), intent(in), pointer :: node !< Actual node pointer in the list.
    logical,         intent(out)         :: done !< Flag to set to true to stop traversing.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    has_key = are_keys_equal(lhs=node%key, rhs=key)
    done = has_key
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine key_iterator_search
  endfunction has_key

  function node(self, key) result(p)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a pointer to a node in the list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(in)  :: self !< The list.
  class(*),    intent(in)  :: key  !< The key ID.
  type(list_node), pointer :: p    !< Pointer to node queried.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  p => null()
  call self%traverse_iterator(iterator=key_iterator_search)
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine key_iterator_search(node, done)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Iterator procedure for searching a key.
    !-------------------------------------------------------------------------------------------------------------------------------
    type(list_node), intent(in), pointer :: node !< Actual node pointer in the list.
    logical,         intent(out)         :: done !< Flag to set to true to stop traversing.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    done = are_keys_equal(lhs=node%key, rhs=key)
    if (done) then
      p => node
    endif
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine key_iterator_search
  endfunction node

  subroutine print_keys(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print the list of keys.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(in) :: self !< The list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%traverse_iterator(iterator=key_iterator_print)
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine key_iterator_print(node, done)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Iterator procedure for printing a key.
    !-------------------------------------------------------------------------------------------------------------------------------
    type(list_node), intent(in), pointer :: node    !< Actual node pointer in the list.
    logical,         intent(out)         :: done    !< Flag to set to true to stop traversing.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (allocated(node%key)) print '(A)', str_key(node%key)
    done = .false. ! never stop until the tail
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine key_iterator_print
  endsubroutine print_keys

  subroutine remove(self, key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove a node from the list, given the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(inout) :: self !< The list.
  class(*),    intent(in)    :: key  !< The key ID, can be any [[key_adt]] concrete extensions.
  type(list_node), pointer   :: p    !< Pointer to scan the list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  p => self%node(key=key)
  if (associated(p)) call self%remove_by_pointer(p=p)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine remove

  subroutine traverse(self, iterator)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Traverse list from head to tail calling the iterator procedure.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(in)           :: self     !< The list.
  procedure(key_iterator_interface) :: iterator !< The (key) iterator procedure to call for each node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%traverse_iterator(key_iterator_wrapper)
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine key_iterator_wrapper(node, done)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Wrapper for calling the user-specified key_iterator procedure.
    !-------------------------------------------------------------------------------------------------------------------------------
    type(list_node), intent(in), pointer :: node !< The list node.
    logical,         intent(out)         :: done !< Flag to set to true to stop traversing.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    call iterator(key=node%key, done=done)
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine key_iterator_wrapper
  endsubroutine traverse

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

  subroutine traverse_iterator(self, iterator)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Traverse list from head to tail calling the iterator procedure.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(list), intent(in)       :: self      !< The list.
  procedure(iterator_interface) :: iterator  !< The iterator procedure to call for each node.
  type(list_node), pointer      :: p         !< Pointer to scan the list.
  logical                       :: done      !< Flag to set to true to stop traversing.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  done = .false.
  p => self%head
  do
    if (associated(p)) then
      call iterator(node=p, done=done)
      if (done) exit
      p => p%next
    else
      exit
    endif
  enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine traverse_iterator

  ! finalizer
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
