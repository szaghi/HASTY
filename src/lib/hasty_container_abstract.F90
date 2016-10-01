!< HASTY abstract **container** class.
module hasty_container_abstract
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY abstract **container** class.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: container_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: container_abstract
  !< Abstract **container** class to storage any contents.
  class(*), pointer :: storage_=>null()   !< Storage of containers' contents.
  logical           :: is_filled_=.false. !< Check if the container is set.
  contains
    ! public methods
    procedure, pass(self) :: is_filled !< Return storage status.
    ! public generics
    generic, public :: operator(==) => is_equal !< Overloading `==` operator.
    ! private methods
    procedure, private :: is_equal !< Implement `==` operator.
endtype container_abstract
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  elemental logical function is_equal(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Implement `==` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(container_abstract), intent(in) :: lhs !< Left hand side.
  class(container_abstract), intent(in) :: rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.same_type_as(lhs, rhs)) then
    is_equal = .false.
    return
  end if
  if ((.not.lhs%filled).and.(.not.rhs%filled)) then
    is_equal = .true.
    return
  end if
  if (lhs%filled.neqv.rhs%filled) then
    is_equal = .false.
    return
  end if
  is_equal = lhs%storage==rhs%storage
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal

  elemental logical function is_filled(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return storage status.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(container_abstract), intent(in) :: self !< The container.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_filled = self%is_filled_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_filled

  subroutine set(self, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Sets the content of the storage to value passed.
  !<
  !< The type of the variable provided must be the same as the container variable is designed to accept
  !< (as determined by the concrete implementation of the [[container:typeguard]] method in the extension)
  !< or be of the same type of container.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(container), intent(out) :: self    !< The container.
  class(*), intent(in)          :: content !< The content of the container.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (same_type_as(self, content)) then
    select type(content)
    class is(container)
      if (content%filled) then
        allocate(self%storage_, source=content%storage_)
        self%is_filled_ = .true.
      else
        self%is_filled_ = .false.
      endif
      return
    endselect
  endif
  if (self%typeguard(content)) then
    allocate(self%storage_, source=content)
    self%is_filled_ = .true.
  else
    write(stderr, '(A)') 'error: cannot assign given storage\ to this container'
#ifdef __GFORTRAN__
    call backtrace
#endif
    stop
  end if
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set
endmodule hasty_container_abstract
