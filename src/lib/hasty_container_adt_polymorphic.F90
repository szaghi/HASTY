!< HASTY abstract **container** class based on unlimited polymorphic storage.
module hasty_container_adt_polymorphic
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY abstract **container** class based on unlimited polymorphic storage.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use hasty_container_adt
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: container_adt_polymorphic
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(container_adt), abstract :: container_adt_polymorphic
  !< Abstract **container** class to storage any contents by means of unlimited polymorphic storage.
  class(*), pointer :: storage_=>null()   !< Storage of containers' contents.
  logical           :: is_filled_=.false. !< Check if the container is set.
  contains
    ! public methods
    procedure, pass(self) :: content   !< Return the content of the container.
    procedure, pass(self) :: destroy   !< Destroy the container content.
    procedure, pass(self) :: is_filled !< Return storage status.
    procedure, pass(self) :: set       !< Set the content of the container.
    ! private methods
    procedure, pass(lhs), private :: is_equal !< Implement `==` operator.
endtype container_adt_polymorphic
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  elemental logical function is_filled(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return storage status.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(container_adt_polymorphic), intent(in) :: self !< The container.
  class(*), pointer :: storage_=>null()   !< Storage of containers' contents.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_filled = self%is_filled_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_filled

  elemental subroutine destroy(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the container content.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(container_adt_polymorphic), intent(inout) :: self !< The container.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(self%storage_)) deallocate(self%storage_)
  self%storage_ => null()
  self%is_filled_ = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy

  elemental logical function is_filled(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return storage status.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(container_adt_polymorphic), intent(in) :: self !< The container.
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
  class(container_adt_polymorphic), intent(out) :: self    !< The container.
  class(*),                         intent(in)  :: content !< The content of the container.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (same_type_as(self, content)) then
    select type(content)
    class is(container_adt_polymorphic)
      if (content%is_filled()) then
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
    write(stderr, '(A)') 'error: cannot assign given content to this container'
#ifdef __GFORTRAN__
    call backtrace
#endif
    stop
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  ! private methods
  elemental logical function is_equal(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Implement `==` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(container_adt_polymorphic), intent(in) :: lhs !< Left hand side.
  class(container_adt            ), intent(in) :: rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.same_type_as(lhs, rhs)) then
    is_equal = .false.
    return
  endif
  if ((.not.lhs%is_filled()).and.(.not.rhs%is_filled())) then
    is_equal = .true.
    return
  endif
  if (lhs%is_filled().neqv.rhs%is_filled()) then
    is_equal = .false.
    return
  endif
  ! TODO find the right way to equal two class(*) instances
  ! is_equal = .false.
  ! select type(rhs)
  ! class is(container_adt_polymorphic)
  !   is_equal = lhs%storage_==rhs%storage_
  ! endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal
endmodule hasty_container_adt_polymorphic
