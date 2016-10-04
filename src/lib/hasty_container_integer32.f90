!< HASTY concrete **container** class based on unlimited polymorphic storage for storing integer32 variable.
module hasty_container_integer32
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY concrete **container** class based on unlimited polymorphic storage for storing integer32 variable.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use hasty_container_adt_polymorphic
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: container_integer32
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(container_adt_polymorphic) :: container_integer32
  !< Concrete **container** class to storage integer32 contents by means of unlimited polymorphic storage.
  contains
    ! public methods
    procedure, pass(self) :: typeguard !< Check if the content is accepted by the container.
endtype container_integer32
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  elemental logical function typeguard(self, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the content is accepted by the container.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(container_integer32), intent(in) :: self    !< The container.
  class(*),                   intent(in) :: content !< The content of container.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  typeguard = .false.
  select type(content)
  type is (integer(I4P))
    typeguard = .true.
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction typeguard
endmodule hasty_container_integer32
