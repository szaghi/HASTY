!< HASTY test hash table homogeneous content failure.
program hasty_test_hash_table_homocontent_failure
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY test hash table homogeneous content failure.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : int32
use hasty
use tester
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(tester_t)   :: hasty_tester !< Tests handler.
type(hash_table) :: a_table      !< A table.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
call a_table%initialize(homogeneous=.true., typeguard_key='a string', typeguard_content=1_int32)
call a_table%add_clone(key='first key', content='a string') ! An error is raised (if all go rigth)

call hasty_tester%init
call hasty_tester%assert_equal(len(a_table), 1)
call hasty_tester%print
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram hasty_test_hash_table_homocontent_failure
