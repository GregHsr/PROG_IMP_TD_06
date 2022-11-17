module m_type
    implicit none
    
    type phys
        real :: L, H, U0, D, C0, C1, R1, Cl, Cr, Ct, Cb, tf
    end type phys

    type num
        integer :: nx, ny, Nout
        real :: CFL, R
    end type num

end module m_type