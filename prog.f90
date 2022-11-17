program TD_06
    use m_type
    implicit none

    type (phys):: data_phys
    type (num):: data_num
    real, dimension(:), allocatable:: x_reg, y_reg
    real, dimension(:,:), allocatable:: C
    integer :: i, j, k, l

    call read_data("data_2D.txt", data_phys, data_num)
    write(*,*) data_phys, data_num
    allocate(x_reg(data_num%nx), y_reg(data_num%ny), C(data_num%nx, data_num%ny))

    do i=1,data_num%nx
        do j=1,data_num%ny
            C(i,j) = 0
        end do
    end do

    call mesh(x_reg, data_phys%L, data_num%nx)
    call mesh(y_reg, data_phys%H, data_num%ny)

    call C_init(C,x_reg,y_reg,data_phys%C0,data_phys%C1,data_phys%R1,data_phys%L,data_phys%H,data_num%nx,data_num%ny)

    do k=1,data_num%ny
        write(*,200) (C(l,k),l=1,data_num%nx)
    end do

    deallocate(x_reg, y_reg, C)

200 format(15(F5.2))
end program TD_06
