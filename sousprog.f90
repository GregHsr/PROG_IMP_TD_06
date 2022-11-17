subroutine read_data (filename, data_phys, data_num)
    use m_type

    implicit none
    
    character(len=*), intent(in) :: filename

    type (phys), intent(out) :: data_phys
    type (num), intent(out) :: data_num

    open (unit=10, file=filename)
    read (10, *)
    read (10, *)

    read (10, *) data_phys%L, data_phys%H
    read (10, *) data_phys%U0
    read (10, *) data_phys%D
    read (10, *) data_phys%C0, data_phys%C1
    read (10, *) data_phys%R1
    read (10, *) data_phys%Cl, data_phys%Cr, data_phys%Ct, data_phys%Cb
    read (10, *) data_phys%tf

    read (10, *) 
    read (10, *)
    read (10, *) 

    read (10, *) data_num%nx, data_num%ny
    read (10, *) data_num%Nout
    read (10, *) data_num%CFL
    read (10, *) data_num%R

    close (10)

end subroutine read_data

subroutine mesh(Liste, longueur, n)
    use m_type

    implicit none 

    real, dimension(n), intent(inout):: Liste
    integer, intent(in):: n
    real, intent(in):: longueur
    integer:: i
    
    do i = 1, n
        Liste(i)=longueur*(i-1)/(n-1)
    end do

end subroutine mesh

function distance(x,y,L,H)
    real:: x, y, L, H, distance
    distance = sqrt((x-L/2)**2+(y-H/2)**2)
end function distance

subroutine C_init(C,x_reg,y_reg,C0,C1,R1,L,H,nx,ny)
    implicit none
    real, dimension(nx,ny), intent(inout):: C
    real, intent(in):: C0,C1,R1,L,H
    real, dimension(nx), intent(in):: x_reg 
    real, dimension(ny), intent(in):: y_reg
    real:: distance 
    integer, intent(in):: nx,ny
    integer:: i,j
    real:: d1,delta

    do i=1,nx
        do j=1,ny
            d1=distance(x_reg(i),y_reg(j),L,H)
            delta=2*min(L/(nx-1),H/(ny-1))/3
            C(i,j)=C1+(C0-C1)*(1+erf((D1-R1)/delta))/2
        end do
    end do
end subroutine C_init