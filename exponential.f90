program linearFitting
    implicit none
    integer :: i,j
    doubleprecision, dimension(5) :: x,y
    doubleprecision :: a,b,addx=0,addy=0,addxsq=0,der=0,derx=0,dery=0,addxy=0,addi=0
    doubleprecision, allocatable, dimension(:,:) :: matrix
    x = (/1,2,3,4,5/)
    y = (/2.37,10.58,34.67,46.3,70.3/)
    do i=1,5
        addx = addx + x(i)
        addy = addy + log(y(i))
        addxy = addxy + (x(i)*log(y(i)))
        addxsq = addxsq + (x(i)**2)
    end do
    addi = i-1
    allocate(matrix(2,2))
    matrix(1,1) = addxsq
    matrix(1,2) = addx
    matrix(2,1) = addx
    matrix(2,2) = addi

    der = (matrix(1,1)*matrix(2,2))-(matrix(1,2)*matrix(2,1))

    matrix(1,1) = addxy
    matrix(1,2) = addx
    matrix(2,1) = addy
    matrix(2,2) = addi

    derx = (matrix(1,1)*matrix(2,2))-(matrix(1,2)*matrix(2,1))

    matrix(1,1) = addxsq
    matrix(1,2) = addxy
    matrix(2,1) = addx
    matrix(2,2) = addy

    dery = (matrix(1,1)*matrix(2,2))-(matrix(1,2)*matrix(2,1))
    deallocate(matrix)
    b = exp(derx/der)
    a = exp(dery/der)
    print*, 'y=',a,'* b^', b
end program
