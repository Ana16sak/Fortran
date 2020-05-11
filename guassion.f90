program gaussian
    implicit none
    interface
        subroutine transform(matrix,n)
            doubleprecision, intent(inout), dimension(n,n+1) :: matrix
            doubleprecision :: sub,multi
            integer, intent(in) :: n
        end subroutine
        function cal_unknown(matrix,n)result(ans)
            integer, intent(inout) :: n
            doubleprecision, intent(inout), dimension(n,n+1) :: matrix
            doubleprecision , dimension(n) :: ans
            integer :: i,j
        end function
    end interface
    integer :: n,i=1,j,r
    doubleprecision , allocatable, dimension(:,:) :: matrix
    doubleprecision , allocatable, dimension(:) :: ans
    print*, 'enter the no. of variables'
    read*, n
    allocate(ans(n))
    allocate(matrix(n,n+1))
    r=n
    do i=1,n
        print*,'enter the coefficients of equation ', i
        do j=1,n
            read*,matrix(i,j)
        end do
        print*, 'enter the constant (RHS)'
        read*,matrix(i,j)
    end do
    call transform(matrix,n)
    ans = cal_unknown(matrix,n)
    do i=1,r
        print*, ans(i)
    end do
end program


subroutine transform(matrix,n)
    doubleprecision, intent(inout), dimension(n,n+1) :: matrix
    doubleprecision :: sub,multi
    integer, intent(in) :: n
    do i=1,n
        do j=i+1,n
            multi = matrix(j,i)/matrix(i,i)
            do m=i,n+1
                sub = matrix(i,m)*multi
                matrix(j,m) = matrix(j,m) - sub
            end do
        end do
    end do
end subroutine

function cal_unknown(matrix,n)result(ans)
    implicit none
    integer, intent(inout) :: n
    doubleprecision, intent(inout), dimension(n,n+1) :: matrix
    doubleprecision ,dimension(n) :: ans
    integer :: i,j,r=1
    i = n+1
    do while(n>0)
        ans(n) = matrix(n,i)/matrix(n,n)
        do j=1,n-1
            matrix(j,i) = matrix(j,i) - ans(n)*matrix(j,i-r)
        end do
        r=r+1
        n = n-1
    end do
end function
