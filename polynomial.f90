program polynomial
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
    integer :: i,j,m,r=1,n
    doubleprecision :: add
    doubleprecision, dimension(5) :: x,y
    doubleprecision, allocatable, dimension(:,:) :: matrix
    doubleprecision, allocatable, dimension(:) :: ans
    print*, 'enter no. of polynomial degree'
    read*, n
    x=(/1891,1901,1911,1921,1931/)
    y=(/46,66,81,93,101/)
    allocate(matrix(n+1,n+2))
    allocate(ans(n+1))
    do i=1,n+1
        r=i-1
        do j=1,n+1
            do m=1,size(x)
                add = add + (x(m)**r)
            end do
            matrix(i,j) = add
            r=r+1
            add = 0
        end do
    end do
    add = 0
    r = 0
    do i=1,n+1
        do m=1,size(x)
            add = add + (y(m)*(X(m)**r))
        end do
        matrix(i,n+2) = add
        add = 0
        r = r+1
    end do

    call transform(matrix,n+1)
    n = n+1
    j = n
    ans = cal_unknown(matrix,n)
    do i=1,j
        print*, ans(i),"* x ^",(i-1)
    end do
    deallocate(matrix)
    deallocate(ans)
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
