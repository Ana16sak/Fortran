program Newton_forward
    interface
        function FindTerms(u,term)
            doubleprecision, intent(in) :: u
            integer, intent(in) :: term
        end function
        function fac(n)
            integer, intent(in) :: n
        end function
        function delta(arr,base,term)
            doubleprecision, dimension(4,4), intent(in) :: arr ! change this to your data-set - 1
            integer, intent(in) :: base, term
            integer :: ranval
        end function
    end interface
    doubleprecision, dimension(5) :: x ! change the no. of datasets
    doubleprecision, dimension(5) :: y
    doubleprecision, dimension(4,4) :: matrix ! change this to your data-set - 1
    integer :: i,j,r = (size(y)-1), base
    doubleprecision :: n,term,u,d, approx = 0, maxn = 0
    x = (/0.1,0.2,0.3,0.4,0.5/)
    y = (/1.4,1.56,1.76,2.00,2.28/)
    do j = 1, r
        matrix(j,1) = y(j+1) - y(j)
    end do
    r = r - 1
    do i = 2,(size(y)-1)
        do j = 1, (size(y)-1)
            if(j > r)then
                matrix(j, i) = 150003
            else
                matrix(j, i) = matrix(j+1,i-1) - matrix(j,i-1)
            end if
        end do
        r = r - 1
    end do
    print*, "enter the interpolation value : "
    read*, n
    do i=1,size(x)
        if(maxn < x(i) .and. x(i) > n)then
            maxn = x(i)
            base = i-1
            exit
        end if
    end do
    u = (n - maxn)/(x(2)-x(1))

    print*, 'base = ',maxn
    do i = 1, size(x)
        term = FindTerms(u,i-1)
        d = delta(matrix,base,i)
        approx = approx + (d*term)
    end do
    approx = approx + y(base+1)
    print*,'interpolated ans = ', approx
end program

function FindTerms(u,term)
    doubleprecision, intent(in) :: u
    integer, intent(in) :: term
    FindTerms = 1.0
    do i=0,term
        FindTerms = FindTerms*(u+i)
    end do
    FindTerms = FindTerms/fac(term+1)
end function

function fac(n)
    integer, intent(in) :: n
    fac = 1
    if(n == 0)then
        fac = 1
    else
        do i = 1,n
            fac = fac * i
        end do
    end if
end function

function delta(arr,base,term)
    doubleprecision, dimension(4,4), intent(in) :: arr ! change this to your data-set - 1
    integer, intent(in) :: base, term
    integer :: ranval
    ranval = base
    do i=0,(size(arr)-1)
        if(arr(ranval,term) == 150003)then
            ranval = ranval - 1
        else
            delta = arr(ranval, term)
            exit
        end if
    end do
end function
