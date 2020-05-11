program integration
    implicit none
    interface ! interface begins
        function areatrap(side1,side2,h)
            real , intent(in) :: side1, side2, h
        end function
        function calwidth(upp,low,div)
            real, intent(in) :: upp,low,div
        end function
        function calculatefuction(x)
            real , intent(in):: x
        end function
    end interface
    integer :: i !declaring variables
    real :: a,b,h,area,subarea,div
    real, dimension(:), allocatable ::x,y
    print*, 'Enter the lower limit'
    read*, a
    print*, 'Enter the upper limit'
    read*, b
    print*, 'enter the no. of divisions'
    read*, div
    allocate(x(int(div+1)))
    allocate(y(int(div+1)))
    h = calwidth(b,a,div) ! calculating the width of each interval based on no. of divisions
    do i=0,(int(div))
        x(i+1) = a+(i*h) !assigning x axis's values
        y(i+1) = calculatefuction(x(i+1)) !corresponding y axis value's
    end do
    do i=1,int(div)
        subarea=areatrap(y(i),y(i+1),h) !calculating area  of trapezium
        area = area+subarea !adding all the areas
    end do
    deallocate(x,y)
    print*,'the definite integral is', area
end program

function areatrap(side1,side2,h) !function to find the area of one trapezia
    real , intent(in) :: side1, side2, h
    areatrap = ((side1+side2)/2.0)*h
end function

function calwidth(upp,low,div) ! function to calculate width
    real, intent(in) :: upp,low,div
    calwidth = (upp-low)/div
end function

function calculatefuction(x) ! THE ACCTUAL FUNCTION WHICH YOU WANT TO INTEGRATE
    real , intent(in):: x
    calculatefuction = sqrt(x-1)
end function

