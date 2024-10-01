program gaussian_elimination
    implicit none
    integer :: i, j, k, n
    real :: A(10, 10), b(10), x(10), ratio

    ! ANSI Escape Codes for colors
    character(len=*), parameter :: reset = char(27) // '[0m'
    character(len=*), parameter :: red = char(27) // '[31m'
    character(len=*), parameter :: green = char(27) // '[32m'
    character(len=*), parameter :: blue = char(27) // '[34m'
    character(len=*), parameter :: cyan = char(27) // '[36m'

    ! Number of equations
    n = 3

    ! Coefficient matrix
    A(1, 1) = 2.0
    A(1, 2) = -1.0
    A(1, 3) = 1.0
    A(2, 1) = 3.0
    A(2, 2) = 3.0
    A(2, 3) = 9.0
    A(3, 1) = 3.0
    A(3, 2) = 3.0
    A(3, 3) = 5.0

    ! Right-hand side vector
    b(1) = 8.0
    b(2) = 0.0
    b(3) = 2.0

    ! Printing the matrix and vector with colors
    print *, cyan // "Coefficient matrix (A):" // reset
    do i = 1, n
        do j = 1, n
            print *, green, A(i, j), reset,
        end do
        print *, ""
    end do

    print *, cyan // "Right-hand side vector (b):" // reset
    do i = 1, n
        print *, blue, b(i), reset
    end do

    ! Gaussian Elimination
    do i = 1, n - 1
        do j = i + 1, n
            ratio = A(j, i) / A(i, i)
            do k = 1, n
                A(j, k) = A(j, k) - ratio * A(i, k)
            end do
            b(j) = b(j) - ratio * b(i)
        end do
    end do

    ! Back substitution
    x(n) = b(n) / A(n, n)
    do i = n - 1, 1, -1
        x(i) = b(i)
        do j = i + 1, n
            x(i) = x(i) - A(i, j) * x(j)
        end do
        x(i) = x(i) / A(i, i)
    end do

    ! Display the solution vector with colors
    print *, cyan // "Solution vector:" // reset
    do i = 1, n
        print *, red // "x(", i, ") = ", x(i), reset
    end do

end program gaussian_elimination
