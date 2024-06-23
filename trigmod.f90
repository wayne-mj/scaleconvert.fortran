module trigmod
    implicit none
    real, parameter :: pi = 4 * atan(1.0)

    ! Define a type to store x and y coordinates for plotting
    type :: plotXY
        real :: x,y
    end type plotXY

    ! Define a type to store the parameters of a helix
    type :: helixtype
        real :: height
        real :: gradient
        real :: degree
        real :: radius
        real :: clearence
        real :: supports
    end type helixtype

    contains 

    ! Find the adjacent side of a right triangle given the hypotenuse and angle in degrees
    function adj_hyp_ang_deg(hyp,deg) result(adj)
        real, intent(in) :: hyp, deg
        real :: adj

        adj = hyp * cos(deg * pi / 180.0)
    end function adj_hyp_ang_deg

    ! Find the opposite side of a right triangle given the hypotenuse and angle in degrees
    function opp_hyp_ang_deg(hyp,deg) result(opp)
        real, intent(in) :: hyp, deg
        real :: opp

        opp = hyp * sin(deg * pi / 180.0)
    end function opp_hyp_ang_deg

    ! Find the hypotenuse of a right triangle given the opposite side and angle in degrees
    function hyp_adj_ang_deg(adj,deg) result(hyp)
        real, intent(in) :: adj, deg
        real :: hyp

        hyp = adj / cos(deg * pi / 180.0)
    end function hyp_adj_ang_deg
end module trigmod