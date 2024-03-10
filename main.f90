program main
    use iso_c_binding
    implicit none

    integer :: status
    type(c_ptr) :: session, channel
    character(len=100) :: host, username, password, command

    interface
        integer(c_int) function libssh2_init()
        end function libssh2_init

        integer(c_int) function libssh2_exit(session) bind(c)
            use iso_c_binding
            type(c_ptr), value :: session
        end function libssh2_exit
        
        type(c_ptr) function libssh2_session_init() bind(c)
        end function libssh2_session_init

        integer(c_int) function libssh2_session_handshake(session, sock) bind(c)
            use iso_c_binding
            type(c_ptr), value :: session
            integer(c_int), value :: sock
        end function libssh2_session_handshake

        integer(c_int) function libssh2_userauth_password(session, username, password) bind(c)
            use iso_c_binding
            type(c_ptr), value :: session
            character(c_char), intent(in) :: username, password
        end function libssh2_userauth_password

        type(c_ptr) function libssh2_channel_open_session(session) bind(c)
            use iso_c_binding
            type(c_ptr), value :: session
        end function libssh2_channel_open_session

        integer(c_int) function libssh2_channel_exec(channel, command) bind(c)
            use iso_c_binding
            type(c_ptr), value :: channel
            character(c_char), intent(in) :: command
        end function libssh2_channel_exec

        integer(c_int) function libssh2_channel_read(channel, buffer, buflen) bind(c)
            use iso_c_binding
            type(c_ptr), value :: channel
            type(c_ptr), value :: buffer
            integer(c_int), value :: buflen
        end function libssh2_channel_read

        integer(c_int) function libssh2_channel_close(channel) bind(c)
            use iso_c_binding
            type(c_ptr), value :: channel
        end function libssh2_channel_close
    end interface

    status = libssh2_init()
    if (status /= 0) then
        print *, "libssh2 init error"
        stop
    endif

    call get_command_argument(1, host)
    call get_command_argument(2, username)
    call get_command_argument(3, password)

    session = libssh2_session_init()

    status = libssh2_session_handshake(session, -1)
    if (status /= 0) then
        print *, "The connection couldn't be stablished"
        stop
    endif

    status = libssh2_userauth_password(session, c_char(username), c_char(password))
    if (status /= 0) then
        print *, "Invalid credentials"
        stop
    endif

    channel = libssh2_channel_open_session(session)

    do
        print *, "ssh> "
        read(*,*) command

        if (trim(command) == "exit") exit

        status = libssh2_channel_exec(channel, c_char(command))
        if (status /= 0) then
            print *, "Error"
            cycle
        endif

        character(len=1024) :: buffer
        status = libssh2_channel_read(channel, c_loc(buffer), 1024)
        print *, buffer(1:status)
    end do

    call libssh2_channel_close(channel)
    call libssh2_exit(session)
    call libssh2_exit()

end program main
