with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Exceptions; use Ada.Exceptions;

procedure Main is

   Group : constant String := "239.255.128.128";
   --  Multicast group: administratively scoped IP address

   task Hub is
      entry Start;
      entry Stop;
   end Hub;

   task body Hub is
      Address  : Sock_Addr_Type;
      Server   : Socket_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;

   begin
      --  Get an Internet address of a host (here the local host name).
      --  Note that a host can have several addresses. Here we get
      --  the first one which is supposed to be the official one.

      Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);

      --  Get a socket address that is an Internet address and a port

      Address.Port := 5876;

      --  The first step is to create a socket. Once created, this
      --  socket must be associated to with an address. Usually only a
      --  server (Pong here) needs to bind an address explicitly. Most
      --  of the time clients can skip this step because the socket
      --  routines will bind an arbitrary address to an unbound socket.

      Create_Socket (Server);

      --  Allow reuse of local addresses

      Set_Socket_Option
        (Server,
         Socket_Level,
         (Reuse_Address, True));

      Bind_Socket (Server, Address);

      --  A server marks a socket as willing to receive connect events

      Listen_Socket (Server);

      --  Once a server calls Listen_Socket, incoming connects events
      --  can be accepted. The returned Socket is a new socket that
      --  represents the server side of the connection. Server remains
      --  available to receive further connections.

      accept Start;

      Accept_Socket (Server, Socket, Address);

      --  Return a stream associated to the connected socket

      Channel := Stream (Socket);

      --  Force Pong to block

      delay 0.2;

      --  Receive and print message from client Ping

      declare
         Message : String := String'Input (Channel);

      begin
         Ada.Text_IO.Put_Line (Message);

         --  Send same message back to client Ping

         String'Output (Channel, Message);
      end;

      Close_Socket (Server);
      Close_Socket (Socket);

      --  Part of the multicast example

      --  Create a datagram socket to send connectionless, unreliable
      --  messages of a fixed maximum length.

      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      --  Allow reuse of local addresses

      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Reuse_Address, True));

      --  Controls the live time of the datagram to avoid it being
      --  looped forever due to routing errors. Routers decrement
      --  the TTL of every datagram as it traverses from one network
      --  to another and when its value reaches 0 the packet is
      --  dropped. Default is 1.

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_TTL, 1));

      --  Want the data you send to be looped back to your host

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_Loop, True));

      --  If this socket is intended to receive messages, bind it
      --  to a given socket address.

      Address.Addr := Any_Inet_Addr;
      Address.Port := 55505;

      Bind_Socket (Socket, Address);

      --  Join a multicast group

      --  Portability note: On Windows, this option may be set only
      --  on a bound socket.

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Add_Membership, Inet_Addr (Group), Any_Inet_Addr));

      --  If this socket is intended to send messages, provide the
      --  receiver socket address.

      Address.Addr := Inet_Addr (Group);
      Address.Port := 55506;

      Channel := Stream (Socket, Address);

      --  Receive and print message from client Ping

      declare
         Message : String := String'Input (Channel);

      begin
         --  Get the address of the sender

         Address := Get_Address (Channel);
         Ada.Text_IO.Put_Line (Message & " from " & Image (Address));

         --  Send same message back to client Ping

         String'Output (Channel, Message);
      end;

      Close_Socket (Socket);

      accept Stop;

   exception when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));

   end Hub;

begin
   Hub.Start;
   Hub.Stop;
   Put_Line ("Hello World");
end Main;
