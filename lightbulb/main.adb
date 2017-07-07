with Ada.Text_IO; use Ada.Text_IO;
with Gnat.Sockets; use Gnat.Sockets;
with Ada.Exceptions; use Ada.Exceptions;

procedure Main is
   Group : constant String := "239.255.128.128";
   --  Multicast group: administratively scoped IP address


   task Ping is
      entry Start;
      entry Stop;
   end Ping;

   task body Ping is
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;

   begin
      accept Start;

      --  See comments in Ping section for the first steps

      Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
      Address.Port := 5876;
      Create_Socket (Socket);

      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Reuse_Address, True));

      --  Force Ping to block

      delay 0.2;

      --  If the client's socket is not bound, Connect_Socket will
      --  bind to an unused address. The client uses Connect_Socket to
      --  create a logical connection between the client's socket and
      --  a server's socket returned by Accept_Socket.

      Connect_Socket (Socket, Address);

      Channel := Stream (Socket);

      --  Send message to server Pong

      String'Output (Channel, "Hello world");

      --  Force Ping to block

      delay 0.2;

      --  Receive and print message from server Pong

      Ada.Text_IO.Put_Line (String'Input (Channel));
      Close_Socket (Socket);

      --  Part of multicast example. Code similar to Pong's one

      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Reuse_Address, True));

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_TTL, 1));

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_Loop, True));

      Address.Addr := Any_Inet_Addr;
      Address.Port := 55506;

      Bind_Socket (Socket, Address);

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Add_Membership, Inet_Addr (Group), Any_Inet_Addr));

      Address.Addr := Inet_Addr (Group);
      Address.Port := 55505;

      Channel := Stream (Socket, Address);

      --  Send message to server Pong

      String'Output (Channel, "Hello world");

      --  Receive and print message from server Pong

      declare
         Message : String := String'Input (Channel);

      begin
         Address := Get_Address (Channel);
         Ada.Text_IO.Put_Line (Message & " from " & Image (Address));
      end;

      Close_Socket (Socket);

      accept Stop;

   exception when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
   end Ping;

begin

   Ping.Start;
   Ping.Stop;

   --
--     Lightbulb.Initialize;
--
--     loop
--        Lightbulb.Execute;
--     end loop;

end Main;
