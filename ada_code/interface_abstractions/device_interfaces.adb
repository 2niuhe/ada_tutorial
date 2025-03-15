with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Device_Interfaces is
   -- 定义设备接口
   package Device_Interface is
      type Device is interface;
      
      -- 接口方法声明
      function Get_Name(Self : Device) return String is abstract;
      procedure Initialize(Self : in out Device) is abstract;
      procedure Send_Data(Self : in out Device; Data : String) is abstract;
      function Receive_Data(Self : Device) return String is abstract;
   end Device_Interface;

   -- 实现串口设备
   package Serial_Device is
      type Serial_Port is new Device_Interface.Device with private;
      
      -- 实现接口方法
      overriding function Get_Name(Self : Serial_Port) return String;
      overriding procedure Initialize(Self : in out Serial_Port);
      overriding procedure Send_Data(Self : in out Serial_Port; Data : String);
      overriding function Receive_Data(Self : Serial_Port) return String;
      
   private
      type Serial_Port is new Device_Interface.Device with record
         Port_Name : Unbounded_String;
         Is_Open : Boolean := False;
         Buffer : Unbounded_String;
      end record;
   end Serial_Device;

   -- 实现网络设备
   package Network_Device is
      type Network_Interface is new Device_Interface.Device with private;
      
      -- 实现接口方法
      overriding function Get_Name(Self : Network_Interface) return String;
      overriding procedure Initialize(Self : in out Network_Interface);
      overriding procedure Send_Data(Self : in out Network_Interface; Data : String);
      overriding function Receive_Data(Self : Network_Interface) return String;
      
   private
      type Network_Interface is new Device_Interface.Device with record
         IP_Address : Unbounded_String;
         Is_Connected : Boolean := False;
         Buffer : Unbounded_String;
      end record;
   end Network_Device;

   -- 串口设备实现
   package body Serial_Device is
      overriding function Get_Name(Self : Serial_Port) return String is
      begin
         return To_String(Self.Port_Name);
      end Get_Name;

      overriding procedure Initialize(Self : in out Serial_Port) is
      begin
         Self.Port_Name := To_Unbounded_String("/dev/ttyUSB0");
         Self.Is_Open := True;
         Put_Line("Serial port initialized: " & To_String(Self.Port_Name));
      end Initialize;

      overriding procedure Send_Data(Self : in out Serial_Port; Data : String) is
      begin
         if Self.Is_Open then
            Put_Line("Sending data via serial port: " & Data);
            Self.Buffer := To_Unbounded_String(Data);
         else
            raise Program_Error with "Serial port not initialized";
         end if;
      end Send_Data;

      overriding function Receive_Data(Self : Serial_Port) return String is
      begin
         if Self.Is_Open then
            return To_String(Self.Buffer);
         else
            raise Program_Error with "Serial port not initialized";
         end if;
      end Receive_Data;
   end Serial_Device;

   -- 网络设备实现
   package body Network_Device is
      overriding function Get_Name(Self : Network_Interface) return String is
      begin
         return To_String(Self.IP_Address);
      end Get_Name;

      overriding procedure Initialize(Self : in out Network_Interface) is
      begin
         Self.IP_Address := To_Unbounded_String("192.168.1.100");
         Self.Is_Connected := True;
         Put_Line("Network interface initialized: " & To_String(Self.IP_Address));
      end Initialize;

      overriding procedure Send_Data(Self : in out Network_Interface; Data : String) is
      begin
         if Self.Is_Connected then
            Put_Line("Sending data via network: " & Data);
            Self.Buffer := To_Unbounded_String(Data);
         else
            raise Program_Error with "Network interface not initialized";
         end if;
      end Send_Data;

      overriding function Receive_Data(Self : Network_Interface) return String is
      begin
         if Self.Is_Connected then
            return To_String(Self.Buffer);
         else
            raise Program_Error with "Network interface not initialized";
         end if;
      end Receive_Data;
   end Network_Device;

   -- 创建设备实例
   Serial : Serial_Device.Serial_Port;
   Network : Network_Device.Network_Interface;

begin
   Put_Line("Testing device interfaces...");
   
   -- 初始化设备
   Serial.Initialize;
   Network.Initialize;
   
   -- 测试数据发送和接收
   Serial.Send_Data("Hello via Serial!");
   Network.Send_Data("Hello via Network!");
   
   Put_Line("Received from Serial: " & Serial.Receive_Data);
   Put_Line("Received from Network: " & Network.Receive_Data);
   
   Put_Line("Device interface demonstration completed.");
end Device_Interfaces;
