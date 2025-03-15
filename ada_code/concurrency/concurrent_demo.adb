-- concurrent_demo.adb
-- Ada任务和并发编程示例

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Calendar;        use Ada.Calendar;
with Ada.Exceptions;      use Ada.Exceptions;

procedure Concurrent_Demo is
   -- 缓冲区数组类型
   type Buffer_Array is array(Positive range <>) of Integer;
   
   -- 受保护对象：共享缓冲区
   protected type Buffer(Size : Positive) is
      entry Put(Item : Integer);
      entry Get(Item : out Integer);
      function Is_Empty return Boolean;
      function Is_Full return Boolean;
   private
      Data    : Buffer_Array(1 .. Size);
      In_Idx  : Positive := 1;
      Out_Idx : Positive := 1;
      Count   : Natural  := 0;
   end Buffer;
   
   protected body Buffer is
      entry Put(Item : Integer) when Count < Size is
      begin
         Data(In_Idx) := Item;
         In_Idx := (In_Idx mod Size) + 1;
         Count := Count + 1;
         Put_Line("生产者放入: " & Integer'Image(Item));
      end Put;
      
      entry Get(Item : out Integer) when Count > 0 is
      begin
         Item := Data(Out_Idx);
         Out_Idx := (Out_Idx mod Size) + 1;
         Count := Count - 1;
         Put_Line("消费者取出: " & Integer'Image(Item));
      end Get;
      
      function Is_Empty return Boolean is
      begin
         return Count = 0;
      end Is_Empty;
      
      function Is_Full return Boolean is
      begin
         return Count = Size;
      end Is_Full;
   end Buffer;
   
   -- 生产者任务类型
   task type Producer(ID : Positive; Buf : access Buffer);
   
   task body Producer is
      Next_Item : Integer := ID * 100;  -- 每个生产者从不同的起始值开始
   begin
      Put_Line("生产者" & Integer'Image(ID) & "启动");
      for I in 1 .. 5 loop
         delay 0.5;  -- 模拟生产时间
         Buf.Put(Next_Item);
         Next_Item := Next_Item + 1;
      end loop;
      Put_Line("生产者" & Integer'Image(ID) & "完成");
   exception
      when E : others =>
         Put_Line("生产者" & Integer'Image(ID) & "错误: " & Exception_Message(E));
   end Producer;
   
   -- 消费者任务类型
   task type Consumer(ID : Positive; Buf : access Buffer);
   
   task body Consumer is
      Item : Integer;
   begin
      Put_Line("消费者" & Integer'Image(ID) & "启动");
      for I in 1 .. 5 loop
         delay 1.0;  -- 模拟消费时间
         Buf.Get(Item);
      end loop;
      Put_Line("消费者" & Integer'Image(ID) & "完成");
   exception
      when E : others =>
         Put_Line("消费者" & Integer'Image(ID) & "错误: " & Exception_Message(E));
   end Consumer;
   
   -- 计时器任务
   task Timer;
   
   task body Timer is
      Start_Time : Time := Clock;
   begin
      for I in 1 .. 10 loop
         delay 1.0;
         Put_Line("时间已过去:" & Duration'Image(Clock - Start_Time) & "秒");
      end loop;
   end Timer;
   
   -- 主程序变量
   Shared_Buffer : aliased Buffer(5);  -- 大小为5的共享缓冲区
   P1 : Producer(1, Shared_Buffer'Access);
   P2 : Producer(2, Shared_Buffer'Access);
   C1 : Consumer(1, Shared_Buffer'Access);
   C2 : Consumer(2, Shared_Buffer'Access);
   
begin
   Put_Line("===== Ada任务和并发编程示例 =====");
   Put_Line("");
   Put_Line("程序启动，创建了2个生产者和2个消费者");
   Put_Line("每个生产者会生产5个项目，每个消费者会消费5个项目");
   Put_Line("缓冲区大小为5");
   Put_Line("");
   
   -- 主程序等待所有任务完成
   null;
   
end Concurrent_Demo;
