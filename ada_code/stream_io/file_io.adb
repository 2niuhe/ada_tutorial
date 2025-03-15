-- file_io.adb
-- Ada高级特性演示：流式I/O和存储管理

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Streams;         use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;      use Ada.Exceptions;

procedure File_IO is
   -- 定义一个复杂的记录类型
   type Person;
   type Person_Access is access Person;
   
   type Person is record
      ID        : Integer;
      Name      : Unbounded_String;
      Age       : Integer;
      Next      : Person_Access;
   end record;
   
   -- 为Person类型定义流属性
   procedure Write(Stream : not null access Root_Stream_Type'Class;
                  Item   : in Person);
   for Person'Write use Write;
   
   procedure Read(Stream : not null access Root_Stream_Type'Class;
                 Item   : out Person);
   for Person'Read use Read;
   
   -- 实现流操作
   procedure Write(Stream : not null access Root_Stream_Type'Class;
                  Item   : in Person) is
   begin
      Integer'Write(Stream, Item.ID);
      Unbounded_String'Write(Stream, Item.Name);
      Integer'Write(Stream, Item.Age);
   end Write;
   
   procedure Read(Stream : not null access Root_Stream_Type'Class;
                 Item   : out Person) is
   begin
      Integer'Read(Stream, Item.ID);
      Unbounded_String'Read(Stream, Item.Name);
      Integer'Read(Stream, Item.Age);
      Item.Next := null;
   end Read;
   
   -- 内存管理
   procedure Free is new Ada.Unchecked_Deallocation(Person, Person_Access);
   
   -- 创建一个链表并写入文件
   procedure Create_And_Save_List(Filename : String) is
      File   : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;
      Head   : Person_Access := null;
      Current : Person_Access;
      Count   : Integer := 0;
   begin
      Put_Line("创建人员列表...");
      
      -- 创建一些测试数据
      for I in 1..5 loop
         Current := new Person'(
            ID   => I,
            Name => To_Unbounded_String("Person_" & Integer'Image(I)),
            Age  => 20 + I,
            Next => null
         );
         
         if Head = null then
            Head := Current;
         else
            Current.Next := Head;
            Head := Current;
         end if;
         
         Count := Count + 1;
      end loop;
      
      -- 打开文件进行写入
      Ada.Streams.Stream_IO.Create(File, Ada.Streams.Stream_IO.Out_File, Filename);
      Stream := Ada.Streams.Stream_IO.Stream(File);
      
      -- 写入记录数量
      Integer'Write(Stream, Count);
      
      -- 写入所有记录
      Current := Head;
      while Current /= null loop
         Person'Write(Stream, Current.all);
         Current := Current.Next;
      end loop;
      
      -- 清理内存并关闭文件
      Current := Head;
      while Current /= null loop
         Head := Current.Next;
         Free(Current);
         Current := Head;
      end loop;
      
      Ada.Streams.Stream_IO.Close(File);
      Put_Line("数据已保存到文件: " & Filename);
   end Create_And_Save_List;
   
   -- 从文件读取链表
   procedure Load_And_Display_List(Filename : String) is
      File    : Ada.Streams.Stream_IO.File_Type;
      Stream  : Ada.Streams.Stream_IO.Stream_Access;
      Count   : Integer;
      Person_Data : Person;
   begin
      Put_Line("从文件读取数据...");
      
      -- 打开文件进行读取
      Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, Filename);
      Stream := Ada.Streams.Stream_IO.Stream(File);
      
      -- 读取记录数量
      Integer'Read(Stream, Count);
      Put_Line("记录数量: " & Integer'Image(Count));
      New_Line;
      
      -- 读取并显示所有记录
      for I in 1..Count loop
         Person'Read(Stream, Person_Data);
         
         Put_Line("记录 #" & Integer'Image(I));
         Put_Line("  ID: " & Integer'Image(Person_Data.ID));
         Put_Line("  姓名: " & To_String(Person_Data.Name));
         Put_Line("  年龄: " & Integer'Image(Person_Data.Age));
         New_Line;
      end loop;
      
      Ada.Streams.Stream_IO.Close(File);
   end Load_And_Display_List;
   
   -- 主程序
   Filename : constant String := "people.dat";
   
begin
   Put_Line("===== Ada高级特性演示：流式I/O和存储管理 =====");
   New_Line;
   
   -- 创建并保存数据
   Create_And_Save_List(Filename);
   New_Line;
   
   -- 读取并显示数据
   Load_And_Display_List(Filename);
   
   Put_Line("演示完成");
   
exception
   when Ada.Streams.Stream_IO.Name_Error =>
      Put_Line("错误：无法访问文件 " & Filename);
   when Ada.Streams.Stream_IO.Data_Error =>
      Put_Line("错误：文件格式不正确");
   when E : others =>
      Put_Line("发生未知错误：");
      Put_Line(Exception_Message(E));
end File_IO;
