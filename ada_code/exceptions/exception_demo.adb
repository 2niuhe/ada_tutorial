-- exception_demo.adb
-- Ada异常处理示例

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Exceptions;      use Ada.Exceptions;

procedure Exception_Demo is
   -- 自定义异常
   Invalid_Age : exception;
   
   -- 检查年龄的过程
   procedure Check_Age(Age : Integer) is
   begin
      if Age < 0 or Age > 150 then
         -- 抛出自定义异常
         raise Invalid_Age with "年龄必须在0到150之间";
      end if;
      Put_Line("年龄 " & Integer'Image(Age) & " 是有效的");
   end Check_Age;
   
   -- 除法函数，可能引发预定义异常
   function Safe_Division(A, B : Integer) return Float is
   begin
      return Float(A) / Float(B);
   end Safe_Division;
   
   -- 数组索引示例
   type Int_Array is array (1 .. 5) of Integer;
   Numbers : Int_Array := (1, 2, 3, 4, 5);
   Result : Float;
   
begin
   Put_Line("===== Ada异常处理示例 =====");
   Put_Line("");
   
   -- 示例1：处理自定义异常
   Put_Line("示例1：处理自定义异常");
   begin
      Check_Age(30);    -- 正常情况
      Check_Age(-5);    -- 将引发异常
   exception
      when E : Invalid_Age =>
         Put_Line("错误：" & Exception_Message(E));
   end;
   
   Put_Line("");
   
   -- 示例2：处理预定义异常（除以零）
   Put_Line("示例2：处理预定义异常（除以零）");
   begin
      Result := Safe_Division(10, 0);
      Put("结果 = "); 
      Put(Result, Fore => 1, Aft => 2, Exp => 0);
      New_Line;
   exception
      when Constraint_Error =>
         Put_Line("错误：试图除以零");
   end;
   
   Put_Line("");
   
   -- 示例3：处理多个异常
   Put_Line("示例3：处理多个异常");
   begin
      -- 尝试访问数组越界
      Put(Numbers(10));
   exception
      when Constraint_Error =>
         Put_Line("错误：数组索引越界");
      when others =>
         Put_Line("发生了未预期的错误");
   end;
   
   Put_Line("");
   
   -- 示例4：重新抛出异常
   Put_Line("示例4：重新抛出异常");
   begin
      begin
         Check_Age(200);
      exception
         when E : Invalid_Age =>
            Put_Line("内部处理器捕获到异常");
            raise; -- 重新抛出当前异常
      end;
   exception
      when E : Invalid_Age =>
         Put_Line("外部处理器捕获到重新抛出的异常：" & Exception_Message(E));
   end;
   
   Put_Line("");
   
   -- 示例5：异常处理中的清理操作
   Put_Line("示例5：异常处理中的清理操作");
   declare
      File : File_Type;
   begin
      -- 创建一个临时文件
      Create(File, Out_File, "temp.txt");
      Put_Line(File, "这是一个测试文件");
      -- 故意引发一个异常
      raise Program_Error with "模拟错误";
   exception
      when E : others =>
         -- 确保文件被关闭
         if Is_Open(File) then
            Close(File);
         end if;
         Put_Line("文件已安全关闭，原始错误：" & Exception_Message(E));
   end;
   
   Put_Line("");
   Put_Line("异常处理示例结束");
   
end Exception_Demo;
