-- data_types.adb
-- Ada数据类型示例
-- Ada是一种强类型语言，拥有丰富的数据类型系统

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;

procedure Data_Types is
   -- 使用Text_IO包简化输入输出
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Ada.Float_Text_IO;
   
   -- 基本数据类型声明
   -- 整数类型
   I : Integer := 42;                -- 标准整数
   N : Natural := 10;                -- 自然数（0及正整数）
   P : Positive := 5;                -- 正整数（不包括0）
   
   -- 浮点类型
   F : Float := 3.14159;             -- 单精度浮点数
   LF : Long_Float := 3.14159_26535; -- 双精度浮点数
   
   -- 字符和字符串
   C : Character := 'A';             -- 单个字符
   -- 注意：在声明固定长度字符串时，长度必须与初始值精确匹配
   S : String(1 .. 11) := "Hello, Ada!";  -- 正好11个字符（包括逗号、空格和感叹号）
   
   -- 布尔类型
   B : Boolean := True;              -- 布尔值（True或False）
   
   -- Ada允许自定义类型
   -- 子类型（约束现有类型）
   subtype Small_Integer is Integer range -100 .. 100;
   SI : Small_Integer := 50;
   
   -- 枚举类型
   type Day_Type is (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
   Today : Day_Type := Wednesday;
   
   -- 数组类型
   type Int_Array is array (1 .. 5) of Integer;
   My_Array : Int_Array := (10, 20, 30, 40, 50);
   
   -- 记录类型（类似于C的结构体）
   type Person is record
      Name : String (1 .. 4);  -- 修改为更简单的名字长度
      Age  : Natural;
   end record;
   
   -- 初始化记录
   Someone : Person := (
      Name => "Lisa",  -- 正好4个字符
      Age  => 30
   );
   
begin
   -- 输出基本类型的值
   Put_Line("===== 基本数据类型 =====");
   Put("整数 I = "); Put(I); New_Line;
   Put("自然数 N = "); Put(N); New_Line;
   Put("正整数 P = "); Put(P); New_Line;
   
   Put("浮点数 F = "); Put(F, Fore => 1, Aft => 5, Exp => 0); New_Line;
   Put("长浮点数 LF = "); 
   -- 使用Long_Float_IO包处理Long_Float类型
   declare
      package Long_Float_IO is new Ada.Text_IO.Float_IO(Long_Float);
      use Long_Float_IO;
   begin
      Put(LF, Fore => 1, Aft => 10, Exp => 0); 
   end;
   New_Line;
   
   Put_Line("字符 C = " & C);
   Put_Line("字符串 S = " & S);
   
   Put_Line("布尔值 B = " & Boolean'Image(B));
   
   -- 输出自定义类型的值
   Put_Line("");
   Put_Line("===== 自定义类型 =====");
   Put_Line("小整数 SI = " & Small_Integer'Image(SI));
   Put_Line("今天是 " & Day_Type'Image(Today));
   
   -- 输出数组
   Put_Line("");
   Put_Line("===== 数组 =====");
   for I in My_Array'Range loop
      Put("My_Array(" & Integer'Image(I) & ") = ");
      Put(My_Array(I));
      New_Line;
   end loop;
   
   -- 输出记录
   Put_Line("");
   Put_Line("===== 记录 =====");
   Put_Line("姓名: " & Someone.Name);
   Put("年龄: "); Put(Someone.Age); New_Line;
   
   -- 类型转换
   Put_Line("");
   Put_Line("===== 类型转换 =====");
   Put_Line("整数转字符串: " & Integer'Image(I));
   Put_Line("浮点数转字符串: " & Float'Image(F));
   
   -- 类型属性
   Put_Line("");
   Put_Line("===== 类型属性 =====");
   Put_Line("Integer类型的最小值: " & Integer'Image(Integer'First));
   Put_Line("Integer类型的最大值: " & Integer'Image(Integer'Last));
   Put_Line("Float类型的精度: " & Integer'Image(Float'Digits));
   
end Data_Types;
