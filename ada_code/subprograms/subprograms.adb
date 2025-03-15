-- subprograms.adb
-- Ada子程序示例
-- 展示Ada中的过程（procedure）和函数（function）

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;

procedure Subprograms is
   -- 使用Text_IO包简化输入输出
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Ada.Float_Text_IO;
   
   -- 声明一个简单的过程（procedure）
   -- 过程是不返回值的子程序
   procedure Say_Hello is
   begin
      Put_Line("你好！这是一个简单的过程。");
   end Say_Hello;
   
   -- 带参数的过程
   -- in参数：只读参数（默认）
   -- out参数：只写参数
   -- in out参数：可读可写参数
   procedure Greet(Name : in String) is
   begin
      Put_Line("你好，" & Name & "！");
   end Greet;
   
   -- 带多个参数的过程
   procedure Add_And_Display(A, B : in Integer; Sum : out Integer) is
   begin
      Sum := A + B;
      Put("和为: "); Put(Sum); New_Line;
   end Add_And_Display;
   
   -- 带默认参数的过程
   procedure Print_Info(Name : in String; Age : in Integer := 30) is
   begin
      Put_Line(Name & "的年龄是" & Integer'Image(Age) & "岁");
   end Print_Info;
   
   -- 声明一个简单的函数（function）
   -- 函数是返回值的子程序
   function Square(X : Integer) return Integer is
   begin
      return X * X;
   end Square;
   
   -- 带多个参数的函数
   function Max(A, B : Integer) return Integer is
   begin
      if A > B then
         return A;
      else
         return B;
      end if;
   end Max;
   
   -- 递归函数
   function Factorial(N : Natural) return Positive is
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial(N - 1);
      end if;
   end Factorial;
   
   -- 函数重载（同名不同参数）
   function Average(A, B : Integer) return Float is
   begin
      return Float(A + B) / 2.0;
   end Average;
   
   function Average(A, B, C : Integer) return Float is
   begin
      return Float(A + B + C) / 3.0;
   end Average;
   
   -- 声明变量
   Result : Integer;
   Avg2, Avg3 : Float;
   
begin
   Put_Line("===== Ada子程序示例 =====");
   
   -- 调用简单过程
   Put_Line("");
   Put_Line("===== 调用过程 =====");
   Say_Hello;
   
   -- 调用带参数的过程
   Greet("张三");
   
   -- 调用带多个参数的过程
   Add_And_Display(10, 20, Result);
   
   -- 调用带默认参数的过程
   Print_Info("李四");           -- 使用默认年龄
   Print_Info("王五", 25);       -- 指定年龄
   
   -- 调用函数
   Put_Line("");
   Put_Line("===== 调用函数 =====");
   
   Result := Square(5);
   Put("5的平方是: "); Put(Result); New_Line;
   
   Result := Max(15, 7);
   Put("15和7中的最大值是: "); Put(Result); New_Line;
   
   Result := Factorial(5);
   Put("5的阶乘是: "); Put(Result); New_Line;
   
   -- 调用重载函数
   Avg2 := Average(10, 20);
   Put("10和20的平均值是: "); Put(Avg2, Fore => 1, Aft => 2, Exp => 0); New_Line;
   
   Avg3 := Average(10, 20, 30);
   Put("10, 20和30的平均值是: "); Put(Avg3, Fore => 1, Aft => 2, Exp => 0); New_Line;
   
   -- 匿名过程（Ada 2012及以后版本支持）
   Put_Line("");
   Put_Line("===== 匿名过程 =====");
   
   declare
      procedure Anonymous is
      begin
         Put_Line("这是一个在declare块中定义的局部过程");
      end Anonymous;
   begin
      Anonymous;
   end;
   
   -- 前向声明
   Put_Line("");
   Put_Line("===== 前向声明 =====");
   
   declare
      -- 前向声明函数
      function Is_Even(N : Integer) return Boolean;
      
      -- 使用前向声明的函数
      function Is_Odd(N : Integer) return Boolean is
      begin
         if N = 0 then
            return False;
         else
            return Is_Even(N - 1);
         end if;
      end Is_Odd;
      
      -- 实现前向声明的函数
      function Is_Even(N : Integer) return Boolean is
      begin
         if N = 0 then
            return True;
         else
            return Is_Odd(N - 1);
         end if;
      end Is_Even;
      
   begin
      Put_Line("10是偶数? " & Boolean'Image(Is_Even(10)));
      Put_Line("7是偶数? " & Boolean'Image(Is_Even(7)));
   end;
   
   -- 参数传递方式
   Put_Line("");
   Put_Line("===== 参数传递方式 =====");
   
   declare
      A, B, C : Integer;
      
      -- 演示不同参数传递方式
      procedure Demo_Params(
         Param_In     : in Integer;      -- 只读参数
         Param_Out    : out Integer;     -- 只写参数
         Param_In_Out : in out Integer)  -- 可读可写参数
      is
      begin
         -- Param_In := Param_In + 1;    -- 错误！不能修改in参数
         Param_Out := Param_In * 2;      -- 可以写入out参数
         Param_In_Out := Param_In_Out * 2; -- 可以读取和修改in out参数
      end Demo_Params;
      
   begin
      A := 10;
      B := 20;
      C := 30;
      
      Put_Line("调用前: A = " & Integer'Image(A) & 
               ", B = " & Integer'Image(B) & 
               ", C = " & Integer'Image(C));
      
      Demo_Params(A, B, C);
      
      Put_Line("调用后: A = " & Integer'Image(A) & 
               ", B = " & Integer'Image(B) & 
               ", C = " & Integer'Image(C));
   end;
   
   Put_Line("");
   Put_Line("子程序示例结束");
   
end Subprograms;
