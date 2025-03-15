-- packages.adb
-- Ada包的使用示例
-- 展示如何使用自定义包和Ada标准库包

with Ada.Text_IO;
with Ada.Float_Text_IO;
with Math_Utils;  -- 引入我们自定义的Math_Utils包

procedure Packages is
   -- 使用Text_IO包简化输出
   use Ada.Text_IO;
   use Ada.Float_Text_IO;
   
   -- 声明变量
   X : Float := 5.0;
   Angle_Deg : Math_Utils.Angle := 45.0;
   Angle_Rad : Float;
   
begin
   Put_Line("===== Ada包示例 =====");
   
   -- 使用Math_Utils包中的常量
   Put("Pi = ");
   Put(Math_Utils.Pi, Fore => 1, Aft => 5, Exp => 0);
   New_Line;
   
   -- 使用Math_Utils包中的函数
   Put("X = " & Float'Image(X));
   New_Line;
   
   Put("X的平方 = " & Float'Image(Math_Utils.Square(X)));
   New_Line;
   
   Put("X的立方 = " & Float'Image(Math_Utils.Cube(X)));
   New_Line;
   
   -- 使用角度转换函数
   Put("角度(度) = " & Float'Image(Angle_Deg));
   New_Line;
   
   Angle_Rad := Math_Utils.To_Radians(Angle_Deg);
   Put("角度(弧度) = " & Float'Image(Angle_Rad));
   New_Line;
   
   Put("转回度 = " & Float'Image(Math_Utils.To_Degrees(Angle_Rad)));
   New_Line;
   
   -- 使用Math_Utils包中的过程
   Math_Utils.Print_Value(X);
   
   -- 使用use子句简化包的使用
   Put_Line("");
   Put_Line("===== 使用use子句 =====");
   
   declare
      use Math_Utils;  -- 局部use子句
   begin
      -- 现在可以直接使用包中的标识符，不需要包名前缀
      Put("Pi = ");
      Put(Pi, Fore => 1, Aft => 5, Exp => 0);
      New_Line;
      
      Put("X的平方 = " & Float'Image(Square(X)));
      New_Line;
      
      Print_Value(Cube(X));
   end;
   
   -- 使用重命名
   Put_Line("");
   Put_Line("===== 使用重命名 =====");
   
   declare
      -- 重命名包
      package MU renames Math_Utils;
      
      -- 重命名函数
      function Sqr(X : Float) return Float renames MU.Square;
   begin
      Put("使用重命名的包: Pi = ");
      Put(MU.Pi, Fore => 1, Aft => 5, Exp => 0);
      New_Line;
      
      Put("使用重命名的函数: X的平方 = " & Float'Image(Sqr(X)));
      New_Line;
   end;
   
   -- 使用嵌套包
   Put_Line("");
   Put_Line("===== 嵌套包 =====");
   
   declare
      -- 声明嵌套包
      package Nested is
         Factor : constant Float := 2.5;
         
         function Scale(X : Float) return Float;
      end Nested;
      
      -- 实现嵌套包
      package body Nested is
         function Scale(X : Float) return Float is
         begin
            return X * Factor;
         end Scale;
      end Nested;
   begin
      Put("使用嵌套包: 缩放X = " & Float'Image(Nested.Scale(X)));
      New_Line;
   end;
   
   -- 注意：无法访问Math_Utils包中的私有部分（Rad_Per_Deg）
   -- 以下代码会导致编译错误：
   -- Put_Line("Rad_Per_Deg = " & Float'Image(Math_Utils.Rad_Per_Deg));
   
   -- 注意：无法访问Math_Utils包中的内部子程序（Internal_Helper）
   -- 以下代码会导致编译错误：
   -- Math_Utils.Internal_Helper;
   
   Put_Line("");
   Put_Line("包示例结束");
   
end Packages;
