-- math_utils.adb
-- Ada包体（body）文件
-- 包体实现了包规范中声明的子程序

with Ada.Text_IO;
with Ada.Float_Text_IO;

package body Math_Utils is
   -- 使用Text_IO包简化输出
   use Ada.Text_IO;
   use Ada.Float_Text_IO;
   
   -- 实现Square函数
   function Square(X : Float) return Float is
   begin
      return X * X;
   end Square;
   
   -- 实现Cube函数
   function Cube(X : Float) return Float is
   begin
      return X * X * X;
   end Cube;
   
   -- 实现To_Radians函数
   function To_Radians(Degrees : Angle) return Float is
   begin
      return Degrees * Rad_Per_Deg;
   end To_Radians;
   
   -- 实现To_Degrees函数
   function To_Degrees(Radians : Float) return Float is
   begin
      return Radians / Rad_Per_Deg;
   end To_Degrees;
   
   -- 实现Print_Value过程
   procedure Print_Value(X : Float) is
   begin
      Put("值 = ");
      Put(X, Fore => 1, Aft => 4, Exp => 0);
      New_Line;
   end Print_Value;
   
   -- 包体中可以定义包规范中未声明的子程序，这些子程序只在包内部可见
   procedure Internal_Helper is
   begin
      Put_Line("这是一个内部辅助过程，只在包内部可见");
   end Internal_Helper;
   
begin
   -- 包的初始化代码（可选）
   -- 这部分代码在首次使用包时执行
   Put_Line("Math_Utils包已初始化");
   
end Math_Utils;
