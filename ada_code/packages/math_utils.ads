-- math_utils.ads
-- Ada包规范（specification）文件
-- 包规范定义了包的公共接口

package Math_Utils is
   -- 包规范中声明的常量、类型、子程序等可以被其他单元访问
   
   -- 常量
   Pi : constant Float := 3.14159;
   
   -- 类型
   subtype Angle is Float range 0.0 .. 360.0;
   
   -- 函数声明
   function Square(X : Float) return Float;
   function Cube(X : Float) return Float;
   function To_Radians(Degrees : Angle) return Float;
   function To_Degrees(Radians : Float) return Float;
   
   -- 过程声明
   procedure Print_Value(X : Float);
   
   -- 私有部分（仅在包体中可见）
private
   -- 私有常量
   Rad_Per_Deg : constant Float := 0.01745329; -- Pi/180
   
end Math_Utils;
