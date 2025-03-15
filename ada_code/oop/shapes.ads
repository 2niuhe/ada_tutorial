-- shapes.ads
-- Ada面向对象编程示例：形状类层次结构

package Shapes is
   -- 抽象基类：形状
   type Shape is abstract tagged private;
   
   -- 抽象方法
   function Area(S : Shape) return Float is abstract;
   function Perimeter(S : Shape) return Float is abstract;
   procedure Display(S : Shape) is abstract;
   
   -- 具体方法
   procedure Set_Position(S : in out Shape; X, Y : Float);
   function Get_X(S : Shape) return Float;
   function Get_Y(S : Shape) return Float;
   
   -- 圆形类
   type Circle is new Shape with private;
   
   -- 圆形构造函数
   function Create_Circle(Radius : Float) return Circle;
   
   -- 覆盖抽象方法
   overriding function Area(C : Circle) return Float;
   overriding function Perimeter(C : Circle) return Float;
   overriding procedure Display(C : Circle);
   
   -- 圆形特有方法
   procedure Set_Radius(C : in out Circle; Radius : Float);
   function Get_Radius(C : Circle) return Float;
   
   -- 矩形类
   type Rectangle is new Shape with private;
   
   -- 矩形构造函数
   function Create_Rectangle(Width, Height : Float) return Rectangle;
   
   -- 覆盖抽象方法
   overriding function Area(R : Rectangle) return Float;
   overriding function Perimeter(R : Rectangle) return Float;
   overriding procedure Display(R : Rectangle);
   
   -- 矩形特有方法
   procedure Set_Size(R : in out Rectangle; Width, Height : Float);
   function Get_Width(R : Rectangle) return Float;
   function Get_Height(R : Rectangle) return Float;
   
private
   -- 私有类型定义
   type Shape is abstract tagged record
      X, Y : Float := 0.0;  -- 位置
   end record;
   
   type Circle is new Shape with record
      Radius : Float := 1.0;
   end record;
   
   type Rectangle is new Shape with record
      Width  : Float := 1.0;
      Height : Float := 1.0;
   end record;
   
end Shapes;
