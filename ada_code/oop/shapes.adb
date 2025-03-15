-- shapes.adb
-- Ada面向对象编程示例：形状类实现

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics;      use Ada.Numerics;

package body Shapes is
   -- Shape类的方法实现
   procedure Set_Position(S : in out Shape; X, Y : Float) is
   begin
      S.X := X;
      S.Y := Y;
   end Set_Position;
   
   function Get_X(S : Shape) return Float is
   begin
      return S.X;
   end Get_X;
   
   function Get_Y(S : Shape) return Float is
   begin
      return S.Y;
   end Get_Y;
   
   -- Circle类的方法实现
   function Create_Circle(Radius : Float) return Circle is
      C : Circle;
   begin
      C.Radius := Radius;
      return C;
   end Create_Circle;
   
   procedure Set_Radius(C : in out Circle; Radius : Float) is
   begin
      C.Radius := Radius;
   end Set_Radius;
   
   function Get_Radius(C : Circle) return Float is
   begin
      return C.Radius;
   end Get_Radius;
   
   overriding function Area(C : Circle) return Float is
   begin
      return Pi * C.Radius ** 2;
   end Area;
   
   overriding function Perimeter(C : Circle) return Float is
   begin
      return 2.0 * Pi * C.Radius;
   end Perimeter;
   
   overriding procedure Display(C : Circle) is
   begin
      Put("圆形 - 位置: (");
      Put(C.X, Fore => 1, Aft => 2, Exp => 0);
      Put(", ");
      Put(C.Y, Fore => 1, Aft => 2, Exp => 0);
      Put("), 半径: ");
      Put(C.Radius, Fore => 1, Aft => 2, Exp => 0);
      Put(", 面积: ");
      Put(Area(C), Fore => 1, Aft => 2, Exp => 0);
      Put(", 周长: ");
      Put(Perimeter(C), Fore => 1, Aft => 2, Exp => 0);
      New_Line;
   end Display;
   
   -- Rectangle类的方法实现
   function Create_Rectangle(Width, Height : Float) return Rectangle is
      R : Rectangle;
   begin
      R.Width := Width;
      R.Height := Height;
      return R;
   end Create_Rectangle;
   
   procedure Set_Size(R : in out Rectangle; Width, Height : Float) is
   begin
      R.Width := Width;
      R.Height := Height;
   end Set_Size;
   
   function Get_Width(R : Rectangle) return Float is
   begin
      return R.Width;
   end Get_Width;
   
   function Get_Height(R : Rectangle) return Float is
   begin
      return R.Height;
   end Get_Height;
   
   overriding function Area(R : Rectangle) return Float is
   begin
      return R.Width * R.Height;
   end Area;
   
   overriding function Perimeter(R : Rectangle) return Float is
   begin
      return 2.0 * (R.Width + R.Height);
   end Perimeter;
   
   overriding procedure Display(R : Rectangle) is
   begin
      Put("矩形 - 位置: (");
      Put(R.X, Fore => 1, Aft => 2, Exp => 0);
      Put(", ");
      Put(R.Y, Fore => 1, Aft => 2, Exp => 0);
      Put("), 宽度: ");
      Put(R.Width, Fore => 1, Aft => 2, Exp => 0);
      Put(", 高度: ");
      Put(R.Height, Fore => 1, Aft => 2, Exp => 0);
      Put(", 面积: ");
      Put(Area(R), Fore => 1, Aft => 2, Exp => 0);
      Put(", 周长: ");
      Put(Perimeter(R), Fore => 1, Aft => 2, Exp => 0);
      New_Line;
   end Display;
   
end Shapes;
