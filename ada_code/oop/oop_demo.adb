-- oop_demo.adb
-- Ada面向对象编程示例：主程序

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Shapes;           use Shapes;

procedure OOP_Demo is
   -- 声明形状数组类型（使用类标记）
   type Shape_Access is access all Shape'Class;
   type Shape_Array is array (Positive range <>) of Shape_Access;
   
   -- 创建一些形状
   C1 : Shape_Access := new Circle'(Create_Circle(2.0));
   C2 : Shape_Access := new Circle'(Create_Circle(3.0));
   R1 : Shape_Access := new Rectangle'(Create_Rectangle(4.0, 3.0));
   R2 : Shape_Access := new Rectangle'(Create_Rectangle(5.0, 2.0));
   
   -- 创建形状数组
   Shapes : Shape_Array(1..4) := (C1, R1, C2, R2);
   
   -- 计算所有形状的总面积
   function Total_Area(Shapes : Shape_Array) return Float is
      Total : Float := 0.0;
   begin
      for S of Shapes loop
         Total := Total + Area(S.all);
      end loop;
      return Total;
   end Total_Area;
   
   -- 计算所有形状的总周长
   function Total_Perimeter(Shapes : Shape_Array) return Float is
      Total : Float := 0.0;
   begin
      for S of Shapes loop
         Total := Total + Perimeter(S.all);
      end loop;
      return Total;
   end Total_Perimeter;
   
begin
   Put_Line("===== Ada面向对象编程示例 =====");
   Put_Line("");
   
   -- 设置形状的位置
   Set_Position(C1.all, 1.0, 1.0);
   Set_Position(C2.all, 2.0, 2.0);
   Set_Position(R1.all, 3.0, 3.0);
   Set_Position(R2.all, 4.0, 4.0);
   
   -- 显示所有形状
   Put_Line("所有形状的信息：");
   for S of Shapes loop
      Display(S.all);
   end loop;
   
   Put_Line("");
   Put("所有形状的总面积：");
   Put(Total_Area(Shapes), Fore => 1, Aft => 2, Exp => 0);
   New_Line;
   
   Put("所有形状的总周长：");
   Put(Total_Perimeter(Shapes), Fore => 1, Aft => 2, Exp => 0);
   New_Line;
   
   -- 修改一些形状的属性
   Put_Line("");
   Put_Line("修改形状的属性：");
   Set_Radius(Circle(C1.all), 4.0);        -- 修改第一个圆的半径
   Set_Size(Rectangle(R1.all), 6.0, 5.0);  -- 修改第一个矩形的尺寸
   
   -- 再次显示所有形状
   Put_Line("");
   Put_Line("修改后的形状信息：");
   for S of Shapes loop
      Display(S.all);
   end loop;
   
   Put_Line("");
   Put("修改后的总面积：");
   Put(Total_Area(Shapes), Fore => 1, Aft => 2, Exp => 0);
   New_Line;
   
   Put("修改后的总周长：");
   Put(Total_Perimeter(Shapes), Fore => 1, Aft => 2, Exp => 0);
   New_Line;
   
   Put_Line("");
   Put_Line("面向对象编程示例结束");
   
end OOP_Demo;
