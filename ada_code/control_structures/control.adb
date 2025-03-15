-- control.adb
-- Ada控制结构示例
-- 展示Ada中的条件语句、循环和其他流程控制机制

with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Control is
   -- 使用Text_IO包简化输入输出
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   
   -- 声明变量
   A : Integer := 10;
   B : Integer := 20;
   Grade : Character := 'B';
   Count : Integer := 1;
   
begin
   Put_Line("===== Ada控制结构示例 =====");
   
   -- if语句
   Put_Line("");
   Put_Line("===== if语句 =====");
   
   Put_Line("A = " & Integer'Image(A) & ", B = " & Integer'Image(B));
   
   if A > B then
      Put_Line("A大于B");
   elsif A < B then
      Put_Line("A小于B");
   else
      Put_Line("A等于B");
   end if;
   
   -- if表达式（Ada 2012及以后版本支持）
   declare
      Max : Integer := (if A > B then A else B);
   begin
      Put("A和B中的最大值是: "); Put(Max); New_Line;
   end;
   
   -- case语句
   Put_Line("");
   Put_Line("===== case语句 =====");
   
   Put_Line("成绩: " & Grade);
   
   case Grade is
      when 'A' =>
         Put_Line("优秀");
      when 'B' =>
         Put_Line("良好");
      when 'C' =>
         Put_Line("及格");
      when 'D' | 'F' =>  -- 可以组合多个选项
         Put_Line("不及格");
      when others =>     -- 默认情况
         Put_Line("无效成绩");
   end case;
   
   -- 基本循环
   Put_Line("");
   Put_Line("===== 基本循环 =====");
   
   -- 无限循环（使用exit退出）
   loop
      Put("Count = "); Put(Count); New_Line;
      Count := Count + 1;
      exit when Count > 3;  -- 条件退出
   end loop;
   
   -- while循环
   Put_Line("");
   Put_Line("===== while循环 =====");
   
   Count := 1;
   while Count <= 3 loop
      Put("While循环: Count = "); Put(Count); New_Line;
      Count := Count + 1;
   end loop;
   
   -- for循环
   Put_Line("");
   Put_Line("===== for循环 =====");
   
   -- 范围循环
   for I in 1 .. 3 loop
      Put("For循环(正向): I = "); Put(I); New_Line;
   end loop;
   
   -- 反向循环
   for I in reverse 1 .. 3 loop
      Put("For循环(反向): I = "); Put(I); New_Line;
   end loop;
   
   -- 使用数组的for循环
   declare
      type Int_Array is array (1 .. 5) of Integer;
      Arr : Int_Array := (10, 20, 30, 40, 50);
   begin
      Put_Line("");
      Put_Line("遍历数组:");
      for I in Arr'Range loop
         Put("Arr(" & Integer'Image(I) & ") = ");
         Put(Arr(I));
         New_Line;
      end loop;
   end;
   
   -- 嵌套循环和标签
   Put_Line("");
   Put_Line("===== 嵌套循环和标签 =====");
   
   Outer_Loop:
   for I in 1 .. 3 loop
      Inner_Loop:
      for J in 1 .. 2 loop
         Put("I = " & Integer'Image(I) & ", J = " & Integer'Image(J));
         New_Line;
         
         -- 如果I=2且J=1，跳过当前迭代的剩余部分
         if I = 2 and J = 1 then
            Put_Line("  跳过当前迭代");
            goto Continue_Inner;
         end if;
         
         -- 如果I=3且J=1，完全退出外层循环
         if I = 3 and J = 1 then
            Put_Line("  退出外层循环");
            exit Outer_Loop;
         end if;
         
         Put_Line("  继续执行");
         
         <<Continue_Inner>>
         null;  -- 空语句，不执行任何操作
      end loop Inner_Loop;
   end loop Outer_Loop;
   
   -- 块语句（声明局部变量的作用域）
   Put_Line("");
   Put_Line("===== 块语句 =====");
   
   declare
      Local_Var : Integer := 100;
   begin
      Put_Line("局部变量 Local_Var = " & Integer'Image(Local_Var));
      
      -- 嵌套块
      declare
         Another_Var : Integer := 200;
      begin
         Put_Line("另一个局部变量 Another_Var = " & Integer'Image(Another_Var));
         Local_Var := Local_Var + Another_Var;
      end;
      
      Put_Line("修改后的 Local_Var = " & Integer'Image(Local_Var));
   end;
   
   -- 这里无法访问Local_Var和Another_Var
   
   Put_Line("");
   Put_Line("控制结构示例结束");
   
end Control;
