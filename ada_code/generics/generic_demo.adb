-- generic_demo.adb
-- Ada泛型编程示例

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Exceptions;      use Ada.Exceptions;

procedure Generic_Demo is
   -- 泛型包：实现一个简单的栈
   generic
      type Item_Type is private;  -- 私有类型，可以是任何类型
      Size : Positive;           -- 栈的大小
   package Generic_Stack is
      type Stack is private;
      
      Overflow_Error  : exception;
      Underflow_Error : exception;
      
      procedure Push(S : in out Stack; Item : in Item_Type);
      procedure Pop(S : in out Stack; Item : out Item_Type);
      function Is_Empty(S : Stack) return Boolean;
      function Is_Full(S : Stack) return Boolean;
      
   private
      type Stack_Array is array (1 .. Size) of Item_Type;
      type Stack is record
         Items : Stack_Array;
         Top   : Natural := 0;
      end record;
   end Generic_Stack;
   
   -- 泛型包的实现
   package body Generic_Stack is
      procedure Push(S : in out Stack; Item : in Item_Type) is
      begin
         if Is_Full(S) then
            raise Overflow_Error with "栈已满";
         end if;
         S.Top := S.Top + 1;
         S.Items(S.Top) := Item;
      end Push;
      
      procedure Pop(S : in out Stack; Item : out Item_Type) is
      begin
         if Is_Empty(S) then
            raise Underflow_Error with "栈已空";
         end if;
         Item := S.Items(S.Top);
         S.Top := S.Top - 1;
      end Pop;
      
      function Is_Empty(S : Stack) return Boolean is
      begin
         return S.Top = 0;
      end Is_Empty;
      
      function Is_Full(S : Stack) return Boolean is
      begin
         return S.Top = Size;
      end Is_Full;
   end Generic_Stack;
   
   -- 泛型函数：交换两个值
   generic
      type Element_Type is private;
   procedure Generic_Swap(X, Y : in out Element_Type);
   
   procedure Generic_Swap(X, Y : in out Element_Type) is
      Temp : Element_Type;
   begin
      Temp := X;
      X := Y;
      Y := Temp;
   end Generic_Swap;
   
   -- 泛型函数：查找数组中的最大值
   generic
      type Element_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function ">"(Left, Right : Element_Type) return Boolean is <>;
   function Generic_Max(A : Array_Type) return Element_Type;
   
   function Generic_Max(A : Array_Type) return Element_Type is
      Max_Value : Element_Type := A(A'First);
   begin
      for I in A'Range loop
         if A(I) > Max_Value then
            Max_Value := A(I);
         end if;
      end loop;
      return Max_Value;
   end Generic_Max;
   
   -- 实例化整数栈
   package Integer_Stack is new Generic_Stack(Item_Type => Integer, Size => 5);
   use Integer_Stack;
   Int_Stack : Stack;
   Int_Item  : Integer;
   
   -- 实例化字符串栈
   Max_String_Length : constant := 10;
   subtype Fixed_String is String(1 .. Max_String_Length);
   package String_Stack is new Generic_Stack(Item_Type => Fixed_String, Size => 3);
   Str_Stack : String_Stack.Stack;
   Str_Item  : Fixed_String;
   
   -- 实例化交换过程
   procedure Swap_Integer is new Generic_Swap(Element_Type => Integer);
   procedure Swap_Float is new Generic_Swap(Element_Type => Float);
   
   -- 实例化最大值函数
   type Int_Array is array (Positive range <>) of Integer;
   function Max_Integer is new Generic_Max(Element_Type => Integer,
                                         Index_Type   => Positive,
                                         Array_Type   => Int_Array);
   
   -- 测试变量
   X : Integer := 10;
   Y : Integer := 20;
   A : Float := 3.14;
   B : Float := 2.718;
   Numbers : Int_Array := (15, 7, 23, 9, 12);
   
begin
   Put_Line("===== Ada泛型编程示例 =====");
   Put_Line("");
   
   -- 测试整数栈
   Put_Line("示例1：使用整数栈");
   begin
      Push(Int_Stack, 10);
      Push(Int_Stack, 20);
      Push(Int_Stack, 30);
      
      Pop(Int_Stack, Int_Item);
      Put("弹出: "); Put(Int_Item); New_Line;
      Pop(Int_Stack, Int_Item);
      Put("弹出: "); Put(Int_Item); New_Line;
   exception
      when E : others =>
         Put_Line("栈操作错误：" & Exception_Message(E));
   end;
   
   Put_Line("");
   
   -- 测试字符串栈
   Put_Line("示例2：使用字符串栈");
   begin
      String_Stack.Push(Str_Stack, "Hello     ");
      String_Stack.Push(Str_Stack, "World     ");
      
      String_Stack.Pop(Str_Stack, Str_Item);
      Put_Line("弹出: " & Str_Item);
   exception
      when E : others =>
         Put_Line("栈操作错误：" & Exception_Message(E));
   end;
   
   Put_Line("");
   
   -- 测试泛型交换过程
   Put_Line("示例3：使用泛型交换过程");
   Put("交换前：X = "); Put(X); Put(", Y = "); Put(Y); New_Line;
   Swap_Integer(X, Y);
   Put("交换后：X = "); Put(X); Put(", Y = "); Put(Y); New_Line;
   
   Put("交换前：A = "); Put(A, Fore => 1, Aft => 3, Exp => 0);
   Put(", B = "); Put(B, Fore => 1, Aft => 3, Exp => 0); New_Line;
   Swap_Float(A, B);
   Put("交换后：A = "); Put(A, Fore => 1, Aft => 3, Exp => 0);
   Put(", B = "); Put(B, Fore => 1, Aft => 3, Exp => 0); New_Line;
   
   Put_Line("");
   
   -- 测试泛型最大值函数
   Put_Line("示例4：使用泛型最大值函数");
   Put_Line("数组：15, 7, 23, 9, 12");
   Put("最大值："); Put(Max_Integer(Numbers)); New_Line;
   
   Put_Line("");
   Put_Line("泛型编程示例结束");
   
end Generic_Demo;
