-- hello.adb
-- 这是Ada语言的Hello World程序
-- Ada中的注释以两个连字符（--）开始，一直延续到行尾

-- 使用Ada.Text_IO包，它提供了基本的输入输出功能
with Ada.Text_IO;

-- 程序（procedure）是Ada的主要程序单元
-- 每个Ada程序都必须有一个主程序（main procedure）
procedure Hello is
   -- 这是声明部分，可以在这里声明变量、常量、类型等
begin
   -- 这是执行部分，包含程序的实际指令
   
   -- 使用Ada.Text_IO包中的Put_Line过程输出文本
   -- Put_Line会在输出后自动添加换行符
   Ada.Text_IO.Put_Line("你好，世界！");
   Ada.Text_IO.Put_Line("Hello, World!");
   
   -- 也可以使用"use"指令简化包的使用
   -- 如果在程序开头添加"use Ada.Text_IO;"，就可以直接调用Put_Line
end Hello;  -- 程序结束，注意end后面要跟程序名
