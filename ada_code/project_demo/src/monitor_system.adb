-- monitor_system.adb
-- 简单数据采集与监控演示主程序

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

with Data_Types;         use Data_Types;
with Data_Processor;     use Data_Processor;

procedure Monitor_System is
   -- 配置
   NUM_SENSORS : constant := 3;
   
   -- 数据处理器
   Processor : Reading_Processor;
   
   -- 随机数生成器
   Gen : Generator;
   
   -- 生成模拟数据
   function Generate_Reading(ID : Sensor_ID) return Sensor_Reading is
      -- 生成-100到100之间的随机值
      Value : constant Float := (Random(Gen) * 200.0) - 100.0;
   begin
      return (ID    => ID,
              Value => Sensor_Value(Value));
   end Generate_Reading;
   
   -- 显示处理后的数据
   procedure Display_Reading(Reading : Processed_Reading) is
   begin
      Put("传感器 #");
      Put(Integer(Reading.Raw_Reading.ID), Width => 2);
      Put(" | 原始值: ");
      Put(Float(Reading.Raw_Reading.Value), Fore => 4, Aft => 2, Exp => 0);
      Put(" | 平均值: ");
      Put(Float(Reading.Avg_Value), Fore => 4, Aft => 2, Exp => 0);
      Put(" | 状态: ");
      Put_Line(if Reading.Status = Valid then "有效" else "无效");
   end Display_Reading;
   
   -- 显示统计信息
   procedure Display_Statistics is
   begin
      Put_Line("统计信息:");
      Put("  样本数: "); Put(Get_Sample_Count(Processor), Width => 0); New_Line;
      Put("  当前平均值: "); 
      Put(Float(Get_Average(Processor)), Fore => 4, Aft => 2, Exp => 0); 
      New_Line(2);
   end Display_Statistics;
   
   -- 主程序变量
   Reading : Sensor_Reading;
   Processed : Processed_Reading;
   Display_Count : Natural := 0;
   
begin
   -- 初始化随机数生成器
   Reset(Gen);
   
   Put_Line("===== 简单数据采集与监控演示 =====");
   Put_Line("按Ctrl+C停止程序");
   New_Line;
   
   -- 主循环
   loop
      -- 清屏
      Put(ASCII.ESC & "[2J" & ASCII.ESC & "[H");
      Put_Line("===== 简单数据采集与监控演示 =====");
      Put_Line("运行时间:" & Integer'Image(Display_Count) & " 秒");
      New_Line;
      
      -- 处理每个传感器的数据
      for I in 1..NUM_SENSORS loop
         Reading := Generate_Reading(Sensor_ID(I));
         if Is_Valid_Reading(Reading) then
            Process(Processor, Reading, Processed);
            Display_Reading(Processed);
         end if;
      end loop;
      
      New_Line;
      Display_Statistics;
      
      delay 1.0;  -- 每秒更新一次
      Display_Count := Display_Count + 1;
   end loop;
end Monitor_System;
