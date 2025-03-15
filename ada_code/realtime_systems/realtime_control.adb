with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with System; use System;

procedure Realtime_Control is
   -- 定义实时任务类型
   task type Periodic_Task(Period_Ms : Integer) is
      pragma Priority(Priority'Last);
      entry Start(Task_Name : in String);
      entry Stop;  
   end Periodic_Task;

   -- 定义一个实时控制器
   protected type RT_Controller is
      -- 设置和获取控制值
      procedure Set_Value(Value : Float);
      function Get_Value return Float;
      
      -- 执行控制计算
      procedure Update_Control;
      
      -- 检查是否在安全范围内
      entry Wait_Until_Safe;
   private
      Current_Value : Float := 0.0;
      Target_Value : Float := 0.0;
      Is_Safe : Boolean := True;
   end RT_Controller;

   -- 实现实时控制器
   protected body RT_Controller is
      procedure Set_Value(Value : Float) is
      begin
         Target_Value := Value;
      end Set_Value;

      function Get_Value return Float is
      begin
         return Current_Value;
      end Get_Value;

      procedure Update_Control is
         Error : constant Float := Target_Value - Current_Value;
      begin
         -- 简单的PID控制器实现
         Current_Value := Current_Value + (Error * 0.1);  
         Is_Safe := abs Error < 1.0;  
      end Update_Control;

      entry Wait_Until_Safe when Is_Safe is
      begin
         null;  
      end Wait_Until_Safe;
   end RT_Controller;

   -- 实现周期性任务
   task body Periodic_Task is
      Next_Period : Time := Clock;
      Period : constant Time_Span := Milliseconds(Period_Ms);
      Controller : RT_Controller;
      Name : String(1..15) := (others => ' ');
      Running : Boolean := True;  
      
      -- 模拟传感器读数
      function Read_Sensor return Float is
         -- 在实际应用中，这里会读取真实的传感器数据
         Current_Time : constant Time := Clock;
      begin
         return Float(To_Duration(Current_Time - Next_Period));
      end Read_Sensor;
      
   begin
      -- 接收任务名称
      accept Start(Task_Name : in String) do
         Name(1..Task_Name'Length) := Task_Name;
      end Start;
      
      while Running loop  
         select
            accept Stop do
               Running := False;
            end Stop;
         else
            -- 读取传感器并更新控制器
            Controller.Set_Value(Read_Sensor);
            Controller.Update_Control;
            
            -- 等待直到状态安全
            select
               Controller.Wait_Until_Safe;
               Put_Line(Name & ": Safe state achieved. Value:" & 
                        Float'Image(Controller.Get_Value));
            or
               delay 0.1;  
               Put_Line(Name & ": Safety timeout!");
            end select;
            
            -- 精确的周期调度
            Next_Period := Next_Period + Period;
            delay until Next_Period;
         end select;
      end loop;
      Put_Line(Name & ": Task terminated.");
   end Periodic_Task;

   -- 创建两个不同周期的实时任务
   Fast_Task : Periodic_Task(100);  
   Slow_Task : Periodic_Task(500);  

begin
   Put_Line("Starting real-time control system...");
   Fast_Task.Start("Fast Controller");
   Slow_Task.Start("Slow Controller");
   delay 5.0;  
   
   -- 停止任务
   Put_Line("Stopping tasks...");
   Fast_Task.Stop;
   Slow_Task.Stop;
   
   Put_Line("Real-time control system demonstration completed.");
end Realtime_Control;
