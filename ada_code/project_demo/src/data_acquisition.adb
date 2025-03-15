-- data_acquisition.adb
-- 数据采集模块实现

with Ada.Calendar;     use Ada.Calendar;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;

package body Data_Acquisition is
   -- 随机数生成器
   Generator : Generator_Type;
   
   -- 实现受保护的数据缓冲区
   protected body Reading_Buffer is
      entry Put(Reading : in Sensor_Reading)
        when Count < Size is
      begin
         Buffer(Tail) := Reading;
         Tail := (if Tail = Size then 1 else Tail + 1);
         Count := Count + 1;
      end Put;
      
      entry Get(Reading : out Sensor_Reading)
        when Count > 0 is
      begin
         Reading := Buffer(Head);
         Head := (if Head = Size then 1 else Head + 1);
         Count := Count - 1;
      end Get;
      
      function Is_Empty return Boolean is
      begin
         return Count = 0;
      end Is_Empty;
      
      function Is_Full return Boolean is
      begin
         return Count = Size;
      end Is_Full;
   end Reading_Buffer;
   
   -- 实现传感器任务
   task body Sensor_Task is
      Task_Config : Acquisition_Config;
      Next_Time   : Time;
      Period      : Duration;
      Running     : Boolean := False;
      Last_Reading : Sensor_Reading;
   begin
      -- 等待配置
      accept Configure(Config : in Acquisition_Config) do
         Task_Config := Config;
         Period := Duration(Task_Config.Sample_Period_Ms) / 1000.0;
      end Configure;
      
      -- 等待启动信号
      accept Start do
         Running := True;
         Next_Time := Clock;
      end Start;
      
      -- 主循环
      while Running loop
         -- 生成新的读数
         Last_Reading := Generate_Reading(Task_Config.Target_ID);
         
         -- 提供最新读数
         select
            accept Get_Latest_Reading(Reading : out Sensor_Reading) do
               Reading := Last_Reading;
            end Get_Latest_Reading;
         else
            null;  -- 如果没有请求，继续执行
         end select;
         
         -- 检查停止请求
         select
            accept Stop do
               Running := False;
            end Stop;
         else
            -- 等待下一个周期
            delay until Next_Time;
            Next_Time := Next_Time + Period;
         end select;
      end loop;
   end Sensor_Task;
   
   -- 实现模拟传感器读数生成
   function Generate_Reading(ID : Sensor_ID) return Sensor_Reading is
      -- 生成-100到100之间的随机值
      Value : constant Float := (Random(Generator) * 200.0) - 100.0;
   begin
      return (ID        => ID,
              Value     => Sensor_Value(Value),
              Timestamp => Clock);
   end Generate_Reading;
   
   -- 实现采集器管理器操作
   procedure Initialize(Manager : in out Acquisition_Manager) is
   begin
      Reset(Generator);  -- 初始化随机数生成器
      Manager.Count := 0;
      -- 确保所有任务指针为空
      for I in Manager.Tasks'Range loop
         Manager.Tasks(I) := null;
      end loop;
   end Initialize;
   
   procedure Free is new Ada.Unchecked_Deallocation
     (Sensor_Task, Sensor_Task_Access);
   
   procedure Add_Sensor(Manager : in out Acquisition_Manager;
                       Config  : Acquisition_Config) is
   begin
      if not Is_Full(Manager) then
         Manager.Count := Manager.Count + 1;
         Manager.Tasks(Manager.Count) := new Sensor_Task;
         Manager.Tasks(Manager.Count).Configure(Config);
      end if;
   end Add_Sensor;
   
   procedure Start_All(Manager : in out Acquisition_Manager) is
   begin
      for I in 1..Manager.Count loop
         if Manager.Tasks(I) /= null then
            Manager.Tasks(I).Start;
         end if;
      end loop;
   end Start_All;
   
   procedure Stop_All(Manager : in out Acquisition_Manager) is
   begin
      -- 停止所有任务
      for I in 1..Manager.Count loop
         if Manager.Tasks(I) /= null then
            Manager.Tasks(I).Stop;
            Free(Manager.Tasks(I));  -- 释放任务内存
         end if;
      end loop;
      Manager.Count := 0;
   end Stop_All;
   
   function Is_Full(Manager : Acquisition_Manager) return Boolean is
   begin
      return Manager.Count >= Manager.Max_Sensors;
   end Is_Full;
   
begin
   Reset(Generator);  -- 初始化随机数生成器
end Data_Acquisition;
