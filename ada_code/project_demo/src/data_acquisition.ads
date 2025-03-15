-- data_acquisition.ads
-- 数据采集模块，演示Ada的实时和并发编程特性

with System;
with Ada.Real_Time; use Ada.Real_Time;
with Data_Types; use Data_Types;

package Data_Acquisition is
   -- 采集器配置类型
   type Acquisition_Config is record
      Sample_Period_Ms : Positive;  -- 采样周期（毫秒）
      Priority        : System.Priority;
      Target_ID      : Sensor_ID;  -- 改名以避免命名冲突
   end record;
   
   -- 数据采集任务类型
   task type Sensor_Task is
      entry Configure(Config : in Acquisition_Config);
      entry Start;
      entry Stop;
      entry Get_Latest_Reading(Reading : out Sensor_Reading);
   end Sensor_Task;
   
   -- 缓冲区数组类型定义
   type Reading_Array is array(Positive range <>) of Sensor_Reading;
   
   -- 受保护的数据缓冲区
   protected type Reading_Buffer(Size : Positive) is
      entry Put(Reading : in Sensor_Reading);
      entry Get(Reading : out Sensor_Reading);
      function Is_Empty return Boolean;
      function Is_Full return Boolean;
   private
      Buffer : Reading_Array(1..Size);
      Count  : Natural := 0;
      Head   : Positive := 1;
      Tail   : Positive := 1;
   end Reading_Buffer;
   
   -- 模拟传感器读数生成
   function Generate_Reading(ID : Sensor_ID) return Sensor_Reading
      with Post => Generate_Reading'Result.ID = ID;
      
   -- 采集器管理器
   type Acquisition_Manager(Max_Sensors : Positive) is limited private;
   
   -- 管理器操作
   procedure Initialize(Manager : in out Acquisition_Manager);
   procedure Add_Sensor(Manager : in out Acquisition_Manager;
                       Config  : Acquisition_Config)
      with Pre => not Is_Full(Manager);
   procedure Start_All(Manager : in out Acquisition_Manager);
   procedure Stop_All(Manager : in out Acquisition_Manager);
   
   function Is_Full(Manager : Acquisition_Manager) return Boolean;
   
private
   type Sensor_Task_Access is access Sensor_Task;
   type Task_Array is array(Positive range <>) of Sensor_Task_Access;
   
   type Acquisition_Manager(Max_Sensors : Positive) is limited record
      Tasks : Task_Array(1..Max_Sensors);
      Count : Natural := 0;
   end record;
end Data_Acquisition;
