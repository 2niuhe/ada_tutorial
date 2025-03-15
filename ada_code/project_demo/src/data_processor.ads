-- data_processor.ads
-- 数据处理模块，演示Ada的契约式编程和接口抽象特性

with Data_Types; use Data_Types;

package Data_Processor is
   -- 简单的数据处理器
   type Reading_Processor is tagged private;
   
   -- 处理一个读数
   procedure Process(Self : in out Reading_Processor;
                    Reading : in Sensor_Reading;
                    Result : out Processed_Reading)
      with Pre => Is_Valid_Reading(Reading);
   
   -- 获取统计信息
   function Get_Average(Self : Reading_Processor) return Sensor_Value;
   function Get_Sample_Count(Self : Reading_Processor) return Natural;
   
private
   -- 保持最近5个读数的移动平均
   Window_Size : constant := 5;
   type Value_Array is array(1..Window_Size) of Sensor_Value;
   
   type Reading_Processor is tagged record
      Values       : Value_Array := (others => 0.0);
      Current_Pos  : Positive := 1;
      Sample_Count : Natural := 0;
      Sum         : Float := 0.0;
   end record;
end Data_Processor;
