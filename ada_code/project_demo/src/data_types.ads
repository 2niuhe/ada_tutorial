-- data_types.ads
-- 定义系统中使用的数据类型

package Data_Types is
   -- 基本数据类型
   type Sensor_ID is new Integer range 1 .. 100;
   type Sensor_Value is new Float range -1000.0 .. 1000.0;
   
   -- 传感器读数
   type Sensor_Reading is record
      ID    : Sensor_ID;
      Value : Sensor_Value;
   end record;
   
   -- 数据状态
   type Data_Status is (Valid, Invalid);
   
   -- 处理后的数据
   type Processed_Reading is record
      Raw_Reading : Sensor_Reading;
      Status     : Data_Status;
      Avg_Value  : Sensor_Value;  -- 简单移动平均值
   end record;
   
   -- 验证函数
   function Is_Valid_Reading(Reading : Sensor_Reading) return Boolean;
end Data_Types;
