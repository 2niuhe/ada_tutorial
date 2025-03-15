-- data_processor.adb
-- 数据处理模块实现

package body Data_Processor is
   -- 处理一个读数
   procedure Process(Self : in out Reading_Processor;
                    Reading : in Sensor_Reading;
                    Result : out Processed_Reading) is
      Status : Data_Status;
   begin
      -- 更新移动平均窗口
      Self.Sum := Self.Sum - Float(Self.Values(Self.Current_Pos));
      Self.Values(Self.Current_Pos) := Reading.Value;
      Self.Sum := Self.Sum + Float(Reading.Value);
      
      -- 更新位置和计数
      Self.Current_Pos := (if Self.Current_Pos = Window_Size then 1 
                          else Self.Current_Pos + 1);
      if Self.Sample_Count < Window_Size then
         Self.Sample_Count := Self.Sample_Count + 1;
      end if;
      
      -- 计算平均值
      declare
         Avg : constant Sensor_Value := 
           Sensor_Value(Self.Sum / Float(Self.Sample_Count));
      begin
         -- 如果当前值与平均值相差太大，标记为无效
         if abs(Float(Reading.Value - Avg)) > 100.0 then
            Status := Invalid;
         else
            Status := Valid;
         end if;
         
         Result := (Raw_Reading => Reading,
                   Status     => Status,
                   Avg_Value  => Avg);
      end;
   end Process;
   
   -- 获取平均值
   function Get_Average(Self : Reading_Processor) return Sensor_Value is
   begin
      if Self.Sample_Count = 0 then
         return 0.0;
      else
         return Sensor_Value(Self.Sum / Float(Self.Sample_Count));
      end if;
   end Get_Average;
   
   -- 获取样本数
   function Get_Sample_Count(Self : Reading_Processor) return Natural is
   begin
      return Self.Sample_Count;
   end Get_Sample_Count;
end Data_Processor;
