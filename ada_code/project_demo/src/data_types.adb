-- data_types.adb
-- 数据类型实现

package body Data_Types is
   -- 验证传感器读数
   function Is_Valid_Reading(Reading : Sensor_Reading) return Boolean is
      pragma Unreferenced (Reading);
   begin
      -- 由于使用了范围类型，编译器已经确保了值在有效范围内
      return True;
   end Is_Valid_Reading;
end Data_Types;
