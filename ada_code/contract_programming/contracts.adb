-- contracts.adb
-- Ada高级特性演示：契约式编程

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Exceptions;      use Ada.Exceptions;

procedure Contracts is
   -- 定义一个简单的银行账户类型
   type Money is new Integer;
   
   type Account is record 
      Balance : Money := 0;
      Account_Number : Positive;
      Is_Active : Boolean := True;
   end record;
   
   -- 存款操作
   procedure Deposit(Acc : in out Account; Amount : Money)
      with
         Pre => Amount > 0 and Acc.Is_Active,
         Post => Acc.Balance = Acc'Old.Balance + Amount;
   
   -- 取款操作
   procedure Withdraw(Acc : in out Account; Amount : Money)
      with
         Pre => (Amount > 0 and 
                Amount <= Acc.Balance and 
                Acc.Is_Active),
         Post => (Acc.Balance = Acc'Old.Balance - Amount and
                Acc.Balance >= 0);
   
   -- 转账操作
   procedure Transfer(From, To : in out Account; Amount : Money)
      with
         Pre => (Amount > 0 and 
                Amount <= From.Balance and
                From.Is_Active and 
                To.Is_Active and
                From.Account_Number /= To.Account_Number),
         Post => (From.Balance = From'Old.Balance - Amount and
                To.Balance = To'Old.Balance + Amount);
   
   -- 计算利息（年利率5%）
   function Calculate_Interest(Acc : Account) return Money
      with
         Pre => Acc.Is_Active,
         Post => Calculate_Interest'Result >= 0;
   
   -- 实现存款操作
   procedure Deposit(Acc : in out Account; Amount : Money) is
   begin
      Acc.Balance := Acc.Balance + Amount;
   end Deposit;
   
   -- 实现取款操作
   procedure Withdraw(Acc : in out Account; Amount : Money) is
   begin
      Acc.Balance := Acc.Balance - Amount;
   end Withdraw;
   
   -- 实现转账操作
   procedure Transfer(From, To : in out Account; Amount : Money) is
   begin
      Withdraw(From, Amount);
      Deposit(To, Amount);
   end Transfer;
   
   -- 实现利息计算
   function Calculate_Interest(Acc : Account) return Money is
      Interest_Rate : constant := 0.05;  -- 5%年利率
   begin
      return Money(Float(Acc.Balance) * Interest_Rate);
   end Calculate_Interest;
   
   -- 打印账户信息
   procedure Print_Account(Acc : Account) is
   begin
      Put("账户 #");
      Put(Acc.Account_Number, Width => 0);
      Put(" - 余额: ");
      Put(Integer(Acc.Balance), Width => 0);
      Put(" 元");
      if not Acc.Is_Active then
         Put(" (已冻结)");
      end if;
      New_Line;
   end Print_Account;
   
   -- 测试账户
   Account_1 : Account := (Balance => 1000, Account_Number => 1, Is_Active => True);
   Account_2 : Account := (Balance => 500, Account_Number => 2, Is_Active => True);
   Account_3 : Account := (Balance => 200, Account_Number => 3, Is_Active => False);
   
begin
   Put_Line("===== Ada契约式编程演示 =====");
   New_Line;
   
   -- 显示初始状态
   Put_Line("初始账户状态：");
   Print_Account(Account_1);
   Print_Account(Account_2);
   Print_Account(Account_3);
   New_Line;
   
   -- 测试存款
   Put_Line("测试存款操作：");
   Put_Line("向账户 #1 存入 500 元");
   Deposit(Account_1, 500);
   Print_Account(Account_1);
   New_Line;
   
   -- 测试取款
   Put_Line("测试取款操作：");
   Put_Line("从账户 #2 取出 200 元");
   Withdraw(Account_2, 200);
   Print_Account(Account_2);
   New_Line;
   
   -- 测试转账
   Put_Line("测试转账操作：");
   Put_Line("从账户 #1 向账户 #2 转账 300 元");
   Transfer(Account_1, Account_2, 300);
   Print_Account(Account_1);
   Print_Account(Account_2);
   New_Line;
   
   -- 测试利息计算
   Put_Line("测试利息计算：");
   Put("账户 #1 的年利息：");
   Put(Integer(Calculate_Interest(Account_1)), Width => 0);
   Put_Line(" 元");
   Put("账户 #2 的年利息：");
   Put(Integer(Calculate_Interest(Account_2)), Width => 0);
   Put_Line(" 元");
   New_Line;
   
   -- 测试前置条件违反
   Put_Line("测试错误情况：");
   
   -- 1. 尝试存入负数金额
   begin
      Put_Line("尝试存入负数金额...");
      Deposit(Account_1, -100);
      Put_Line("操作成功（不应该到达这里）");
   exception
      when E : others =>
         Put_Line("预期的错误：" & Exception_Message(E));
   end;
   New_Line;
   
   -- 2. 尝试取出超过余额的金额
   begin
      Put_Line("尝试取出超过余额的金额...");
      Withdraw(Account_2, 1000);
      Put_Line("操作成功（不应该到达这里）");
   exception
      when E : others =>
         Put_Line("预期的错误：" & Exception_Message(E));
   end;
   New_Line;
   
   -- 3. 尝试从冻结账户操作
   begin
      Put_Line("尝试从冻结账户取款...");
      Withdraw(Account_3, 100);
      Put_Line("操作成功（不应该到达这里）");
   exception
      when E : others =>
         Put_Line("预期的错误：" & Exception_Message(E));
   end;
   New_Line;
   
   -- 4. 尝试向自己转账
   begin
      Put_Line("尝试向自己转账...");
      Transfer(Account_1, Account_1, 100);
      Put_Line("操作成功（不应该到达这里）");
   exception
      when E : others =>
         Put_Line("预期的错误：" & Exception_Message(E));
   end;
   
   Put_Line("");
   Put_Line("契约式编程演示完成");
   
exception
   when E : others =>
      Put_Line("发生未预期的错误：");
      Put_Line(Exception_Message(E));
end Contracts;
