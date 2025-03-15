with Ada.Text_IO; use Ada.Text_IO;

procedure Advanced_Tasking is
   task type Worker;
   
   task body Worker is
   begin
      Put_Line("Worker task is running");
      delay 1.0;
      Put_Line("Worker task completed");
   end Worker;
   
   Task1 : Worker;
begin
   Put_Line("Main program started");
   delay 2.0;
   Put_Line("Main program completed");
end Advanced_Tasking;
