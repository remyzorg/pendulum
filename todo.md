
# TODO


* Implement the scheduling the scheduling techniques of CEC
 
* Fix escaping problems :
An exit can't be linked to a not outside the fork-sync it is created. 
The solution is to link 'exit' to the sync and bind the sync to the 
correct escaping node.

 

* Test files based on Esterel examples using pendulum.compiler lib
 * parser from Esterel to pendulum ast
 * script to compare the results
 * test files from CEC ?

* Object oriented program interface :
 ```ocaml
 
class virtual program_instance :
 object
   method virtual react : unit
 end
   
class virtual program :
object
  method virtual new_program : program_instance
end
  
let%sync p =
  input s;
  loop begin
    present s !(print_string "hello\n");
    pause
  end
  
(* =====> *)   
  
let p_obj =
  object
    inherit program
    method new_program = 
      object
        method react = p~inst ()
      end  
  end
  
 ```
 
* Run
