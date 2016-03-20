
# TODO

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
