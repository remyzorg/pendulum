


# Compilation

* depandancies : oasis, oUnit, ppx_deriving
* `make`


# Esterel Semantic

Solution à l'extrusion de portée : *connecion code*

# GRC intermediate format


# Selection Tree

Label correspondance :
* Squares : pauses
* # : exclusives
* parallel : ||


```ocaml

type selection_tree = {status : bool; label : int; t = tree}
and type tree =
| Pause
| Par of selection_tree list
| Excl of selection_tree list
| Ref of selection_tree


```

The status is the current selection status of the node : selected
(true) or not (false). Selected means the statement is currently
activated and paused, and if the program is resumed, it's resumed from
this statement.

If n exclusive nodes never have more than one selected child then the
valuation of the selection tags is said to be consistent. A valuation
is always consistent before an instant and after an
instant.

The selection flags are updated by state access primitives : *enter*,
*exit*, *test*, *switch* and *sync*. Those primitives are inside
flowgraph nodes. *Switch* carries a exclusive node and *sync* a
parellel node. *Enter* set the status of the selection node to `true`,
and *exit* set the status of the selection node and all it's children
to `false`


## Generation

* Pause : Leaf
* Sequential composition and tests (present/if) : exclusive nodes
* Parallel : parallel
* Composite statements (Loop, suspend, trap) : reference selection nodes
* nothing and emit : ignored


## Completion code

An Esterel program can be completed in three way :
* terminate its execution normally
* pause (retain the execution flow for one instant)
* exit by a trap

For each kind of termination we associate a completion code : 
* 0 : normal termination
* 1 : pause
* k + 2 : exit T where k is the number of traversed trap to reach Trap T

The parallel statement takes the maximum of all its branches

### Example

```
trap U in      	     => code 0
	trap T in		 => code 2
      nothing	     => code 0
	  ||		     => code 3
	  pause		     => code 1
	  ||		 
	  exit T	     => code 2
	  ||   		 
	  exit U	     => code 3
    end       		 
    ||		     	 => code 2
    exit U		     => code 2
end
;        			 => code 1 
pause	    		 => code 1 
```

# Flowgraph

Memo : 
* Tick : Triangle inversé
* Test : losange : point => else
* Join : triangle inversé
* Call : round corner
* Switch : triangle
* Sync : quadrilatère
* Fork : quadrilatère inversé

```ocaml

type flowgraph =
  | Tick of flowgraph
  | Test of signal * flowgraph * flowgraph
  | Join of flowgraph
  | Call of action
  | Switch of selection_tree * flowgraph list
  | Sync of selection_tree * flowgraph list
  | Fork of flowgraph list


```


Each nodes has input ports and output ports which are connected
through control arcs. If a node receives control from an input port, it
is executed. There are also signal input arcs on test nodes. A node is
executed if it receives control from ast least one arc. At the end of
its computation, it gives control to one of its outputs. One arc is a
link between an input and an ouput, but a output port of a node can be
connected to several arcs, and it's the same for input ports.

#### Tick

The root of the Flowgraph with only an output CONT.

#### Test

The Test node is generated from Present or IfElse. It has two outputs
nodes THEN and ELSE depending on the signal status bit. It bears
signal inputs ports.

#### Join

Reassemble control flows like branches of a test.
A single input GO and an output CONT.

#### Call

Just a node with GO and CONT. A function call or emit (state update)
is executed between the control receiving and control passing.

#### Switch

Choice between selection tree nodes depending of selection tree
valuation. It triggered a state decoding primitive after GO. A
*Switch* bears a selection node `s`. *Switch* has one output port for
every child of `s`.

#### Sync

Collect the **selection status** and **completion codes** of parallel
branches trough input ports and compute its own completion code.

A *Sync* node is associated with a parallel selection node.

* **Input**
 * completion code for each parallel branch indiced by selection
    node `Ki(br)` which is the completion level of `i` for the branch.
 * port for each child of the selection node with is the `Dead(br)`
    input. It carries the negated selection status. The statement
    `br` produces no completion code when `Dead(br) = 1`
* **Output** :
 * `Ki` (one output port per possible completion code)

After the completion code computation, it passes control to the
relevant handler through output completion port `K(i)`.

A **Sync** node may also carry a *sync* state access primitive that is used
in the code generation process. The computation of the *sync*
primitive is redundant in a **Sync** node but allow better encoding.

#### Fork

Construct to represent the places where control forks at ther
beggining of parellel branches. It is only a list of successors.


# Translation to GRC

First, the state access primitives must always be called by the
flowgraph in a “consistent” order. In particular, state flags are
decoded before they are modified within an instant.

## Translation interface

The translation to selection tree is trivial since it's juste a view
of the ast of *p*.

On the other side, the construction of the flowgraph is not
obvious. This graph is the fusion of two graphs generated by the
*depth* function and *surface* function. There type is

``env -> Ast.t -> Flowgraph.t -> Flowgraph.t -> Flowgraph.t``

The `Ast.t` is *p*, the first `Flowgraph.t` parameter is the **pause**
escaping of *p* which represent the end of the execution for the
current instant of *p* by pausing. The second `Flowgraph.t` parameter
is the **end** escaping of *p* : it's the flowgraph of the statement
following *p* execution.

The environnement is of type

```ocaml
type env = {
  exits : Flowgraph.t StringMap.t;
  under_suspend : bool;
  synctbl : (int * int, Flowgraph.t) Hashtbl.t
}
```

It contains a association table between labels and
exits. `under_suspend` gives the information wether it's under a
*suspend* statement or not. `synctbl` is an associated mutable table
from couple of ids to sync flowgraphs.

The flowgraph is a tree with up to two children.

```ocaml
type t =
    | Node_bin of node * t * t
    | Node of node * t
    | Leaf of node
```

The node tags are the different GRC constructions : 


```ocaml
type node =
  | Call of action
  | Test of test_value
  | Sync of int * int
  | Fork
  | Dep
```


Each function generates its flowgraph. The glue between them is a node
with a test on the selection state of the root node. The final end
node is a *call* which set `finished` to true.



## From statement to flowgraph

The symbole 

### Pause

surface :
```
enter p
	-> endp
	-> pause
```

depth : `endp`


### Await s

surface :
```
enter p
	-> test s (exit p -> endp,
	pause
)
```
depth : _


### Loop q

surface :
```
enter p ->
	surface q
		-> pause
		-> endp
```
depth :
```
depth q
	-> pause
	-> surface q
		-> pause
		-> pause
```

### Sequence q r

surface :
```
enter p
	-> surface q
		-> pause
		-> surface r
			-> pause
			-> endp
```

depth :
```
test_node q.id
	-> depth q
		-> pause
		-> surface r
			-> pause
			-> endp
    -> depth r
		-> pause
		-> endp
```



# GRC Interpretation

Steps:
* state update
* state decoding
