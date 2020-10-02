%%

%public value_definition:
| v = vdefinition { DefineValue v }

vdefinition:
| LET   id=located(identifier) 
        s=option(preceded(COLON, located(type_scheme))) 
  EQUAL e=located(expr)                                 { SimpleValue(id, s, e) }
| d = function_definitions                              { d                     }

simple_vdefinition:
| LET   id=located(identifier) 
        s=option(preceded(COLON, located(type_scheme))) 
  EQUAL e=located(nodef_expr)                           { SimpleValue(id, s, e) }
| d = simple_function_definitions                              { d                     }

function_definitions:
| FUN l=separated_nonempty_list(AND, function_definition) { RecFunctions(l) }
function_definition:
| s=option(preceded(COLON, located(type_scheme))) id=located(identifier) arg = located(pattern) EQUAL e=located(expr) { (id, s, FunctionDefinition(arg, e)) }

simple_function_definitions:
| FUN l=separated_nonempty_list(AND, simple_function_definition) { RecFunctions(l) }
simple_function_definition:
| s=option(preceded(COLON, located(type_scheme))) id=located(identifier) arg = located(pattern) EQUAL e=located(nodef_expr) { (id, s, FunctionDefinition(arg, e)) }


%public expr:
| v=simple_vdefinition SEMICOLON
  e=located(nodef_expr)   { Define(v, e) }
| e=nodef_expr            { e            }
| e1=located(nodef_expr) SEMICOLON 
  e2=located(nodef_expr)                     { Sequence (match value e2 with
                                                   | Sequence l ->  e1 :: l 
                                                   | _          ->  [e1; e2]) }

nodef_expr:
| e=simple_expr                        { e                                    }
| e=control_structure                  { e                                    }
| BACKSLASH arg=located(pattern) ARROW 
            body=located(nodef_expr)         { Fun(FunctionDefinition(arg, body))   }
| REF e=located(nodef_expr)                  { Ref e                                }
| e1=located(simple_expr) COLONEQUAL 
  e2=located(simple_expr)              { Assign(e1, e2)                       }
| EXCLAMATION e=located(simple_expr)   { Read e                               }
| l=twolong_list(located(simple_expr)) { HopixASTHelper.expr_of_apply_list l  }


simple_expr:
| e = very_simple_expr                        { e             }
| e = binop(simple_expr, prio_0, simple_expr) { e             }

very_simple_expr:
| e = very_very_simple_expr { e }
| e = binop(very_simple_expr, prio_1, very_simple_expr) { e }

very_very_simple_expr:
| e = atomic_expr    { e }
| e = binop(very_very_simple_expr, prio_2, very_very_simple_expr) { e           }

atomic_expr:
| t = tuple                                   { t           }
| e=located(atomic_expr) DOT l=located(label) { Field(e, l) }
| LPAR e = expr RPAR                          { e           }
| v = variable                                { v           }
| l = located(literal)                        { Literal l   }
| const = located(constructor) type_args=option(type_argument_apply) args=ioption(constructor_arguments) 
                                              { Tagged(const, type_args, list_of_list_option args) }

%inline constructor_arguments:
| LPAR l=separated_nonempty_list(COMMA, located(simple_expr)) RPAR { l }

%inline tuple:
| LPAR  l = separated_twolong_list(COMMA, located(simple_expr)) RPAR { Tuple(l) }


%inline variable:
| id = located(identifier) types=option(type_argument_apply) { Variable (id, types) }

%inline binop(E1, OP, E2):
| e1 = located(E1) b = located(OP) e2 = located(E2) { Apply({value=Apply(b, e1); position= join e1.position e2.position},
                                                             e2                                                           ) }
%inline prio_2:
(* int -> int -> int *)
| p = located(STAR)  { Variable(with_val (binop_name STAR) p, None)  }
| p = located(SLASH) { Variable(with_val (binop_name SLASH) p, None) }

%inline prio_1:
(* int -> int -> int *)
| p = located(MINUS) { Variable(with_val (binop_name MINUS) p, None) }
| p = located(PLUS)  { Variable(with_val (binop_name PLUS) p, None)  }
(* int -> int -> bool *)
| p = located(EQUALQUESTION)       { Variable(with_val (binop_name EQUALQUESTION) p, None)       }
| p = located(LANGLEEQUALQUESTION) { Variable(with_val (binop_name LANGLEEQUALQUESTION) p, None) }
| p = located(RANGLEEQUALQUESTION) { Variable(with_val (binop_name RANGLEEQUALQUESTION) p, None) }
| p = located(LANGLEQUESTION)      { Variable(with_val (binop_name LANGLEQUESTION) p, None)      }
| p = located(RANGLEQUESTION)      { Variable(with_val (binop_name RANGLEQUESTION) p, None)      }

%inline prio_0:
(* bool -> bool -> bool *)
| p = located(DOUBLEAMPERSAND)   { Variable(with_val (binop_name DOUBLEAMPERSAND) p, None)   }
| p = located(PIPEPIPE) { Variable(with_val (binop_name PIPEPIPE) p, None) }

%inline literal:
| c = CHAR   { LChar c   }
| s = STRING { LString s }
| i = INT    { LInt i    }


pattern:
| p = atomic_pattern                                                    { p                         }
| LPAR  l=separated_twolong_list(COMMA, located(pattern)) RPAR          { PTuple(l)                 }
| branches = separated_twolong_list(PIPE, located(atomic_pattern))      { POr(branches)             }
| branches = separated_twolong_list(AMPERSAND, located(atomic_pattern)) { PAnd(branches)            }


record_pattern:
| l=located(label) EQUAL p=located(pattern) { (l, p) }

atomic_pattern:
| LPAR p = pattern RPAR    { p             }
| c=located(constructor)
  targs=option(type_argument_apply)
  l=option(delimited(LPAR, 
                     separated_nonempty_list(COMMA, located(pattern)), 
                     RPAR))                                              { PTaggedValue(c, targs, list_of_list_option l) }
| UNDERSCORE               { PWildcard     }
| id = located(identifier) { PVariable id  }
| lit = located(literal)   { PLiteral lit  }
| LCBRACK  l=separated_nonempty_list(COMMA, record_pattern) RCBRACK 
           t = option(type_argument_apply)                              { PRecord(l, t)             }


control_structure:
| IF LPAR cond=located(expr) RPAR 
  LCBRACK body1=located(expr) RCBRACK 
  ELSE 
  LCBRACK body2=located(expr) RCBRACK    { IfThenElse(cond, body1, body2) }
| FOR     id=located(identifier) IN 
  LPAR    e1=located(expr) 
    TO    e2=located(expr) 
  RPAR
  LCBRACK body=located(expr) RCBRACK     { For(id, e1, e2, body) }

| WHILE LPAR cond=located(expr) RPAR 
  LCBRACK    body=located(expr) RCBRACK  { While(cond, body) }
| DO LCBRACK body=located(expr) RCBRACK 
  WHILE LPAR cond=located(expr) RPAR     { While(cond, body) }
| SWITCH LPAR cond=located(expr) RPAR 
  LCBRACK     cases=switch_cases RCBRACK { Case(cond, cases) }

switch_cases:
| option(PIPE) cases=separated_nonempty_list(PIPE, located(switch_branch)) { cases }

switch_branch:
| p=located(pattern) ARROW e=located(expr) { Branch(p, e) }