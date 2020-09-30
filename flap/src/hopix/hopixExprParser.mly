%%

%public value_definition:
| LET id=located(identifier) s=option(preceded(COLON, located(type_scheme))) EQUAL e=located(expr) { DefineValue(SimpleValue(id, s, e)) }



%public expr:
| e = simple_expr                                         { e                                    }
| e = control_structure                                   { e                                    }
| e1=located(expr) SEMICOLON e2=located(expr)             { Sequence (match value e2 with
                                                                      | Sequence l ->  e1 :: l 
                                                                      | _          ->  [e1; e2]) }
| f = located(simple_expr) arg=located(simple_expr)       { Apply(f, arg)                        }
| BACKSLASH arg=located(pattern) ARROW body=located(expr) { Fun(FunctionDefinition(arg, body))   }
| REF e=located(expr)                                     { Ref e                                }   


function_definitions:
| FUN l=separated_nonempty_list(AND, function_definition) { RecFunctions(l) }
function_definition:
| s=option(preceded(COLON, located(type_scheme))) id=located(identifier) arg = located(pattern) EQUAL e=located(expr) { (id, s, FunctionDefinition(arg, e)) }


simple_expr:
| e = very_simple_expr          { e }
| e = binop(simple_expr, prio_0, simple_expr) { e }

very_simple_expr:
| e = very_very_simple_expr { e }
| e = binop(very_very_simple_expr, prio_1, very_very_simple_expr) { e }

very_very_simple_expr:
| e = atomic_expr    { e }
| LPAR e = expr RPAR { e }
| t = tuple          { t } 
| e = binop(very_very_simple_expr, prio_2, very_very_simple_expr) { e }


atomic_expr:
| v = variable                                { v         }
| l = located(literal)                        { Literal l }
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
| p = atomic_pattern                                               { p             }
| LPAR  l = separated_twolong_list(COMMA, located(pattern)) RPAR   { PTuple(l)     }
| branches = separated_twolong_list(PIPE, located(atomic_pattern)) { POr(branches) }

atomic_pattern:
| UNDERSCORE               { PWildcard }
| id = located(identifier) { PVariable id  }
| lit = located(literal)   { PLiteral lit  }


control_structure:
| IF LPAR cond=located(expr) RPAR LCBRACK body1=located(expr) RCBRACK ELSE LCBRACK body2=located(expr) RCBRACK { IfThenElse(cond, body1, body2) }
| WHILE LPAR cond=located(expr) RPAR LCBRACK body=located(expr) RCBRACK { While(cond, body) }
| SWITCH LPAR cond=located(expr) RPAR LCBRACK cases=switch_cases RCBRACK { Case(cond, cases) }

switch_cases:
| option(PIPE) cases=separated_nonempty_list(PIPE, located(switch_branch)) { cases }

switch_branch:
| p=located(pattern) ARROW e=located(expr) { Branch(p, e) }