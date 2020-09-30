%%

%public type_argument_apply:
| LANGLE args = separated_nonempty_list(COMMA, located(type_)) RANGLE { args }


%public define_type:
| TYPE id=located(type_constructor) args=option(type_argument_declaration) EQUAL t=type_definition { DefineType(id, 
                                                                                                     list_of_list_option args,
                                                                                                     t                         ) }

%public type_:
| con=type_constructor args=option(type_argument_apply)        { TyCon(con, list_of_list_option args) }
| t1 = located(type_) ARROW t2 = located(type_)                { TyArrow(t1, t2)                      }
| LPAR l = separated_nonempty_list(COMMA, located(type_)) RPAR { TyTuple(l)                           }
| var = type_variable                                          { TyVar var                            }

%public %inline constructor:
| id = UPPERCASE_ID { KId id }

type_argument_declaration:
| LANGLE args = separated_nonempty_list(COMMA, located(type_variable)) RANGLE { args }

type_definition:
| option(PIPE) l=separated_nonempty_list(PIPE, sum_type_constructor_definition) { DefineSumType(l) }

sum_type_constructor_definition:
| c = located(constructor) args = option(sum_type_constructor_arg_list) { (c, list_of_list_option args) }

sum_type_constructor_arg_list:
| LPAR l = separated_nonempty_list(COMMA, located(type_)) RPAR { l }

type_constructor:
| id=LOWERCASE_ID { TCon(id) }

type_variable:
| v = TYPE_VARIABLE { TId v }
