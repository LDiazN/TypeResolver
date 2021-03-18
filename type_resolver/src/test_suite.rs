use crate::type_resolver::*;

// Shortcut to create list of tokens
#[allow(unused)]
fn s(s : &str) -> Vec<String> {
    s.to_string()
        .replace("(", " ( ")
        .replace(")", " ) ")
        .replace("->", " -> ")
        .split_whitespace()
        .map(|s| s.to_string()).collect()
}

// Expression shortcuts
#[allow(unused)]
fn p(exp : Expr) -> Expr {
    Expr::ParExpr(Box::new(exp))
}

#[allow(unused)]
fn f(l_exp : Expr, r_exp : Expr) -> Expr {
    let f = FuncApply{ function : l_exp, arg : r_exp };
    Expr::Apply( Box::new(f) )
}

#[allow(unused)]
fn a(name : &str) -> Expr {
    Expr::Atom(name.to_string())
}

// Type shortcuts
#[allow(unused)]
fn tf(t1 : Type, t2 : Type) -> Type {
    let f = Function{l_type : t1, r_type : t2};
    Type::Func(Box::new(f))
}

#[allow(unused)]
fn ta(name : &str) -> Type {
    if name.chars().next().unwrap().is_lowercase() {
        Type::Var(VarType{name : name.to_string()})
    }
    else {
        Type::Const(ConstType{name : name.to_string()})
    }
}

#[allow(unused)]
fn tp (my_type : Type) -> Type {
    Type::ParType(Box::new(my_type))
}

#[test]
fn test_expr_invalid_syntax() {

    // no arrow operator in expression
    let e1 = Expr::new(s("->"));

    // unmatching closing parenthesis
    let e2 = Expr::new(s("(f g))"));

    // unmatching openning parenthesis
    let e3 = Expr::new(s("((f g)"));

    // no empty expression
    let e4 = Expr::new(s(""));

    assert_eq!(e1, Err(TypeError::InvalidSyntax));
    assert_eq!(e2, Err(TypeError::InvalidSyntax));
    assert_eq!(e3, Err(TypeError::InvalidSyntax));
    assert_eq!(e4, Err(TypeError::InvalidSyntax));
}

#[test]
fn test_expr_valid_exp() {
    let e1 = Expr::new(s("s")).unwrap();
    
    let e2 = Expr::new(s("f g")).unwrap();
    
    let e3 = Expr::new(s("f (g h)")).unwrap();
    
    let e4 = Expr::new(s("(f g)")).unwrap();

    let e5 = Expr::new(s("(f g) h")).unwrap();

    let e6 = Expr::new(s("f g h")).unwrap();

    let ans1 = a("s");
    let ans2 = f( a("f"), a("g"));
    let ans3 = f(a("f"), p(f(a("g"), a("h"))));
    let ans4 = p(f( a("f"), a("g") ));
    let ans5 = f(  p(f(a("f"), a("g"))), a("h"));
    let ans6 = f(f(a("f"), a("g")), a("h"));

    assert_eq!(e1, ans1);
    assert_eq!(e2, ans2);
    assert_eq!(e3, ans3);
    assert_eq!(e4, ans4);
    assert_eq!(e5, ans5);
    assert_eq!(e6, ans6);
}

#[test]
fn test_expr_valid_name() {
    let mut manager = TypeManager::new();

    manager.add_type(&format!("0"), ta("Int"));

    assert_eq!(manager.get_type(&a("0")), Ok(ta("Int")));
}

#[test]
fn test_expr_invalid_name() {
    let manager = TypeManager::new();

    let err = Err(TypeError::UndefinedSymbol("0".to_string()));

    assert_eq!(manager.get_type(&a("0")), err);
}

#[test]
fn test_unmatching_type() {

    let mut manager = TypeManager::new();

    let int = ta("Int");
    let my_char = ta("Char");

    manager.add_type(&format!("0"), int.clone());
    manager.add_type(&format!("eq"), tf(int.clone(), int.clone()));
    manager.add_type(&format!("char"), my_char.clone());

    // Check function typechecks ok
    assert_eq!(manager.get_type(&Expr::new(s("eq 0")).unwrap()), Ok(int.clone()));

    // Check function typechecks unmatch
    assert_eq!(manager.get_type(&Expr::new(s("eq char")).unwrap()), Err(TypeError::UnmatchingTypes(int, my_char)));
}

#[test]
fn test_type_invalid_syntax() {
    let t1 = Type::new(s("f g"));
    let t2 = Type::new(s(""));
    let t3 = Type::new(s("(a -> b))"));
    let t4 = Type::new(s("-> a"));
    let t5 = Type::new(s("((a -> b) -> c"));
    let t6 = Type::new(s("_f"));
    
    let err = Err(TypeError::InvalidSyntax);

    assert_eq!(t1, err);
    assert_eq!(t2, err);
    assert_eq!(t3, err);
    assert_eq!(t4, err);
    assert_eq!(t5, err);
    assert_eq!(t6, err);
}

#[test]
fn test_type_valid_syntax() {
    let t1 = Type::new(s("a")).unwrap();
    let t2 = Type::new(s("a -> b")).unwrap();
    let t3 = Type::new(s("a -> b->c")).unwrap();
    let t4 = Type::new(s("(a -> b) -> c")).unwrap();
    let t5 = Type::new(s("(a)")).unwrap();
    let t6 = Type::new(s("a ->(a ->b)")).unwrap();
    let t7 = Type::new(s("Int")).unwrap();

    let ans1 = ta("a");
    let ans2 = tf(ta("a"), ta("b"));
    let ans3 = tf(ta("a"), tf(ta("b"), ta("c")));
    let ans4 = tf(tp(tf(ta("a"), ta("b"))), ta("c"));
    let ans5 = tp(ta("a"));
    let ans6 = tf(ta("a"), tp(tf(ta("a"), ta("b"))));
    let ans7 = ta("Int");

    assert_eq!(t1, ans1);
    assert_eq!(t2, ans2);
    assert_eq!(t3, ans3);
    assert_eq!(t4, ans4);
    assert_eq!(t5, ans5);
    assert_eq!(t6, ans6);
    assert_eq!(t7, ans7);

}