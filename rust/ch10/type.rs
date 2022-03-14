#![allow(dead_code)]
#![allow(non_snake_case)]

#[derive(Clone, Debug, PartialEq, Eq)]
enum Ty {
    TyArr {
        left: Box<Ty>,
        right: Box<Ty>
    },
    TyBool
}

use Ty::*;

#[derive(Clone, Debug)]
enum Term {
    TmVar {
        val: i32,
        length: i32
    },
    TmAbs {
        term: Box<Term>,
        ty: Ty,
        name: String
    },
    TmApp {
        lterm: Box<Term>,
        rterm: Box<Term>
    },
    TmTrue,
    TmFalse,
    TmIf {
        cond: Box<Term>,
        cons: Box<Term>,
        alt: Box<Term>
    },
}

use Term::*;

#[derive(Clone)]
enum Binding {
    NameBind,
    VarBind(Ty)
}

use Binding::*;

type Context = Vec<(String, Binding)>;

fn addbinding(ctx: Context, x: String, bind: Binding) -> Context {
    let mut res = vec![(x, bind)];
    res.append(&mut ctx.clone());
    return res
}

fn getbinding(ctx: &Context, i: i32) -> Binding {
    if let Some((_, b)) = ctx.get(i as usize) {
        return b.clone()
    } else {
        panic!()
    }
}

fn getTypeFromContext(ctx: &Context, i: i32) -> Ty {
    match getbinding(ctx, i) {
        VarBind(ty) => return ty,
        _ => panic!()
    }
}

// typeofは予約語
fn typeof_term(ctx: &Context, t: &Term) -> Ty {
    match t {
        TmVar { val, length: _ } => {
            return getTypeFromContext(ctx, *val)
        }
        TmAbs { term, ty, name } => {
            let ctx2 = addbinding(ctx.clone(), name.to_string(), VarBind(ty.clone()));
            let ty2 = typeof_term(&ctx2, term);
            return TyArr {
                left: Box::new(ty.clone()),
                right: Box::new(ty2)
            }
        }
        TmApp { lterm, rterm } => {
            let lty = typeof_term(ctx, lterm);
            let rty = typeof_term(ctx, rterm);
            match lty {
                TyArr { left, right } => {
                    if *left == rty {
                        return *right
                    } else {
                        panic!()
                    }
                },
                _ => panic!()
            }
        }
        TmTrue | TmFalse => TyBool,
        TmIf { cond, cons, alt } => {
            match typeof_term(ctx, cond) {
                TyBool => {
                    let ty2 = typeof_term(ctx, cons);
                    let ty3 = typeof_term(ctx, alt);
                    if ty2 == ty3 {
                        return ty2
                    } else {
                        panic!()
                    }
                }
                _ => panic!()
            }
        }
    }
}

fn main() {
    let ctx: Context = vec![];
    let abs = TmAbs {
        term: Box::new(TmIf { cond: Box::new(TmTrue), cons: Box::new(TmTrue), alt: Box::new(TmFalse) }),
        ty: TyBool,
        name: String::from("x")
    };
    let app = TmApp {
        lterm: Box::new(abs),
        rterm: Box::new(TmFalse),
    };
    let r = typeof_term(&ctx, &app);
    println!("{:?}", r)
}
