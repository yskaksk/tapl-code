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
        rterm: Box<Term>,
    },
    TmTrue,
    TmFalse,
    TmIf {
        cond: Box<Term>,
        cons: Box<Term>,
        alt: Box<Term>
    },
    TmLet {
        val: String,
        t1: Box<Term>,
        t2: Box<Term>
    }
}

use Term::*;

#[derive(Clone, Debug)]
enum Binding {
    NameBind,
    VarBind(Ty)
}

use Binding::*;
type Context = Vec<(String, Binding)>;

fn printtm(ctx: &Context, t: &Term) -> String {
    match t {
        TmAbs { term, ty: _, name } => {
            let (ctx_, x_) = pickfreshname(ctx, name);
            return format!("(lambda {x_}.{})", printtm(&ctx_, term.as_ref()))
        }
        TmApp { lterm, rterm } => {
            return format!("({} {})", printtm(ctx, lterm.as_ref()), printtm(ctx, rterm.as_ref()))
        }
        TmVar { val, length } => {
            if ctxlength(ctx) == *length {
                return index2name(ctx, *val)
            } else {
                String::from("[bad index]")
            }
        }
        TmTrue => String::from("True"),
        TmFalse => String::from("False"),
        TmIf { cond, cons, alt } => {
            return format!("If {} then {} else {}", printtm(ctx, cond.as_ref()), printtm(ctx, cons.as_ref()), printtm(ctx, alt.as_ref()))
        }
        TmLet { val, t1, t2 } => {
            return format!("let {} = {} in {}", val, printtm(ctx, t1.as_ref()), printtm(ctx, t2.as_ref()))
        }
    }
}

fn pickfreshname(ctx: &Context, x: &String) -> (Context, String) {
    if ctx.iter().any(|c| c.0.eq(x)) {
        let new_x = format!("{x}'");
        return pickfreshname(ctx, &new_x)
    } else {
        let new_ctx = addbinding(ctx.clone(), x.clone(), NameBind);
        return (new_ctx, x.clone())
    }
}

fn ctxlength(ctx: &Context) -> i32 {
    ctx.len() as i32
}

fn index2name(ctx: &Context, val: i32) -> String {
    if let Some(name) = ctx.get(val as usize) {
        return name.0.clone()
    } else {
        println!("{:?}", ctx);
        panic!()
    }
}

fn termShift(shift: i32, term: &Term) -> Term {
    walk_shift(0, shift, term)
}

fn walk_shift(censor_value: i32, shift: i32, t: &Term) -> Term {
    match t {
        TmVar { val, length } => {
            return if *val >= censor_value {
                TmVar { val: val+shift, length: length+shift }
            } else {
                TmVar { val: *val, length: length+shift }
            }
        }
        TmAbs { term, ty, name } => {
            TmAbs {
                term: Box::new(walk_shift(censor_value+1, shift, term.as_ref())),
                ty: ty.clone(),
                name: name.to_string()
            }
        }
        TmApp { lterm, rterm } => {
            TmApp {
                lterm: Box::new(walk_shift(censor_value, shift, lterm.as_ref())),
                rterm: Box::new(walk_shift(censor_value, shift, rterm.as_ref()))
            }
        }
        TmTrue => TmTrue,
        TmFalse => TmFalse,
        TmIf { cond, cons, alt } => {
            TmIf {
                cond: Box::new(walk_shift(censor_value, shift, cond.as_ref())),
                cons: Box::new(walk_shift(censor_value, shift, cons.as_ref())),
                alt: Box::new(walk_shift(censor_value, shift, alt.as_ref()))
            }
        }
        TmLet { val, t1, t2 } => {
            TmLet {
                val: val.clone(),
                t1: Box::new(walk_shift(censor_value, shift, t1.as_ref())),
                t2: Box::new(walk_shift(censor_value, shift, t2.as_ref()))
            }
        }
    }
}

// [j -> s]t
fn termSubst(j: i32, s: &Term, t: &Term) -> Term {
    walk_subst(0, j, s, t)
}

fn walk_subst(censor_value: i32, shift: i32, s: &Term, t: &Term) -> Term {
    match t {
        TmVar { val, length } => {
            return if *val == shift+censor_value {
                termShift(shift, s)
            } else {
                TmVar { val: *val, length: *length }
            }
        }
        TmAbs { term, ty, name } => {
            TmAbs {
                term: Box::new(walk_subst(censor_value+1, shift, s, term.as_ref())),
                ty: ty.clone(),
                name: name.to_string()
            }
        }
        TmApp { lterm, rterm } => {
            TmApp {
                lterm: Box::new(walk_subst(censor_value, shift, s, lterm.as_ref())),
                rterm: Box::new(walk_subst(censor_value, shift, s, rterm.as_ref()))
            }
        }
        TmTrue => TmTrue,
        TmFalse => TmFalse,
        TmIf { cond, cons, alt } => {
            TmIf {
                cond: Box::new(walk_subst(censor_value, shift, s, cond.as_ref())),
                cons: Box::new(walk_subst(censor_value, shift, s, cons.as_ref())),
                alt: Box::new(walk_subst(censor_value, shift, s, alt.as_ref()))
            }
        }
        TmLet { val, t1, t2 } => {
            TmLet {
                val: val.clone(),
                t1: Box::new(walk_subst(censor_value, shift, s, t1.as_ref())),
                t2: Box::new(walk_subst(censor_value, shift, s, t2.as_ref()))
            }
        }
    }
}

fn termSubstTop(s: &Term, t: &Term) -> Term {
    termShift(-1, &termSubst(0, &termShift(1, s), t))
}

fn isval(t: &Term) -> bool {
    match t {
        TmAbs { term: _, ty: _, name: _ } => true,
        TmTrue | TmFalse => true,
        _ => false
    }
}

fn eval(ctx: &Context, t: &Term) -> Term {
    match t {
        TmAbs { term: _, ty: _, name: _ } => t.clone(),
        TmApp { lterm, rterm } => {
            let v2 = eval(ctx, rterm.as_ref());
            match eval(ctx, lterm.as_ref()) {
                TmAbs { term, ty: _, name: _ } => eval(ctx, &termSubstTop(&v2, term.as_ref())),
                _ => panic!() // ありえないはず
            }
        }
        TmIf { cond, cons, alt } => {
            match cond.as_ref() {
                TmTrue => eval(ctx, cons.as_ref()),
                TmFalse => eval(ctx, alt.as_ref()),
                _ => panic!()
            }
        }
        // TmLet 評価部分
        TmLet { val, t1, t2 } => {
            if isval(t1) {
                return eval(ctx, &termSubstTop(t1, t2.as_ref()))
            } else {
                return TmLet {
                    val: val.clone(),
                    t1: Box::new(eval(ctx, t1.as_ref())),
                    t2: t2.clone()
                }
            }
        }
        _ => {
            t.clone()
        }
    }
}

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
        // TmLet の型判定
        TmLet { val: _, t1, t2 } => {
            let ty1 = typeof_term(ctx, t1.as_ref());
            let new_ctx = addbinding(ctx.clone(), String::from("x"), VarBind(ty1));
            typeof_term(&new_ctx, t2)
        }
    }
}

fn main() {
    let ctx = vec![];
    let tmlet = TmLet {
        val: String::from("x"),
        t1: Box::new(TmFalse),
        t2: Box::new(TmApp {
            lterm: Box::new(TmAbs {
                term: Box::new(
                    TmVar {
                        val: 0,
                        length: 1
                    }
                ),
                ty: TyBool,
                name: String::from("y")
            }),
            rterm: Box::new(TmVar {
                val: 0,
                length: 1
            })
        })
    };
    let evaluated = eval(&ctx, &tmlet);
    let res = printtm(&ctx, &evaluated);
    println!("{}", res);
}

