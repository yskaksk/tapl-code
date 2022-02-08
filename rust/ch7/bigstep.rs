#![allow(dead_code)]
#![allow(non_snake_case)]

#[derive(Clone, Debug)]
enum Term {
    TmVar {
        val: i16,
        length: i16
    },
    TmAbs {
        term: Box<Term>,
        name: String
    },
    TmApp {
        lterm: Box<Term>,
        rterm: Box<Term>,
    }
}

use Term::*;

enum Binding {
    NameBind
}

type Context = Vec<String>;

fn printtm(ctx: &Context, t: &Term) -> String {
    match t {
        TmAbs { term, name } => {
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
    }
}

fn pickfreshname(ctx: &Context, x: &String) -> (Context, String) {
    if ctx.iter().any(|c| c == x) {
        let new_x = format!("{x}'");
        return pickfreshname(ctx, &new_x)
    } else {
        let mut new_ctx = ctx.clone();
        new_ctx.push(x.clone());
        return (new_ctx, x.clone())
    }
}

fn ctxlength(ctx: &Context) -> i16 {
    ctx.len() as i16
}

fn index2name(ctx: &Context, val: i16) -> String {
    if let Some(name) = ctx.get(val as usize) {
        return name.clone()
    } else {
        panic!()
    }
}

fn termShift(shift: i16, term: &Term) -> Term {
    walk_shift(0, shift, term)
}

fn walk_shift(censor_value: i16, shift: i16, t: &Term) -> Term {
    match t {
        TmVar { val, length } => {
            return if *val >= censor_value {
                TmVar { val: val+shift, length: length+shift }
            } else {
                TmVar { val: *val, length: length+shift }
            }
        }
        TmAbs { term, name } => {
            TmAbs {
                term: Box::new(walk_shift(censor_value+1, shift, term.as_ref())),
                name: name.to_string()
            }
        }
        TmApp { lterm, rterm } => {
            TmApp {
                lterm: Box::new(walk_shift(censor_value, shift, lterm.as_ref())),
                rterm: Box::new(walk_shift(censor_value, shift, rterm.as_ref()))
            }
        }
    }
}

// [j -> s]t
fn termSubst(j: i16, s: &Term, t: &Term) -> Term {
    walk_subst(0, j, s, t)
}

fn walk_subst(censor_value: i16, shift: i16, s: &Term, t: &Term) -> Term {
    match t {
        TmVar { val, length } => {
            return if *val == shift+censor_value {
                termShift(shift, s)
            } else {
                TmVar { val: *val, length: *length }
            }
        }
        TmAbs { term, name } => {
            TmAbs {
                term: Box::new(walk_subst(censor_value+1, shift, s, term.as_ref())),
                name: name.to_string()
            }
        }
        TmApp { lterm, rterm } => {
            TmApp {
                lterm: Box::new(walk_subst(censor_value, shift, s, lterm.as_ref())),
                rterm: Box::new(walk_subst(censor_value, shift, s, rterm.as_ref()))
            }
        }
    }
}

fn termSubstTop(s: &Term, t: &Term) -> Term {
    termShift(-1, &termSubst(0, &termShift(1, s), t))
}

fn isval(t: &Term) -> bool {
    match t {
        TmAbs { term: _, name: _ } => true,
        _ => false
    }
}

fn eval(ctx: &Context, t: &Term) -> Term {
    match t {
        TmAbs { term: _, name: _ } => t.clone(),
        TmApp { lterm, rterm } => {
            let v2 = eval(ctx, rterm.as_ref());
            match eval(ctx, lterm.as_ref()) {
                TmAbs { term, name: _ } => eval(ctx, &termSubstTop(&v2, term.as_ref())),
                _ => panic!() // ありえないはず
            }
        }
        _ => panic!()
    }
}

fn main() {
    let ctx = vec![];
    let app = TmApp {
        lterm: Box::new(TmAbs {
            term: Box::new(TmApp {
                lterm: Box::new(TmVar { val: 0, length: 1 }),
                rterm: Box::new(TmAbs {
                    term: Box::new(TmVar { val: 0, length: 2 }),
                    name: String::from("x")
                })
            }),
            name: String::from("s") }),
        rterm: Box::new(TmAbs {
            term: Box::new(TmVar { val: 0, length: 1 }),
            name: String::from("x")
        })
    };
    let evaluated = eval(&ctx, &app);
    let res = printtm(&ctx, &evaluated);
    println!("{}", res);
}
