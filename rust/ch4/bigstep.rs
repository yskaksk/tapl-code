#![allow(dead_code)]

#[derive(Clone, Debug)]
enum Term {
    TmTrue,
    TmFalse,
    TmIf {
        cond: Box<Term>,
        cons: Box<Term>,
        alt: Box<Term>
    },
    TmZero,
    TmSucc {
        val: Box<Term>
    },
    TmPred {
        val: Box<Term>
    },
    TmIsZero {
        val: Box<Term>
    }
}

use Term::*;

fn isnumerical(t: &Term) -> bool {
    match t {
        TmZero => true,
        TmSucc { val } => isnumerical(val),
        _ => false,
    }
}

fn isval(t: &Term) -> bool {
    match t {
        TmTrue => true,
        TmFalse => true,
        t if isnumerical(t) => true,
        _ => false
    }
}

fn eval(t: &Term) -> Term {
    match t {
        t if isval(t) => t.clone(),
        TmIf { cond, cons, alt } => {
            match cond.as_ref() {
                TmTrue => eval(cons),
                TmFalse => eval(alt),
                _ => panic!(),
            }
        }
        TmSucc { val } => {
            if isnumerical(&eval(&val)) {
                return eval(val)
            } else {
                panic!()
            }
        }
        TmPred { val } => {
            let val1 = eval(val);
            match val1 {
                TmZero => val1,
                TmSucc { val } if isnumerical(val.as_ref()) => *val,
                _ => panic!(),
            }
        }
        TmIsZero { val } => {
            let val1 = eval(val);
            match val1 {
                TmZero => TmTrue,
                TmSucc { val } if isnumerical(val.as_ref()) => TmFalse,
                _ => panic!(),
            }
        }
        _ => panic!()
    }
}

fn main() {
    let t = TmIf {
        cond: Box::new(TmTrue),
        cons: Box::new(TmSucc {
            val: Box::new(TmZero)
        }),
        alt: Box::new(TmFalse)
    };
    let t1 = eval(&t);
    println!("{:?}", t1);
}

