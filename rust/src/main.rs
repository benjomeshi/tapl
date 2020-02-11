mod syntax;
use syntax::{Term, Type};

fn is_val(t: &Term) -> bool {
    match t {
        Term::Tru { .. } => true,
        Term::Fls { .. } => true,
        Term::Zero { .. } => true,
        _ => false,
    }
}

fn is_numeric(term: &Term) -> bool {
    match term {
        Term::Zero { .. } => true,
        Term::Succ { t, .. } => is_numeric(&*t),
        _ => false,
    }
}

fn eval1(term: Term) -> Term {
    if is_val(&term) {
        return term;
    }

    match term {
        Term::IfTE { t1, t2, t3, info } => match *t1 {
            Term::Tru { .. } => *t2,
            Term::Fls { .. } => *t3,
            _ => Term::IfTE {
                info,
                t1: Box::new(eval1(*t1)),
                t2,
                t3,
            },
        },
        Term::Succ { t, info } => Term::Succ {
            t: Box::new(eval1(*t)),
            info,
        },
        Term::Pred { t, info } => match *t {
            Term::Zero { .. } => *t,
            Term::Succ { t, .. } if is_numeric(&*t) => *t,
            _ => Term::Pred {
                info,
                t: Box::new(eval1(*t)),
            },
        },
        Term::IsZero { t, info } => match *t {
            Term::Zero { .. } => Term::Tru {
                info: String::new(),
            },
            Term::Succ { t, .. } if is_numeric(&*t) => Term::Fls {
                info: String::new(),
            },
            _ => Term::IsZero {
                info,
                t: Box::new(eval1(*t)),
            },
        },

        _ => panic!("No rule applies"),
    }
}

fn main() {
    let t: Term = Term::IfTE {
        info: String::new(),
        t1: Box::<Term>::new(Term::Tru {
            info: String::new(),
        }),
        t2: Box::<Term>::new(Term::Succ {
            info: String::new(),
            t: Box::<Term>::new(Term::Zero {
                info: String::new(),
            }),
        }),
        t3: Box::<Term>::new(Term::Zero {
            info: String::new(),
        }),
    };

    println!("{:?}", eval1(t));
}
