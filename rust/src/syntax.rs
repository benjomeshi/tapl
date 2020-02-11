pub enum Type {
    Tnat,
    Tbool,
}

#[derive(Debug)]
pub enum Term {
    Tru {
        info: String,
    },
    Fls {
        info: String,
    },
    Zero {
        info: String,
    },
    Succ {
        info: String,
        t: Box<Term>,
    },
    Pred {
        info: String,
        t: Box<Term>,
    },
    IsZero {
        info: String,
        t: Box<Term>,
    },
    IfTE {
        info: String,
        t1: Box<Term>,
        t2: Box<Term>,
        t3: Box<Term>,
    },
}
