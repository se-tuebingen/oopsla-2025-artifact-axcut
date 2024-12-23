use std::env;
use std::rc::Rc;

enum List {
    Nil,
    Cons(isize, Rc<List>),
}

fn range(i: isize, n: isize) -> List {
    if i < n {
        List::Cons(i, Rc::new(range(i + 1, n)))
    } else {
        List::Nil
    }
}

fn sum(list: &List) -> isize {
    match list {
        List::Nil => 0,
        List::Cons(y, ys) => y + sum(ys),
    }
}

fn run(n: isize) -> isize {
    sum(&range(0, n))
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let arg = match args.get(1) {
        None => 10,
        Some(val) => match val.parse::<isize>() {
            Ok(val) => val,
            Err(_) => {
                println!("argument is not a number");
                return;
            }
        },
    };

    println!("{}", run(arg));
}
