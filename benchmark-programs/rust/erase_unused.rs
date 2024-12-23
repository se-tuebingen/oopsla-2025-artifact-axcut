use std::env;
use std::rc::Rc;

enum List {
    Nil,
    Cons(isize, Rc<List>),
}

fn replicate(value: isize, n: isize) -> List {
    let mut list = List::Nil;
    for _ in 0..n {
        list = List::Cons(value, Rc::new(list));
    }
    list
}

fn useless(n: isize) -> isize {
    let mut i = 0;
    for j in 0..n {
        replicate(0, j);
        i += 1;
    }
    i
}

fn run(n: isize) -> isize {
    useless(n)
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
