use std::env;

fn attempt(i: isize) -> Option<isize> {
    if i == 0 {
        Some(i)
    } else {
        match attempt(i - 1) {
            Some(x) => Some(x + 1),
            None => None,
        }
    }
}

fn run(n: isize) -> isize {
    match attempt(n) {
        Some(x) => x,
        None => -1,
    }
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
