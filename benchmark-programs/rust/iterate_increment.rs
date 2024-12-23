use std::env;

fn iterate(i: isize, f: impl Fn(isize) -> isize, arg: isize) -> isize {
    let mut res = arg;
    for _ in 0..i {
        res = f(res);
    }
    res
}

fn run(n: isize) -> isize {
    iterate(n, |x| x + 1, 0)
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
