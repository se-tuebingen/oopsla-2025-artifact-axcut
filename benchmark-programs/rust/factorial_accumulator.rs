use std::env;

fn factorial(i: isize) -> isize {
    let mut acc = 1;
    for j in 1..(i + 1) {
        acc = (j * acc) % 1000000007
    }
    acc
}

fn run(n: isize) -> isize {
    factorial(n)
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
