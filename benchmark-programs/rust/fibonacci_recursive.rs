use std::env;

fn fibonacci(i: isize) -> isize {
    if i == 0 {
        i
    } else if i == 1 {
        i
    } else {
        fibonacci(i - 1) + fibonacci(i - 2)
    }
}

fn run(n: isize) -> isize {
    fibonacci(n)
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
