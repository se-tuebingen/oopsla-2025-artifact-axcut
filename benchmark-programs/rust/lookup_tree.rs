use std::env;
use std::rc::Rc;

#[derive(Clone)]
enum Tree {
    Leaf(isize),
    Node(Rc<Tree>, Rc<Tree>),
}

fn create(i: isize, n: isize) -> Tree {
    if i < n {
        let tree = Rc::new(create(i + 1, n));
        Tree::Node(tree.clone(), tree)
    } else {
        Tree::Leaf(n)
    }
}

fn lookup(tree: &Tree) -> isize {
    match tree {
        Tree::Leaf(value) => *value,
        Tree::Node(left, _) => lookup(left),
    }
}

fn run(n: isize) -> isize {
    lookup(&create(0, n))
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
