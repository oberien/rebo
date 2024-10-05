use clap::Parser;
use rebo::ReturnValue;

mod xdot;

#[derive(clap::Parser)]
struct Args {
    #[clap(long)]
    type_graph_before: bool,
    #[clap(long)]
    type_graph_after: bool,
    #[clap(long, long = "gen")]
    generator: Vec<String>,
    #[clap(default_value = "main.re")]
    filename: String,
}

fn main() {
    env_logger::init();
    let args = Args::parse();
    let code = std::fs::read_to_string(&args.filename).unwrap();
    let result = rebo::run(args.filename, code);
    let mut children = Vec::new();
    if args.type_graph_before && result.type_graph_before.is_some() {
        children.push(xdot::xdot(result.type_graph_before.as_deref().unwrap()));
    }
    if args.type_graph_after && result.type_graph_after.is_some() {
        children.push(xdot::xdot(result.type_graph_after.as_deref().unwrap()));
    }
    for generator in args.generator {
        match result.generators.get(&generator) {
            Some((graph_dot, code)) => {
                println!("{code}");
                children.push(xdot::xdot(graph_dot));
            },
            None => println!("generator {generator} not found"),
        }
    }
    for mut child in children {
        child.wait().unwrap();
    }
    match result.return_value {
        ReturnValue::Ok(val) => println!("Result Value: {val:?}"),
        ReturnValue::ParseError => ::std::process::exit(2),
        ReturnValue::Diagnostics(_) => ::std::process::exit(3),
        ReturnValue::Panic => ::std::process::exit(4),
    }
}
