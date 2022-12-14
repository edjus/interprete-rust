fn main() {
    println!("- Hola, mundo!");
    print!("- My name is {}, James {}.\n- Hello, {}{}{}!", "Bond", "Bond", -2+2, 0, 3+2*2);
    println!();
    println!("- Hasta la vista, Baby!\t\tI'll be back...");
    println!("{}", if true {"- Lo dudo!\t\t\tBye!"} else {"- Obviamente!"});
    let x: i64 = -7;
    let limite: i64 = f64::pow(x as f64) as i64;

    println!("El doble es: {}", limite )
}
