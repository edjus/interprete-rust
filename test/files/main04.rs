use std::io;
use std::io::Write;
use std::process;

fn mcd(mut x: i32, mut y: i32) -> i32 {
    if x <= 0 || y <= 0 {
        println!("ERROR: El algoritmo requiere dos numeros enteros positivos!");
        process::exit(1);
    }

    while x != y {
        if x < y {
            y -= x;
        }
        if y < x {
            x -= y;
        }
    }

    x
}

fn main() {
    println!("******************************************************************************");
    println!("Se ingresan dos valores enteros positivos, se muestra su maximo comun divisor.");
    println!("Se utiliza el algoritmo de Euclides.");
    println!("******************************************************************************");

    print!("x: ");
    io::stdout().flush().expect("Error de escritura!");

    let mut renglon: String = String::new();
    io::stdin()
        .read_line(&mut renglon)
        .expect("Error de lectura!");
    let x: i32 = renglon
        .trim()
        .parse::<i32>()
        .expect("Se esperaba un numero entero!");

    print!("y: ");
    io::stdout().flush().expect("Error de escritura!");

    renglon = String::new();
    io::stdin()
        .read_line(&mut renglon)
        .expect("Error de lectura!");
    let y: i32 = renglon
        .trim()
        .parse::<i32>()
        .expect("Se esperaba un numero entero!");

    print!("{} es el MCD entre ", mcd(x, y));

    println!("{} y {}", x, y);
}
