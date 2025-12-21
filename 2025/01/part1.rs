fn main() {
    let mut d= 50;
    let mut c = 0;
    let mut input = std::fs::read_to_string("2025/01/input.txt").unwrap();
    input = input.replace("R", "").replace("L", "-");
    for line in input.lines() {
        let x: i32 = line.parse().unwrap();
        d = (d + x) % 100;
        if d == 0 {
            c += 1;
        }
    }
    println!("{c}");
}