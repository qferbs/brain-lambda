use crate::parsing::parse_lit;
use crate::parsing::strip_comments;
use regex::Regex;
use std::convert::TryInto;
use std::fs::File;
use std::io::Write;
use std::process::Command;

const FILE_PATH: &str = "./brainfuck-compiler/bf-gen-exp.b";
const BRAINFUCK_CHARS: [char; 9] = ['>', '<', '+', '-', '[', ']', '.', ',', '!'];
const SPECIAL_CHARS: [char; 10] = ['>', '<', '+', '-', '[', ']', '.', ',', '!', 'r'];

// runs brainfuck except that large stacks of operations can be entered as 50+, 'c'-, ect.
pub fn compile_and_run_augmented_brainfuck(program: String) {
    // strips out any comments
    let program = strip_comments(program);
    let re = Regex::new(r"[\n\r\t ]").unwrap();
    let program = re.replace_all(&program, "");
    let mut cur_reg: i32 = 0;
    println!("program: \n{}", program);

    let mut compiled = String::new();
    let mut chars = program.chars().peekable();

    while let Some(ch) = chars.next() {
        if BRAINFUCK_CHARS.contains(&ch) {
            push_str_track(&mut compiled, &mut cur_reg, &char::to_string(&ch));
        } else if ch == 'r' {
            let mut reg_lit = String::new();

            while chars
                .peek()
                .and_then(|c| Some(!SPECIAL_CHARS.contains(c)))
                .unwrap_or(false)
            {
                reg_lit.push(chars.next().unwrap());
            }

            let mut num_chars =
                parse_lit(&reg_lit).expect("Illegal register.") as i32 - cur_reg;
            let mut dir_char = '>';
            if num_chars < 0 {
                num_chars = -num_chars;
                dir_char = '<';
            }

            let extension = std::iter::repeat(dir_char)
                .take(num_chars as usize)
                .collect::<String>();
            push_str_track(&mut compiled, &mut cur_reg, &extension);
        } else {
            let mut lit = String::new();

            while chars
                .peek()
                .and_then(|c| Some(!SPECIAL_CHARS.contains(c)))
                .unwrap_or(false)
            {
                lit.push(chars.next().unwrap());
            }
            let rep_char = chars.next().expect("Prefix must go before an operator.");
            let num_chars = parse_lit(&lit).expect("Illegal prefix.");

            let extension = std::iter::repeat(rep_char)
                .take(num_chars as usize)
                .collect::<String>();
            push_str_track(&mut compiled, &mut cur_reg, &extension);
        }
    }

    println!("expanded program:\n{}", compiled);

    let mut file = File::create(FILE_PATH).unwrap();
    file.write_all(compiled.as_bytes()).unwrap();
    file.sync_all().unwrap();

    let output = Command::new("./brainfuck-compiler/bffsree_gcc.exe")
        .arg(FILE_PATH)
        .output()
        .unwrap();
    for line in String::from_utf8(output.stdout).unwrap().lines() {
        println!("{}", line);
    }
}

fn push_str_track(string: &mut String, cur_reg: &mut i32, new_str: &str) {
    for ch in new_str.chars() {
        match ch {
            '<' => *cur_reg -= 1,
            '>' => *cur_reg += 1,
            _ => (),
        }
    }

    string.push_str(new_str);
}

// returns expansion
fn parse_debug_annotation(lit: &str, cur_reg: &mut i32) -> String {
    let mut num: i32;
    let rep: char;
    let mut append = String::new();

    if lit.chars().next().unwrap() == '@' {
        println!("{}", lit);
        let reg: i32 = lit[1..lit.len() - 1].parse().expect("Illegal register.");
        append = lit.chars().last().unwrap().to_string();
        num = reg - *cur_reg;
        if num > 0 {
            rep = '>';
        } else {
            rep = '<';
            num = -num;
        }
        *cur_reg = reg;
    } else {
        num = parse_lit(&lit[..lit.len() - 1]).expect("Illegal prefix value.") as i32;
        rep = lit.chars().last().unwrap();
    }
    let mut out: String = std::iter::repeat(rep)
        .take(num.try_into().unwrap())
        .collect();
    out.push_str(&append);
    out
}
