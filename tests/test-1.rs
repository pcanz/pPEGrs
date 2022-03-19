use p_peg::Peg;

#[test]
fn peg_peg() {

    let peg_grammar = r#"
        Peg   = " " (rule " ")+
        rule  = id " = " alt

        alt   = seq (" / " seq)*
        seq   = rep (" " rep)*
        rep   = pre sfx?
        pre   = pfx? term
        term  = call / sq / dq / chs / group / extn

        id    = [a-zA-Z_] [a-zA-Z0-9_]*
        pfx   = [&!~]
        sfx   = [+?] / '*' range?
        range = num (dots num?)?
        num   = [0-9]+
        dots  = '..'

        call  = id !" ="
        sq    = "'" ~"'"* "'" 'i'?
        dq    = '"' ~'"'* '"' 'i'?
        chs   = '[' ~']'* ']'
        group = "( " alt " )"
        extn  = '<' ~'>'* '>'

        _space_ = ('#' ~[\n\r]* / [ \t\n\r]+)*
    "#;

    let peg_peg = Peg::new(peg_grammar).unwrap();

    println!("{}", peg_peg);

    match peg_peg.parse(peg_grammar) {
        Ok(parse) => println!("peg parse:\n{}", parse),
        Err(err) => panic!("\n*** peg_peg ***:\n{}", err),
    }
}

