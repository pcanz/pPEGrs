use p_peg::Peg;

#[test]
fn comments() {
    let date_peg = Peg::new(r#"
        # cooments test...
        date  = year '-' month '-' day
        # --- another comment
        year  = [0-9]*4  # year..
        month = [0-9]+
        day   = [0-9]*2  # day..
        # some last end
        # comment lines
    "#).unwrap();

    println!("{}", date_peg);

    let date_parse = date_peg.parse("2021-02-03");

    match date_parse {
        Ok(parse) => println!("date parse:\n{}", parse),
        Err(err) => panic!("\n*** date_peg ***:\n{}", err),
    }
}

#[test]
fn repeat_ranges() {
    let date_peg = Peg::new(r#"
        date  = year '-' month '-' day
        year  = [0-9]*4
        month = [0-9]*1..2
        day   = [0-9]*1..
    "#).unwrap();

    println!("{}", date_peg);

    let date_parse = date_peg.parse("2021-02-03");

    match date_parse {
        Ok(parse) => println!("date parse:\n{}", parse),
        Err(err) => panic!("\n*** date_peg ***:\n{}", err),
    }
}

#[test]
fn case_insensitive() {
    let ci = Peg::new(r#"
        s  = 'abCd'i "FooBar"i
    "#).unwrap();

    println!("{}", ci);

    let date_parse = ci.parse("ABCdfooBAR");

    match date_parse {
        Ok(parse) => println!("case:\n{}", parse),
        Err(err) => panic!("\n*** case ***:\n{}", err),
    }
}
