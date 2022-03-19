/*
    This is an almost trivial example for processing a parse tree.

    It is intended to illustrate an application structure:

    - Defines an 'Eval` trait with an `eval` function to evaluate a parse tree.
    - Matches on the rule names of the parse tree nodes (only "date" here).
    - Translates the grammar matched text value into application data.

*/

use p_peg::{Peg, Parse, Ptree};

fn main() {

    let date_peg = Peg::new(r#"
        date  = year '-' month '-' day
        year  = [0-9]*4
        month = [0-9]*2
        day   = [0-9]*2
    "#).expect("or bad grammar definition");

    let date_parse = date_peg.parse("2022-05-06").expect("or bad example");

    let date = date_parse.eval(date_parse.root());

    println!("date: {:?}", date); // => date: Date(2020, 5, 6)
}

#[derive(Debug)]
struct Date(u16, u8, u8);

trait Eval {
    fn eval(&self, node: &Ptree) -> Date;
}

impl<'a> Eval for Parse<'a> {
    fn eval(&self, node: &Ptree) -> Date {
        assert!("date" == self.name(node));
        let ymd = self.args(node);
        Date(
            self.text(&ymd[0]).parse::<u16>().unwrap(),
            self.text(&ymd[1]).parse::<u8>().unwrap(),
            self.text(&ymd[2]).parse::<u8>().unwrap(),
        )
    }   
}

