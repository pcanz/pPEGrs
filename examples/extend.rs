/* Extensions...

This example illustrates how an extension function is defined
and linked into the grammar.

*/

use p_peg::Peg;

fn main() {

    let date_peg = Peg::new(r#"
        date  = <start> year '-' month '-' day
        year  = [0-9]*4
        month = [0-9]*2
        day   = <prior month> [0-9]*2 
    "#).unwrap();

    let date_peg = date_peg.extend(extras);

    match date_peg.parse("2002-03-04") {
        Ok(parse) => println!("date parse:\n{}", parse),
        Err(err) => println!("date grammar: {}", err),
    }

    // -- <extension> functions ----------------------------------

    fn extras(pen: &mut p_peg::Pen, extra: String) -> bool {
        let args: Vec<&str> = extra.split(' ').collect();
        match args[0] {
            "start" => {
                println!("\nRunning <start> at: {:?}", pen.pos);
                println!("input: {} ", pen.input);
                true
            }
            "prior" => { // <prior rule>
                println!("\nRunning <{}> at: {:?}", extra, pen.pos);
                println!("Rule: {} matched: {:?}", args[1], pen.prior_match(args[1]));
                true
            }
            _ => {
                println!("\nRunning extra: <{}> at: {:?}", extra, pen.pos);
                true
            }
        }
    }
 
}