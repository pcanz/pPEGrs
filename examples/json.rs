/*
    The Json grammar illustrate a general application structure:

    - Define an 'Eval` trait with an `eval` function to evaluate a parse tree.
    - Match on the rule names of the parse tree nodes.
    - Translate the grammar matched text value into application data.

    The Json type is an enum of all the different types that
    the application needs to return from the `eval` function.
*/

use p_peg::{Peg, Parse, Ptree};

fn main() {

    let json_peg = Peg::new(r#"
        json   = " " value " "
        value  =  Str / Arr / Obj / num / lit
        Obj    = "{ " (memb (" , " memb)*)? " }"
        memb   = Str " : " value
        Arr    = "[ " (value (" , " value)*)? " ]"
        Str    = '"' chars* '"'
        chars  = ~[\u0000-\u001F"\\]+ / '\\' esc
        esc    = ["\\/bfnrt] / 'u' [0-9a-fA-F]*4
        num    = int frac?
        int    = '-'? ([1-9] [0-9]* / '0')
        frac   = _frac _exp?
        _frac  = '.' [0-9]+
        _exp   = [eE] [+-]? [0-9]+
        lit    = "true" / "false" / "null"
    "#).expect("Bad Json gramar...");

    let json_parse = json_peg.parse(r#"
        { "answer": 42,
          "mixed": [1, 2.3, "a\tstring", true, [4, 5]],
          "empty": {}
        }        
    "#);

    match json_parse {
        Ok(parse) => {
            println!("json parse:\n{}", parse);
            let json = parse.eval(parse.root());
            println!("json data:\n {:?}", json);
        },
        Err(err) => println!("{}", err),
    }

}   
 
use std::collections::HashMap;

#[derive(Debug)]
enum Json {
    Null,
    Bool(bool),
    Num(f64),
    Int(i64),
    Str(String),
    Arr(Vec<Json>),
    Obj(HashMap<String, Json>),
}

use Json::*;

trait Eval {
    fn eval(&self, node: &Ptree) -> Json;
}

impl<'a> Eval for Parse<'a> {
    fn eval(&self, node: &Ptree) -> Json {
        match self.name(node) {
            "Str" => {
                let segs: Vec<String> = self.args(node).iter()
                    .map(|arg| self.eval(arg).string())
                    .collect();
                return Str(segs.join(""));
            },
            "Arr" => {
                let items: Vec<Json> = self.args(node).iter()
                    .map(|arg| self.eval(arg))
                    .collect();
                return Arr(items);
            },
            "Obj" =>  {
                let membs: Vec<Json> = self.args(node).iter()
                    .map(|arg| self.eval(arg))
                    .collect();
                let mut obj: HashMap<String, Json> = HashMap::new();
                for memb in membs { // Memb(Arr([key, val]))
                    if let Arr(mut kv) = memb {
                        let key = &kv[0].string();
                        let val = kv.swap_remove(1);
                        obj.insert(key.to_string(), val);
                    }
                }
                return Obj(obj);
            },
            "memb" => {
                let membs: Vec<Json> = self.args(node).iter()
                    .map(|arg| self.eval(arg))
                    .collect();
                return Arr(membs);
            },
            "num" => { // int frac
                let args: Vec<String> = self.args(node).iter()
                    .map(|arg| self.text(arg).to_string())
                    .collect();
                let num = args.join("");
                return Num(num.parse::<f64>().expect("Bad grammar") as f64)
            },
            "int" => Int(self.text(node).parse::<i64>().expect("Bad grammar") as i64),
            "lit" =>  match self.text(node) {
                "true" => Bool(true),
                "false" => Bool(false),
                "null" => Null,
                _ => panic!("Bad grammar.."),
            },
            "chars" => Str(self.text(node).to_string()),
            "esc" => match self.text(node) {
                "t" => Str("\t".to_string()),
                "n" => Str("\n".to_string()),
                "r" => Str("\r".to_string()),
                x => panic!("Bad ecape: \\{}", x),
            },
            _ => panic!("Unexpected parse-tree node: {:?}", node),
        }
    }    
}

impl Json {
    fn string(&self) -> String {
        if let Str(s) = self { return s.to_string(); }
        panic!("Bad grammar");
    }
}

/* example output...
json parse:
["Obj" [["memb" [["Str" [["chars" "answer"]]]["int" "42"]]]
    ["memb" [["Str" [["chars" "mixed"]]]
        ["Arr" [["int" "1"]["num" [["int" "2"]["frac" ".3"]]]
            ["Str" [["chars" "a"]["esc" "t"]["chars" "string"]]]
            ["lit" "true"]
            ["Arr" [["int" "4"]["int" "5"]]]]]]]
    ["memb" [["Str" [["chars" "empty"]]]["Obj" []]]]]]
json data:
 Obj({
    "mixed": Arr([Int(1), Num(2.3), Str("a\tstring"), Bool(true), Arr([Int(4), Int(5)])]),    "empty": Obj({}),
    "answer": Int(42)
})
*/
