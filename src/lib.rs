//! This p_peg crate is an implementation of pPEG (portable Parser Expression Grammar).
//!
//! For documentation see: <https://github.com/pcanz/pPEG>

//! # Example

//!```rust
//!#    use p_peg::Peg;
//!     let date_peg = Peg::new(r#"
//!         date  = year '-' month '-' day
//!         year  = [0-9]*4
//!         month = [0-9]*2
//!         day   = [0-9]*2
//!     "#).unwrap();
//!     
//!     let parse = date_peg.parse("2022-03-04").unwrap();
//!     
//!     println!("parse-tree: {}", parse);
//!
//!     // parse-tree: ["date", [["year", "2022"], ["month", "03"], ["day", "04"]]]
//!```     

/// The PEG grammar grammar.
pub static PEG_GRAMMAR: &str = r#"
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

// -- Parser Machine ------------------------------------------------

/// A parser for a grammar.
#[derive(Debug)]
pub struct Peg {
    names: Vec<String>,
    rules: Vec<Op>,
    space: Op,
}

/// A parse-tree result.
#[derive(Debug)]
pub struct Parse<'a> {
    input: &'a str,
    names: &'a Vec<String>,
    tree: Box<Ptree>,
}

/// A parse tree node. 
#[derive(Debug)]
pub struct Ptree {
    tag: u8,
    start: usize,
    end: usize,
    args: Vec<Ptree>,
}

/// An error report.
pub struct PegErr<'a> {
    ok: bool,
    input: &'a str,
    cursor: usize,
    rule: &'a str,
    expect: String,
    error: String,
}

/// An operator instruction.
#[derive(Debug, Clone)]
enum Op {
    Id(u8),
    Alt(Vec<Box<Op>>),
    Seq(Vec<Box<Op>>),
    Rep(Box<Op>, u8, u8),
    Pre(char, Box<Op>),
    Sq(String),
    Sqi(String),
    // Dq(String),
    Chs(Vec<char>, bool, u8, u8),
    Ext(String),
    Sp,
    Fail(String),
}

use Op::*;

/// An environment for the parser machine.
pub struct Pen<'a> {
    pub input: &'a str,
    pub pos: usize,
    pub tree: Vec<Ptree>,

    code: &'a Peg,
    extend: fn(&mut Pen, String) -> bool,
    depth: usize,
    calls: [u8; 128],
    peak: usize,
    fault_pos: usize,
    fault_rule: u8,
    fault_expect: &'a Op,
    trace: bool,
    trace_pos: i32, 
}

impl<'p> Pen<'p> {

    fn parse<'a>(code: &'a Peg, input: &'a str, //) -> Result<Parse<'a>, PegErr<'a>>  {
            extend: fn(&mut Pen, String) -> bool ) -> Result<Parse<'a>, PegErr<'a>>  {
        let mut p = Pen {
            input,
            pos: 0,
            tree: vec![],
            // private ------
            code,
            extend,
            depth: 0,
            calls: [0; 128],
            peak: 0,
            fault_pos: 0,
            fault_rule: 0,
            fault_expect: &code.rules[0],
            trace: false, //flag == 1,
            trace_pos: -1,       
        };
        let ok = p.run(&Id(0));
        if p.trace {
            println!("");
        }
        if !ok || p.pos < p.input.len() {
            let cursor = if ok {
                p.pos
            } else {
                std::cmp::max(p.pos, p.peak)
            };
            let rule = &p.code.names[p.fault_rule as usize];
            let expect = code.op_display(p.fault_expect);
            let error = String::new(); // for compile errors
            return Err(PegErr{ok, input: &p.input, cursor, rule, expect, error});
        }
        assert_eq!(p.tree.len(), 1);
        let root = p.tree.swap_remove(0);
        Ok(Parse{input: &p.input, names: &p.code.names, tree: Box::new(root)})
    }
    
    fn run(&mut self, op: &'p Op) -> bool {
        match op {
            Id(idx) => {
                let start = self.pos;
                let stack = self.tree.len();
                let name = &self.code.names[*idx as usize];
                if self.trace {
                    self.trace_report(&name, 0);
                }
                self.calls[self.depth] = *idx;
                self.depth += 1;
                let code = &self.code.rules[*idx as usize];
                let result = self.run(code);
                self.depth -= 1;
                if !result {
                    if self.trace {
                        self.trace_report(&name, 1);
                    }
                    return false;
                }
                let arity = self.tree.len() - stack;
                let first = name.as_bytes()[0];
                if first == 0x5F { // _anon 
                    if arity > 0 { //self.tree.len() > stack {
                        self.tree.truncate(stack);
                    }
                    return true;
                }
                if arity > 1 || first < 0x61 { // cap rule name
                    let args = self.tree.split_off(stack);
                    // self.tree.push(Ptree::Top(*idx, Box::new(args)));
                    self.tree.push(Ptree{tag:*idx, start, end:self.pos, args});
                    if self.trace {
                        self.trace_report(&name, 2);
                    }
                } else if arity == 0 { // terminal rule, text match
                    // self.tree.push(Ptree::Tip(*idx, start, (self.pos-start) as u32));
                    self.tree.push(Ptree{tag:*idx, start, end:self.pos, args:Vec::new()});
                    if self.trace {
                        self.trace_report(&name, 3);
                    }
                }    
                true // elide, arity == 1 && NOT cap rule
            }
            Alt(args) => {
                let start = self.pos;
                let stack = self.tree.len();
                for arg in args {
                    let result = self.run(arg);
                    if result {
                        return true;
                    }
                    if self.pos > self.peak {
                        self.peak = self.pos;
                    }
                    self.pos = start; // reset
                    if self.tree.len() > stack {
                        self.tree.truncate(stack);
                    }
                }
                false
            }
            Seq(args) => {
                let start = self.pos;
                let stack = self.tree.len();
                for arg in args {
                    let result = self.run(arg);
                    if !result {
                        if self.pos > start && self.pos > self.fault_pos {
                            self.fault_pos = self.pos;
                            self.fault_rule = self.calls[self.depth-1];
                            self.fault_expect = &*arg;
                        }
                        if self.pos > self.peak {
                            self.peak = self.pos;
                        }
                        self.pos = start;
                        if self.tree.len() > stack {
                            self.tree.truncate(stack);
                        }
                        return false;
                    }
                }
                true
            }
            Rep(expr, min, max) => {
                let mut count = 0;
                loop {
                    let start = self.pos;
                    if !self.run(&expr) {
                        break;
                    }
                    if self.pos == start { // no progress
                        break;
                    }
                    count += 1;
                    if count == *max { // max 0 means any
                        break;
                    }
                }
                if count < *min {
                    return false;
                }
                true
            }
            Pre(pfx, expr) => {
                let start = self.pos;
                let stack = self.tree.len();
                let result = self.run(expr);
                self.pos = start;
                if self.tree.len() > stack {
                    self.tree.truncate(stack);
                }
                if *pfx == '~' {
                    if result {
                        return false;
                    }
                    self.pos += 1;
                    while !self.input.is_char_boundary(self.pos) {
                        self.pos += 1; // to next Unicode or end
                    }
                    return true;
                }
                if *pfx == '!' {
                    return !result;
                }
                result         
            }
            Sq(str) => {
                if self.input[self.pos..].starts_with(str) {
                    self.pos += str.len();
                    return true;
                }
                false
            }
            Sqi(str) => { // TODO...
                let t = &self.input[self.pos..self.pos+str.len()];
                if &t.to_uppercase() == str {
                // if self.input[self.pos..].starts_with(str) {
                    self.pos += str.len();
                    return true;
                }
                false
            }
            Chs(cs, neg, min, max) => {
                let mut count = 0;
                loop { // min..max
                    let txt = self.input[self.pos..].chars().next();
                    if txt == None {
                        return count >= *min;
                    }
                    let t = txt.unwrap(); // target input text char
                    let mut hit = true;
                    let mut i = 0;
                    loop { // cs char ranges..
                        if i >= cs.len() {
                            hit = false;
                            break;
                        }
                        let c = cs[i];
                        if i+2 < cs.len() && cs[i+1] == '-' {
                            if t < c || t > cs[i+2] {
                                i += 3;
                                continue;
                            }
                            break; // ok t is in range
                        }
                        if c == t {
                            break;
                        }
                        i += 1;
                    }
                    if *neg { // ~chs
                        hit = !hit;
                    }
                    if !hit {
                        break;
                    }
                    count += 1;
                    self.pos += t.len_utf8();
                    if count == *max {
                        break;
                    }
                } // min..max loop
                if count < *min {
                    return false;
                }
                true
            } // Chs
            Ext(cmd) => self.extras(&cmd),
            Sp => self.run(&self.code.space),
            Fail(_) => false
        } // match op
    } // run
} // Pen impl

// -- <extra> extensions ----------------------------------------

impl<'p> Pen<'p> {
    fn extras(&mut self, cmd: &String) -> bool {
        let args: Vec<&str> = cmd.split(' ').collect();
        match args[0] {
            "?" => {
                self.trace = true;
                true
            }
            "@" | "same" =>  self.same_again(args[1]),
            _ => {
                (self.extend)(self, cmd.to_string())
            }
        }
    }

    pub fn prior_match(&self, name: &str)-> Option<String> {
        let idx = self.code.names.iter().position(|s| s == name);
        if idx == None { return None; } // no such rule name
        let id = idx.unwrap() as u8;
        let mut t = self.tree.len()-1;
        while t > 0 {
            let node = &self.tree[t];
            if node.tag == id {
                return Some(self.input[node.start..node.end].to_string());
            }
            t -= 1;
        }
        None
    }
    
    fn same_again(&mut self, name: &str)-> bool {
        let idx = self.code.names.iter().position(|s| s == name);
        if idx == None { return false; } // no such rule name
        let id = idx.unwrap() as u8;
        let mut t = self.tree.len()-1;
        loop {
            let node = &self.tree[t];
            if node.tag == id {
                let s = &self.input[node.start..node.end];
                if self.input[self.pos..].starts_with(s) {
                    self.pos += s.len();
                    return true; 
                };
                return false; // mismatch                
            }
            if t == 0 { break; }
            t -= 1;
        }
        return true; // match '' when there is no prior match
    }        

} // Pen impl


// -- parser results ----------------------------------------

impl<'a> Parse<'a>  {
    /// Root node of a parse-tree.
    pub fn root(&self) -> &Ptree {
        &self.tree
    }
    /// rule name of the parse-tree node.
    pub fn name(&self, node: &Ptree) -> &str {
        &self.names[node.tag as usize][..]
    }
    /// string matched by a terminal leaf node.
    pub fn text(&self, node: &Ptree) -> &str {
        &self.input[node.start..node.end]
    }
    /// parse-tree node argument list (i.e. children nodes).
    pub fn args(&self, node: &'a Ptree) -> &'a Vec<Ptree> {
        &node.args
    }

    // ---------------------------------------------

    pub fn to_string(&self) -> String { 
        self.to_json(&self.tree)
    }

    pub fn to_json(&self, node: &'a Ptree) -> String {
        let name = self.names[node.tag as usize].to_string();
        if node.args.len() == 0 { // terminal
            let mut text = "".to_string();
            for ch in self.input[node.start..node.end].chars() {
                if ch == '"' {
                    text.push('\\');
                }
                text.push(ch);
            }
            format!("[\"{}\" \"{}\"]", name, text)
        } else { // nonterminal
            let mut argstr = "".to_string();
            for arg in node.args.iter() {
                argstr.push_str(&self.to_json(arg));
            }
            format!("[\"{}\" {} {} [{}]]", name, node.start, node.end, argstr)
        }
    }

    // -- compile Parse to Peg ---------------------------------------------

    fn to_peg(&self) -> (Peg, String) {
        assert_eq!("Peg", self.name(&self.tree)); // must be a pPeg grammar parse
        let mut names: Vec<String> = vec![]; // rule names
        let mut exprs: Vec<&Ptree> = vec![]; // ptree rule bodies, to be translated
        for rule in self.args(&self.tree).iter() { // Peg = rule+
            if let [id, expr] = &self.args(rule)[..] { // rule = id " = " alt
                let name = self.text(&id);
                names.push(name.to_string());
                exprs.push(expr);
            }
        }
        let mut rules: Vec<Op> = vec![]; // code rules result, from exprs
        let mut error = String::new();
        for expr in exprs {
            let (op, err) = self.to_op(&names, &expr);
            if let Some(msg) = err {
                error.push_str(&msg);
            }
            rules.push(op);
        }
        assert_eq!(names.len(), rules.len());
        let mut space = Op::Chs(vec![' ', '\t', '\n', '\r'], false, 0, 0);
        let sid = names.iter().position(|s| s == "_space_");
        if let Some(idx) = sid {
            space = Op::Id(idx as u8); //rules.swap_remove(idx);
        }
        (Peg{names, rules, space}, error)
    }
    
    fn to_op(&self, names: &Vec<String>, node: &Ptree) -> (Op, Option<String>) {
        let name = self.name(node);
        match name {
            "id" =>  {
                let name = self.text(node);
                let idx = names.iter().position(|s| s == name);
                if idx == None {
                    let msg = format!("Misssing rule: {}\n", name);
                    return (Op::Fail(name.to_string()), Some(msg));
                }
                return (Op::Id(idx.unwrap() as u8), None);
            },
            "seq" | "alt" => {
                let mut expr: Vec<Box<Op>> = vec![];
                let mut errs = String::new();
                for arg in self.args(node) {
                    let (op, msg) = self.to_op(names, &arg);
                    if let Some(err) = msg {
                        errs.push_str(&err);
                    }
                    expr.push(Box::new(op));
                }
                let mut err = None;
                if errs.len() > 0 {
                    err = Some(errs);
                }
                if name == "seq" {
                    return (Op::Seq(expr), err);
                }
                return (Op::Alt(expr), err);
            }
            // sfx   = [+?] / '*' range?
            // range = num (dots num?)?
            // num   = [0-9]+
            // dots  = '..'        
            "rep" => { // [rep, [expr, sfx]]
                let args = self.args(node);
                let (expr, msg) = self.to_op(names, &args[0]);
                let mut min = 0;
                let mut max = 0;
                if self.name(&args[1]) == "sfx" {
                    let sfx = self.text(&args[1]).chars().next().unwrap();
                    if sfx == '+' {
                        min = 1;
                    } else if sfx == '?' {
                        max = 1;
                    }    
                } else if self.name(&args[1]) == "num" { // xxx*num
                    min = self.text(&args[1]).parse().unwrap();
                    max = min;
                } else { // *range
                    let range = self.args(&args[1]);
                    min = self.text(&range[0]).parse().unwrap();
                    if range.len() == 3 { // xxx*min..max
                        max = self.text(&range[2]).parse().unwrap();
                    }
                }
                if let Chs(cs, neg, _min, _max) = expr { // [x]*
                    assert_eq!(_min,1);
                    assert_eq!(_max,1);
                    return (Chs(cs, neg, min, max), msg);
                }
                return (Op::Rep(Box::new(expr), min, max), msg);
            }
            "pre" => { // [pre, [pfx, expr]]
                let args = self.args(node);
                let pfx = self.text(&args[0]).chars().next().unwrap();
                let (expr, msg) = self.to_op(names, &args[1]);
                if pfx == '~' {
                    if let Chs(cs, neg, min, max) = expr { // ~[x]
                        return (Chs(cs, !neg, min, max), msg);
                    }
                    if let Sq(ref str) = expr { // ~'x'
                        if str.len() == 1 {
                            return (Chs(vec![str.chars().next().unwrap()], true, 1, 1), msg);
                        }
                    }
                }
                return (Op::Pre(pfx, Box::new(expr)), msg);
            }
            "sq" => {
                let t = self.text(node);
                if &t[t.len()-1..] == "i" { // "xxx"i
                    let s = escape(&t[1..t.len()-2]).to_uppercase();
                    return (Op::Sqi(s), None);
                }
                return (Op::Sq(escape(&t[1..t.len()-1])), None);
            }, 
            "dq" => { // Sp, Sq(), ...
                let t = self.text(node);
                let case = &t[t.len()-1..] == "i"; // "xxx"i
                let k = if case {2} else {1}; // to chop i sfx 
                let s = escape(&t[1..t.len()-k]);
                let xs: Vec<&str> = s.split(' ').collect();
                let mut seq = vec![];
                let mut prior = false; // not prior _space_
                for x in xs {
                    if x == "" {
                        if !prior {
                            seq.push(Op::Sp);
                        }
                        prior = true;                       
                    } else {
                        if case {
                            seq.push(Op::Sqi(escape(x).to_uppercase()));
                        } else { 
                            seq.push(Op::Sq(escape(x)));
                        }
                        prior = false;
                    }
                }
                if seq.len() == 1 {
                    return (seq.swap_remove(0), None); 
                } else {
                    let mut ops = vec![];
                    for op in seq {
                        ops.push(Box::new(op));
                    }
                    return (Op::Seq(ops), None);
                }                           
            }, 
            "chs" => {
                let t = self.text(node);
                let chs = escape(&t[1..t.len()-1]).chars().collect();
                return (Op::Chs(chs, false, 1, 1), None);
            },
            "extn" => {
                let t = self.text(node);
                (Op::Ext(escape(&t[1..t.len()-1])), None)
            },
            _ => panic!("Undefined op name: {:?}", name),
        }
    }

} // impl Parse

fn escape(txt: &str) -> String {
    let mut s = "".to_string();
    let mut cs = txt.chars();
    loop {
        let ch = cs.next();
        match ch {
            Some('\\') => {
                let esc = cs.next();
                match esc {
                    Some('n') => s.push('\n'),
                    Some('r') => s.push('\r'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('u') => {
                        let mut n = 0;
                        for _ in 0..4 {
                            if let Some(d) = cs.next() {
                                if let Some(k) = d.to_digit(16) {
                                    n = n*16+k;
                                } else { // TODO panic => err reoprt...
                                    panic!("not a hex digit {:?}",d);
                                }                                        
                            } else {
                                panic!("expeting 4 hex digits");
                            } 
                        }
                        s.push(std::char::from_u32(n).unwrap());
                    }
                    Some(c) => {
                        println!("illegal escape {}",c);
                        s.push('\\');
                        s.push(c);
                    }
                    None => s.push('\\')
                }
            }
            Some(c) => s.push(c),
            None => break,
        }
    }
    return s;
}

impl fmt::Display for Parse<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_json(&self.tree))
    }
}

// -- fault reporting --------------------------------------------

impl PegErr<'_> {

    fn report(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let report = Report::new(self.input);
        if self.ok {
            if self.error.len() > 0 {
                return write!(f, "grammar error:\n{}", self.error)
            }
            return write!(f, "parse fell short...\n{}", report.line_pos(self.cursor))
        }
        write!(f, "parse failed...\n{}In rule: {}, expected: {}",
            report.line_pos(self.cursor), self.rule, self.expect)
    }

}

impl fmt::Debug for PegErr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        PegErr::report(&self, f)
    }
}

impl fmt::Display for PegErr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        PegErr::report(&self, f)
    }
}

struct Report<'a> {
    src: &'a str,
    starts: Vec<usize>, 
}

impl<'a> Report<'a> {
    fn new(src: &'a str) -> Self {
        let mut starts = vec![0]; // start-of-line index
        let mut cs = src.char_indices();
        loop {
            match cs.next() {
                Some((eol, '\n')) => starts.push(eol+1),
                Some(_) => continue,
                None =>  break,
            }
        } // if the last char is not a '\n' the last line will be missed
        starts.push(src.len()); // ensure at least one sol at end
        Report {
            src,
            starts,
        }
    }

    fn pos_line(&self, pos: usize) -> (usize, usize) {
        let mut ln = 1;
        while ln < self.starts.len() && pos >= self.starts[ln] {
            ln += 1;
        }
        if ln == self.starts.len() && ln > 1 { // pos after end of input
            return (ln-1, (pos-self.starts[ln-2])+1);            
        }
        (ln, (pos-self.starts[ln-1])+1)
    }

    fn inset(&self, new_line: bool, sol: usize, pos: usize) -> String {
        let mut cs = self.src.char_indices();
        let mut sp = if new_line {"\n"} else {""}.to_string();
        let mut i = sol;
        while i < pos {
            match cs.next() {
                Some((j, _)) => {
                    if j == pos { break; };
                    sp.push(' ');
                    i += 1;
                },
                None =>  break,
            }
        }
        return sp.to_owned();
    }

    fn line_pos(&self, pos: usize) -> String {
        let (ln, col) = self.pos_line(pos);
        let len = self.starts.len();
        let end = self.starts[len-1]; // src.len()
        let i = if ln > 4 {self.starts[ln-3]} else {0};
        let j = if ln < len {self.starts[ln]} else {end};
        let k = if ln+3 < len {self.starts[ln+2]} else {end};
        let before = &self.src[i..j];
        let new_line = j == end && self.starts[len-2] != end; // missing final new-line
        let inset = self.inset(new_line, self.starts[ln-1], pos);
        let after = &self.src[j..k];
        return format!("{}{}^ line: {} col: {}  ({} of {})\n{}", 
                        before, inset, ln, col, pos, end, after);
    }

} // impl Report

// -- trace reporting ------------------------------------------------------

impl<'p> Pen<'p> {
    fn trace_report(&mut self, name: &str, flag: u8) {
        let pos = self.pos.try_into().unwrap();
        if self.trace_pos != pos {
            self.trace_pos = pos;
            let mut i: usize = self.pos;
            while i>0 {
                if self.input[i-1..i].as_bytes()[0] == 0x0A  {break;}
                i -= 1;
            }
            let mut j: usize = self.pos;
            while j < self.input.len() {
                if self.input[j..j+1].as_bytes()[0] == 0x0A  {break;}
                j += 1;
            }
            let src = &self.input[i..j];
            let k = self.pos-i;
            let mut inset = "".to_string();
            for _ in 0..k {
                inset.push(' ');
            }
            let mut names = "".to_string();
            for i in 0..self.depth {
                if i>0 { names.push(' '); }
                names.push_str(&self.code.names[self.calls[i] as usize]);
            }
            if flag == 0 {
                print!("\n{}:\n{}\n{}^ {}", names, src, inset, name);
            } else if flag == 1 {
                print!("\n{}:\n{}\n{}^ !{}", names, src, inset, name);
            } else {
                print!(" => {}", name);
                print!("\n{}:\n{}\n{}^", names, src, inset);
            }
        } else {
            match flag {
                0 => print!(" {}", name),
                1 => print!(" !{}", name),
                2 | 3 => print!(" => {}", name),
                x => panic!("undefined trace flag {:?}", x),
            }
        }
    }       

} // Pen impl

// -- Op Display -----------------------------------------------------

impl Peg {
    fn op_display(&self, op: &Op) -> String {
        match op {
            Id(idx) => format!("{}", self.names[*idx as usize]),
            Alt(ops) => {
                let mut sts = vec![];
                for op in ops { sts.push(self.op_display(op)) };
                format!("({})", sts.join(" / "))
            }
            Seq(ops) => {
                let mut sts = vec![];
                for op in ops { sts.push(self.op_display(op)) };
                format!("({})", sts.join(" "))
            }
            Pre(pfx, op) => {
                format!("{}{}", pfx, self.op_display(op))
            }
            Rep(op, min, max) => {
                let sfx = repeat_sigil(*min, *max);
                format!("{}{}", self.op_display(op), sfx)
            }
            Sq(s) => format!("'{}'", s),
            Sqi(s) => format!("'{}'i ", s),
            // Dq(s) => format!("\"{}\"", s),
            Chs(chs, neg, min, max) => {
                let pre = if *neg {"~"} else {""};
                let mut cs = "".to_string();
                for c in chs { cs.push(*c)}
                let sfx = repeat_sigil(*min, *max);
                format!("{}[{}]{}", pre, cs, sfx)
            }
            Ext(s) => format!("<{}>", s),
            Sp  => "\" \"".to_string(),
            _ => format!("{:?}", op),
        }
    }
}

fn repeat_sigil(min: u8, max: u8) -> String {
    match (min, max) {
        (0,0) => "*".to_string(),
        (1,0) => "+".to_string(),
        (0,1) => "?".to_string(),
        (1,1) => "".to_string(),
        (n,0) => format!("*{}..", n),
        (n,m) => if n==m {format!("*{}",n)} else {format!("*{}..{}", n, m)},
    }
}

impl fmt::Display for Peg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rules = String::new();
        let mut i = 0;
        for name in &self.names {
            let code = &self.op_display(&self.rules[i]);
            i += 1;
            rules.push_str(&format!("  {} = {}\n", name, code));
        }
        write!(f, "{}", rules)
    }
}

// -- Bootstrap ---------------------------------------------------

use std::fmt;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref CODE: Peg = Peg {
        names: sos(vec!["Peg", "rule", "alt", "seq", "rep", "pre", "term", "id", "pfx", "sfx", "range", "num", "dots", "call", "sq", "dq", 
        "chs", "group", "extn", "_space_"]), 
    rules: vec![
    seq(vec![Sp, rep(seq(vec![id(1), Sp]), 1, 0)]), 
    seq(vec![id(7), Sp, sq("="), Sp, id(2)]),
    seq(vec![id(3), rep(seq(vec![Sp, sq("/"), Sp, id(3)]), 0, 0)]), 
    seq(vec![id(4), rep(seq(vec![Sp, id(4)]), 0, 0)]), 
    seq(vec![id(5), rep(id(9), 0, 1)]), 
    seq(vec![rep(id(8), 0, 1), id(6)]), 
    alt(vec![id(13), id(14), id(15), id(16), id(17), id(18)]), 
    seq(vec![chs(vec!['a', '-', 'z', 'A', '-', 'Z', '_'], false, 1, 1),
        chs(vec!['a', '-', 'z', 'A', '-', 'Z', '0', '-', '9', '_'], false, 0, 0)]), 
    chs(vec!['&', '!', '~'], false, 1, 1), 
    alt(vec![chs(vec!['+', '?'], false, 1, 1), seq(vec![sq("*"), rep(id(10), 0, 1)])]), 
    seq(vec![id(11), rep(seq(vec![id(12), rep(id(11), 0, 1)]), 0, 1)]), 
    chs(vec!['0', '-', '9'], false, 1, 0), 
    sq(".."), 
    seq(vec![id(7), pre('!', seq(vec![Sp, sq("=")]))]), 
    seq(vec![sq("'"), rep(pre('~', sq("'")), 0, 0), sq("'"), rep(sq("i"), 0, 1)]), 
    seq(vec![sq("\""), chs(vec!['"'], true, 0, 0), sq("\""), rep(sq("i"), 0, 1)]), 
    seq(vec![sq("["), chs(vec![']'], true, 0, 0), sq("]")]),
    seq(vec![sq("("), Sp, id(2), Sp, sq(")")]), 
    seq(vec![sq("<"), chs(vec!['>'], true, 0, 0), sq(">")]), 
    rep(alt(vec![seq(vec![sq("#"), chs(vec!['\n', '\r'], true, 0, 0)]),
        chs(vec![' ', '\t', '\n', '\r'], false, 1, 0)]), 0, 0)
    ],
    space: rep(alt(vec![seq(vec![sq("#"), chs(vec!['\n', '\r'], true, 0, 0)]),
            chs(vec![' ', '\t', '\n', '\r'], false, 1, 0)]), 0, 0),
    };
}

// -- Op constructor functions -----------------------------

fn id(idx:u8) -> Op {
    Id(idx)
}

fn alt(ops:Vec<Op>) -> Op {
    let mut args: Vec<Box<Op>> = vec![];
    for op in ops {
        args.push(Box::new(op));
    }
    Alt(args)
}

fn seq(ops:Vec<Op>) -> Op {
    let mut args: Vec<Box<Op>> = vec![];
    for op in ops {
        args.push(Box::new(op));
    }
    Seq(args)
}

fn rep(op:Op, min:u8, max:u8) -> Op {
    Rep(Box::new(op), min, max)
}

fn pre(ch:char, op:Op) -> Op {
    Pre(ch, Box::new(op))
}

fn sq(s: &str) -> Op {
    Sq(s.to_string())
}

fn chs(cs: Vec<char>, neg:bool, min:u8, max:u8) -> Op {
    Chs(cs, neg, min, max)
}

fn sos(strs:Vec<&str>) -> Vec<String> {
    let mut names: Vec<String> = vec![];
    for s in strs {
        names.push(s.to_string());
    }
    names
}

// test ---------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peg_peg() {
        let peg = Peg::new(PEG_GRAMMAR).unwrap();
        let peg1 = format!("{:?}", peg);
        let peg_parse = peg.parse(PEG_GRAMMAR).unwrap();
        let (peg_2, err) = peg_parse.to_peg();
        assert!(err.len() == 0);
        let peg2 = format!("{:?}", peg_2);
        assert_eq!(peg1, peg2);
    }

} // mod tests


// -- API --------------------------------------------------------

fn no_extend(_pen:&mut Pen, _extra: String) -> bool {
    // println!("undefined extension <{}>", _extra);
    false
}


/// Peg is a grammar defined parser.
impl Peg {
    /// The `new` constructor takes a raw string for a pPEG grammar and
    /// constructs an instance of the Peg type that is a parser for that grammar.
    ///
    /// The Result is an instance of the Peg type with a `parse` method,
    /// or an error report for grammar faults.
    pub fn new(grammar: &str) -> Result<Peg, PegErr> {
        let ptree = Pen::parse(&CODE, &grammar, no_extend)?;
        let (peg, error) = ptree.to_peg();
        if error.len() > 0 {
            return Err(PegErr{ok:true, input:"", cursor:0, 
                    rule:"", expect:"".to_string(), error});
        }
        Ok(peg)
    }

    /// The `parse` method takes an input string and parses it with the Peg's grammar rules.
    ///
    /// The Result is a Parse parse-tree, or a parser error report.
    pub fn parse<'a>(&'a self, src: &'a str) -> Result<Parse<'a>, PegErr> {
        Pen::parse(&self, src, no_extend)
    }
    // pub fn trace<'a>(&'a self, src: &'a str) -> Result<Parse<'a>, PegErr> {
    //     Pen::parse(&self, src, 1)
    // }

    /// The `extend` method hooks in a custom parser function for `<extra>` elements.
    pub fn extend<'a>(&'a self, extend: fn(&mut Pen, String) -> bool) -> Pegex {
        Pegex {
            peg: self,
            extend, 
        }
    }
}

/// A Peg parser with a custom extension.
pub struct Pegex<'a> {
    peg: &'a Peg,
    extend: fn(&mut Pen, String) -> bool,
}

impl<'a> Pegex<'a> {
    pub fn parse<'x>(&'x self, src: &'x str) -> Result<Parse<'x>, PegErr> {
        Pen::parse(&self.peg, src, self.extend)
    }
}

