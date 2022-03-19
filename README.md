# pPEGrs

This is an implementation of [pPEG] in Rust.

##  Example

``` rs
use p_peg::Peg;

fn main() {
        
    // Equivalent to the regex for well-formed URI's in RFC 3986.

    let uri_peg = Peg::new(r#"
        URI     = (scheme ':')? ('//' auth)? path ('?' query)? ('#' frag)?
        scheme  = ~[:/?#]+
        auth    = ~[/?#]*
        path    = ~[?#]*
        query   = ~'#'*
        frag    = ~[ \t\n\r]*
    "#).unwrap();

    let test = "http://www.ics.uci.edu/pub/ietf/uri/#Related";

    match uri_peg.parse(test) {
        Ok(parse) => println!("URI parse:\n{}", parse),
        Err(err) => println!("URI error:\n{}", err),
    }

    /*
    URI parse:
    ["URI" 0 44 [["scheme" "http"]["auth" "www.ics.uci.edu"]
                    ["path" "/pub/ietf/uri/"]["frag" "Related"]]]
    */
}
```

##  Usage

The pPEG Rust implementation `p_peg` package is a single `lib.rs` file.

The only dependency is `lazy_static` (to init the static bootstrap data).

Not yet available for `crates.io` install.

For basic usage see the examples in the examples directory.

[pPEG]: https://github.com/pcanz/pPEG
