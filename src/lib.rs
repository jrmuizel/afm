extern crate pom;

use pom::char_class::{alpha, alphanum, hex_digit, oct_digit, multispace};
use pom::{parser, Parser};
use pom::parser::*;
use pom::DataInput;

use std::str::FromStr;

#[derive(Debug)]
pub enum Value {
    String(String),
    Name(String),
    Number(String),
    Integer(i64),
    Array(Vec<Value>),
    Boolean(bool)
}

#[derive(Debug)]
pub enum Line {
    CharMetrics(CharMetrics),
    Unknown(String, Vec<Value>)
}

fn content_space() -> Parser<u8, ()> {
    is_a(pom::char_class::space).repeat(0..).discard()
}

fn string() -> Parser<u8, String> {
    (none_of(b"\n\r") | is_a(pom::char_class::space)).repeat(1..).convert(|v|String::from_utf8(v))
}

fn name() -> Parser<u8, String> {
    (is_a(alpha) | one_of(b"*'\"")).repeat(1..).convert(|v|String::from_utf8(v))
}

fn integer() -> Parser<u8, i64> {
    let number = one_of(b"+-").opt() + one_of(b"0123456789").repeat(1..);
    number.collect().convert(|v|String::from_utf8(v)).convert(|s|i64::from_str(&s))
}

fn number() -> Parser<u8, String> {
    let number = one_of(b"+-").opt() +
        ( (one_of(b"0123456789") - one_of(b"0123456789").repeat(0..).discard())
            | (one_of(b"0123456789").repeat(1..) * sym(b'.') - one_of(b"0123456789").repeat(0..))
            | sym(b'.') - one_of(b"0123456789").repeat(1..)
        );
    number.collect().convert(|v|String::from_utf8(v))
}

fn space() -> Parser<u8, ()> {
    ( one_of(b" \t\n\r\0\x0C").repeat(1..).discard()
    ).repeat(0..).discard()
}

fn eol() -> Parser<u8, u8> {
    sym(b'\r') * sym(b'\n') | sym(b'\n') | sym(b'\r')
}

fn value() -> Parser<u8, Value> {
    ( seq(b"true").map(|_| Value::Boolean(true))
    | seq(b"false").map(|_| Value::Boolean(false))
    | integer().map(|v| Value::Integer(v))
    | number().map(|v| Value::Number(v))
    | name().map(|v| Value::Name(v))
    | string().map(|v| Value::String(v))
    | array().map(|v| Value::Array(v))
    ) - content_space()
}
#[derive(Debug, Default)]
pub struct CharMetrics
{
    pub value: i64,
    pub width0x: Option<f64>,
    pub width1x: Option<f64>,
    pub width0y: Option<f64>,
    pub width1y: Option<f64>,
    pub vvector: Option<(f64, f64)>,
    pub name: Option<String>,
    pub bbox: Option<(f64, f64, f64, f64)>,
    pub ligature_sequence: Option<(String, String)>,
}

#[derive(Debug)]
enum CharMetricsKeys {
    WX(f64),
    W1X(f64),
    N(String),
    B(f64, f64, f64, f64),
    L(String, String),
    Unknown(String, String)
}

fn cmetrics_wx() -> Parser<u8, CharMetricsKeys>
{
    (content_space() - (seq(b"WX") | seq(b"WX0")) - content_space() + float() - content_space()).map(|(k, v)| CharMetricsKeys::WX(v))
}

fn cmetrics_w1x() -> Parser<u8, CharMetricsKeys>
{
    (content_space() - seq(b"WX1") - content_space() + float() - content_space()).map(|(k, v)| CharMetricsKeys::W1X(v))
}

fn cmetrics_n() -> Parser<u8, CharMetricsKeys>
{
    (content_space() - seq(b"N") - content_space() + name() - content_space()).map(|(k, v)| CharMetricsKeys::N(v))
}

fn float() -> Parser<u8, f64> {
    number().convert(|v| f64::from_str(&v))
}

fn cmetrics_b() -> Parser<u8, CharMetricsKeys>
{
   (content_space() - sym(b'B') - content_space()
        + (float() - content_space())
        + (float() - content_space())
        + (float() - content_space())
        + (float() - content_space())
    ).map(|((((k, v1), v2), v3), v4)| CharMetricsKeys::B(v1, v2, v3, v4))
}

fn cmetrics_l() -> Parser<u8, CharMetricsKeys>
{
    ((content_space() - sym(b'L') - content_space()) * ((name() - content_space())
        + (name() - content_space()))

    ).map(|(successor, ligature)| CharMetricsKeys::L(successor, ligature))
}

fn ckey_values() -> Parser<u8, String>
{
    none_of(b"\n\r;").repeat(1..).convert(|v|String::from_utf8(v))
}

fn ckey_value_unknown() -> Parser<u8, CharMetricsKeys>
{
    (content_space() * name() - content_space() + ckey_values() - content_space()).map(|(k, v)| CharMetricsKeys::Unknown(k, v))
}

fn ckey_value() -> Parser<u8, CharMetricsKeys>
{
    (cmetrics_wx() | cmetrics_w1x() | cmetrics_n() | cmetrics_b() | cmetrics_l() | ckey_value_unknown())
}

fn character_metrics() -> Parser<u8, CharMetrics>
{
    let l = (content_space() * integer() - content_space() - sym(b';')) + ((ckey_value() - sym(b';')).repeat(0..));
    l.map(|(id, kvs)| { let mut metrics = CharMetrics::default();
        metrics.value = id;
        metrics.name = Some(format!("{:?}", kvs));

        for kv in kvs {
            match kv {
                CharMetricsKeys::Unknown(ref k, ref v) => {
                    println!("{:?} {:?}", k, v);
                    panic!("unexpected kind")
                }
                CharMetricsKeys::WX(wx) => {
                    metrics.width0x = Some(wx);
                }
                CharMetricsKeys::W1X(wx) => {
                    metrics.width1x = Some(wx);
                }
                CharMetricsKeys::B(b1, b2, b3, b4) => {
                    metrics.bbox = Some((b1, b2, b3, b4));
                }
                CharMetricsKeys::N(n) => {
                    metrics.name = Some(n);
                }
                CharMetricsKeys::L(successor, ligature) => {
                    metrics.ligature_sequence = Some((successor, ligature));
                }
            }

        }
        metrics})
}

fn unknown_key() -> Parser<u8, String> {
    (not_a(pom::char_class::multispace) | one_of(b"*'\"")).repeat(1..).convert(|v|String::from_utf8(v))
}

fn key() -> Parser<u8, String> {
    unknown_key()
}

fn array() -> Parser<u8, Vec<Value>> {
    sym(b'[') * space() * call(value).repeat(0..) - sym(b']')
}

fn unknown_line() -> Parser<u8, Line>
{
    let l = key() + (content_space() * value()).repeat(0..);
    l.map(|(key, values)| { Line::Unknown(key, values) })
}

fn character_metrics_line() -> Parser<u8, Line>
{
    let l = sym(b'C') + character_metrics();
    l.map(|(key, metrics)| { Line::CharMetrics(metrics) })
}
fn line() -> Parser<u8, Line>
{
    (character_metrics_line() |  unknown_line())
}

fn file() -> Parser<u8,Vec<Line>>
{
    (line() - eol()).repeat(1..)
}

pub fn parse(input: &[u8]) -> Result<Vec<Line>, pom::Error> {
    file().parse(&mut DataInput::new(input))
}

fn do_parse(input: &[u8]) {
    let result = parse(input);
    if let Ok(lines) = result  {
        for l in lines {
            println!("{:?}", l)
        }
    } else {
        println!("{:?}", result)
    }
}
/*
fn main() {
    let f = File::open("data/Times-Bold.afm").unwrap();
    let mut f = BufReader::new(f);
    let mut contents = Vec::new();
    f.read_to_end(&mut contents);

    //for line in f.lines() {
        do_parse(&contents);
    //}
    do_parse(b"Foo true false\n");
    do_parse(b"Foo 5 7");
    do_parse(b"Foo 5.7");
    do_parse(b"Dance [blue green]")


}
*/

#[cfg(test)]
mod tests {
    use parse;
    #[test]
    fn it_works() {
        assert!(parse(b"Foo Bar\n").is_ok());
    }
}
