use std::f64;
use std::num::ParseFloatError;

pub const PROTOBUF_NAN: &str = "nan"; // "NaN" in rust
pub const PROTOBUF_INF: &str = "inf"; // "inf" in rust
pub const JSON_NAN: &str = "NaN";
pub const JSON_INF: &str = "Infinity";

/// Format float as in protobuf `.proto` files
pub fn format_protobuf_float(f: f64) -> String {
    if f.is_nan() {
        PROTOBUF_NAN.to_owned()
    } else if f.is_infinite() {
        if f > 0.0 {
            format!("{}", PROTOBUF_INF)
        } else {
            format!("-{}", PROTOBUF_INF)
        }
    } else {
        // TODO: make sure doesn't lose precision
        format!("{:?}", f)
    }
}

/// Parse float from `.proto` format
pub fn parse_protobuf_float(s: &str) -> Result<f64, ParseFloatError> {
    if s == PROTOBUF_NAN {
        return Ok(f64::NAN);
    }
    if s == PROTOBUF_INF || s == format!("+{}", PROTOBUF_INF) {
        return Ok(f64::INFINITY);
    }
    if s == format!("-{}", PROTOBUF_INF) {
        return Ok(f64::NEG_INFINITY);
    }
    s.parse()
}
