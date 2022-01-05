use std::fmt;

pub enum Nucleotide {
    A,
    T,
    G,
    C,
}

impl fmt::Display for Nucleotide {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let b = match self {
            Nucleotide::A => "A",
            Nucleotide::T => "T",
            Nucleotide::C => "C",
            Nucleotide::G => "G",
        };
        write!(f, "{}", b)
    }
}

/// All tuples of size `n` for the alphabet defined by `sigma`.
pub fn all_tuples(n: u16, sigma: &Vec<String>) -> Vec<String> {
    if n > 1 {
        let ts = all_tuples(n - 1, sigma);
        let mut v: Vec<String> = Vec::new();
        for b in sigma {
            for t in &ts {
                let r = format!("{}{}", b, t.to_string());
                v.push(r);
            }
        }
        v
    } else {
        sigma.to_vec() // Copy vector.
    }
}

/// All nucleotide tuples of size `n`.
pub fn all_nuc_tuples(n: u16) -> Vec<String> {
    let nucs = vec![
        "A".to_string(),
        "C".to_string(),
        "T".to_string(),
        "G".to_string(),
    ];

    all_tuples(n, &nucs)
}

/// Convenient function, calls `all_nuc_tuples(3)`.
pub fn all_codons() -> Vec<String> {
    all_nuc_tuples(3)
}

