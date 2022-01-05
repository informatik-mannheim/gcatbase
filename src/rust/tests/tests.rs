#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn codons() {
        use gcatbase::tuples::*;
        let codons = all_codons();
        println!("{:?}", codons);
        assert_eq!(codons.len(), 64); // different chars
    }     

    #[test]
    fn four_tuples() {
        use gcatbase::tuples::*;
        let tuples4 = all_nuc_tuples(4);
        println!("{:?}", tuples4);
        assert_eq!(tuples4[4], "AACA");
        assert_eq!(tuples4.len(), 256); // different chars
    }  
    
    #[test]
    fn four_tuples2() {
        use gcatbase::tuples::*;
        let v = vec!["A".to_string(), "B".to_string(), "C".to_string()];
        let tuples4 = all_tuples(4, &v);
        println!("{:?}", tuples4);
        assert_eq!(tuples4[4], "AABB");
        assert_eq!(tuples4.len(), 81);
    }  
    
    #[test]
    fn nucs() {
        use gcatbase::tuples::Nucleotide;
        assert_eq!(format!("{}", Nucleotide::A), "A");
        assert_eq!(format!("{}", Nucleotide::T), "T");
        assert_eq!(format!("{}", Nucleotide::C), "C");
        assert_eq!(format!("{}", Nucleotide::G), "G");
    }    
}
