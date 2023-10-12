// fn binary_to_trinary(mut val: u64) -> u64 {
//     let mut out = 0;
//     let mut idx = 1;

//     while val != 0 {
//         if val & 1 == 1 {
//             out += idx;
//         }
//         val >>= 1;
//         idx *= 3;
//     }

//     out
// }

// fn inspect_trit(tryte: u64, pos: u8) -> u8 {
//     let m = 3u64.pow((pos + 1) as u32);
//     if pos > 0 {
//         let d = 3u64.pow(pos as u32);

//         ((tryte % m) / d).try_into().unwrap()
//     } else {
//         (tryte % m).try_into().unwrap()
//     }
// }
// fn assign_trit(tryte: u64, pos: u8, val: u8) -> u64 {
//     tryte + (val - inspect_trit(tryte, pos)) as u64 * 3u64.pow(pos.into())
// }

fn find_the_table(data_width: u64, minterms_and_dc: &[u64]) -> Vec<Vec<(u64, bool)>> {
    let init = Vec::new();

    let mut n_ones = Vec::new();

    for minterm in minterms_and_dc {
        let minterm = *minterm;
        while (n_ones.len() as u32) <= minterm.count_ones() {
            n_ones.push(Vec::new());
        }
        n_ones
            .iter_mut()
            .nth(minterm.count_ones() as usize)
            .unwrap()
            .push((minterm, 0u64));
    }

    for (curr, last) in n_ones.iter().skip(1).zip(n_ones.iter()) {
        for (a, _projection) in curr.iter().cloned() {
            for (b, _projection) in last.iter().cloned() {
                if (a ^ b).count_ones() == 1 {
                    println!("{a:04b} {b:04b} {:04b}", (a ^ b))
                }
            }
        }
    }

    init
}

fn main() {}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn trit_test() {
    //     let mut v = 109211226;

    //     let t = binary_to_trinary(v);

    //     let mut idx = 0;
    //     while v != 0 {
    //         assert_eq!(v & 1, inspect_trit(t, idx) as u64);
    //         idx += 1;
    //         v >>= 1;
    //     }
    // }

    #[test]
    fn gen_table() {
        let minterms_and_dc = [4, 5, 6, 8, 9, 10, 13, 0, 7, 15];

        println!("{:#?}", find_the_table(4, &minterms_and_dc))
    }
}
