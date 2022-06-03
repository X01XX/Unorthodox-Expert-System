// Return a vector of vectors, any x of 0-to-n numbers, order does not matter.
// These canbe used as indicies into a vector of items.
//##########################################################################
pub fn any_x_of_n(num_items: usize, limit: usize) -> Vec<Vec<usize>> {
    assert!(num_items <= limit);

    if num_items == 0 {
        return Vec::<Vec<usize>>::new();
    }

    if num_items == 1 {
        let mut ret_vec = Vec::<Vec<usize>>::new();
        for i in 0..limit {
            let mut listx = Vec::<usize>::with_capacity(num_items);
            listx.push(i);
            ret_vec.push(listx);
        }
        return ret_vec;
    }

    // Make preliminary vector.
    let mut listx = Vec::<usize>::with_capacity(num_items);
    for i in 0..limit {
        listx.push(i);
    }

    if num_items == limit {
        let mut ret_vec = Vec::<Vec<usize>>::new();
        ret_vec.push(listx);
        return ret_vec;
    }

    any_x_of_n2(num_items, &mut Vec::<usize>::new(), &listx)
}

// Number vectors returned = N! / ((N-x)! * x!)
//
// e.g. any 3 of (a, b, c, d)
//
// 4! / (4-3)! * 3! = 4! / 1!3! = 24 / 6 = 4
//
// =  (a, b, c) (a, b, d) (a, c, d) (b, c, d)
//
//######################################################################
pub fn any_x_of_n2(num_items: usize, listy: &Vec<usize>, listx: &Vec<usize>) -> Vec<Vec<usize>> {
    let mut ret_vec = Vec::<Vec<usize>>::new();

    if num_items < 1 || num_items > listx.len() {
        ret_vec.push(listy.to_vec());
        return ret_vec;
    }

    let numx = listx.len() - num_items;

    for x in 0..(numx + 1) {
        let toright = &listx[x + 1..].to_vec();

        let listz = &mut listy[0..].to_vec();

        listz.push(listx[x]);

        let avec = any_x_of_n2(num_items - 1, &listz, &toright);

        if avec.len() > 0 {
            for avecx in avec.iter() {
                ret_vec.push(avecx.clone());
            }
        }
    }

    ret_vec
}
