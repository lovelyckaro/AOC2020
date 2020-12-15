use std::collections::HashMap;

#[derive(Debug)]
struct State {
    looked_at: HashMap<usize, usize>,
    steps_taken: usize,
    last_seen: usize,
    max_steps: usize
}

impl State {
    fn from_init(values: &[usize], max_steps: usize) -> State {
        let mut m : HashMap<usize, usize> = HashMap::new();
        let mut steps : usize = 1;
        let init = &values[0..values.len() - 1];
        for i in init.iter() {
            m.insert(*i, steps);
            steps += 1;
        }
        let lastseen = values[steps-1];
        State {looked_at: m, steps_taken: steps, last_seen: lastseen, max_steps: max_steps }
    }
    fn run(&mut self) -> usize {
        while self.steps_taken < self.max_steps {
            match self.looked_at.get(&self.last_seen) {
                Some(prev) => {
                    let tmp = *prev;
                    self.looked_at.insert(self.last_seen, self.steps_taken);
                    self.last_seen = self.steps_taken - tmp;
                }
                None => {
                    self.looked_at.insert(self.last_seen, self.steps_taken);
                    self.last_seen = 0;
                }
            }
            self.steps_taken += 1;
        }
        self.last_seen
    }
}

fn main() {
    let input : Vec<usize>= vec![1,20,8,12,0,14];
    let example : Vec<usize> = vec![0,3,6];
    let mut init_state = State::from_init(&input[..], 30000000);
    println!("{:?}", init_state.run());
}