use core::fmt::Display;

pub struct Tape {
    left: Vec<char>,
    right: Vec<char>,
}

impl Default for Tape {
    fn default() -> Self {
        Self {
            left: Vec::new(),
            right: Vec::new(),
        }
    }
}

impl Display for Tape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Tape: ..@{}|{}|@..",
            self.left
                .iter()
                .flat_map(|sym| ['|', *sym])
                .collect::<String>(),
            self.right
                .iter()
                .rev()
                .flat_map(|sym| ['|', *sym])
                .skip(1)
                .collect::<String>()
        )
    }
}

impl Tape {
    pub fn read(&self) -> char {
        *self.right.last().unwrap_or(&'@')
    }
    pub fn write(&mut self, symbol: char) {
        self.right.pop();
        self.right.push(symbol);
    }
    pub fn move_left(&mut self) {
        let character = match self.left.pop() {
            Some(x) => x,
            None => '@',
        };
        self.right.push(character);
    }
    pub fn move_right(&mut self) {
        let character = match self.right.pop() {
            Some(x) => x,
            None => '@',
        };
        self.left.push(character);
    }
    pub fn initialize(&mut self, input: String) {
        input
            .chars()
            .rev()
            .for_each(|character| self.right.push(character));
    }
    pub fn memory(&self) -> usize {
        self.left.len() + self.right.len()
    }
    pub fn output(&self) {
        println!(
            "Output: {}",
            self.right
                .iter()
                .rev()
                .filter(|sym| **sym != '@')
                .collect::<String>()
        )
    }
}
