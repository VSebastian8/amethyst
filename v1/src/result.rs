use colored::*;
use core::fmt::Display;
pub enum RunResult {
    Accept,
    Reject,
    ExceededTime,
    ExceededMemory,
}

impl Display for RunResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunResult::Accept => write!(f, "{}", "Accepted!".bright_blue().bold()),
            RunResult::Reject => write!(f, "{}", "Rejected!".bright_blue().bold()),
            RunResult::ExceededTime => write!(
                f,
                "{}",
                "Turing Machine exceeded the maximum number of iterations!"
                    .red()
                    .bold()
            ),
            RunResult::ExceededMemory => {
                write!(
                    f,
                    "{}",
                    "Turing Machine exceeded the maximum amount of memory!"
                        .red()
                        .bold()
                )
            }
        }
    }
}
