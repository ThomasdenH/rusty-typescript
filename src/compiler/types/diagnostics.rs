#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Message {
    MultipleConsecutiveNumericSeparatorsNotPermitted,
    NumericSeparatorsAreNotAllowedHere,
    DigitExpected
}
