use std::convert::TryInto;

/// Represents a register on the tape.
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Register(i32);

impl Register {
    /// Returns a BF string of commands to switch to this register.
    pub fn to_bf(&self, cur_reg: &mut Register) -> String {
        let Register(self_num) = self;
        let Register(cur_num) = cur_reg;
        let mut delta = self_num - *cur_num;
        let dir_char = if delta > 0 {
            '>'
        } else {
            delta = -delta;
            '<'
        };

        *cur_reg = *self;

        std::iter::repeat(dir_char)
            .take(delta.try_into().unwrap())
            .collect()
    }

    /// Returns a BF string of commands to set this register to 0.
    pub fn zero(&self, cur_reg: &mut Register) -> String {
        let mut out = self.to_bf(cur_reg);
        out.push_str("[-]");
        *cur_reg = *self;
        out
    }
}

/// Returns a BF String of commands to replace 'r1' with
/// the evalutation of 'r1' == 'r2'.
/// # Arguments
/// * 'r1' - The register to evaluate against 'r2'. Will be replaced with
///          the value of the expression 'r1' == 'r2'.
/// * 'r2' - The register to evaluate against 'r1'. Value is preserved.
/// * 'temp1' - A temporary register. Will be cleared after evaluation.
/// * 'temp2' - A temporary register. Will be cleared after evaluation.
/// * 'cur_reg' - The current register the pointer points to.
pub fn bf_equals(
    r1: &Register,
    r2: &Register,
    temp1: &Register,
    temp2: &Register,
    cur_reg: &mut Register,
) -> String {
    bf_equality(r1, r2, temp1, temp2, cur_reg, true)
}

/// Returns a BF String of commands to replace 'r1' with
/// the evalutation of 'r1' != 'r2'.
/// # Arguments
/// * 'r1' - The register to evaluate against 'r2'. Will be replaced with
///          the value of the expression 'r1' != 'r2'.
/// * 'r2' - The register to evaluate against 'r1'. Value is preserved.
/// * 'temp1' - A temporary register. Will be cleared after evaluation.
/// * 'temp2' - A temporary register. Will be cleared after evaluation.
/// * 'cur_reg' - The current register the pointer points to.
pub fn bf_not_equals(
    r1: &Register,
    r2: &Register,
    temp1: &Register,
    temp2: &Register,
    cur_reg: &mut Register,
) -> String {
    bf_equality(r1, r2, temp1, temp2, cur_reg, false)
}

// returns r1 == r2 if equals is True, else returns r1 != r2 in bf.
fn bf_equality(
    r1: &Register,
    r2: &Register,
    temp1: &Register,
    temp2: &Register,
    cur_reg: &mut Register,
    equals: bool,
) -> String {
    let mut out = format!(
        "{zero1}\
        {zero2}\
        {r1_1}[{t1_1}+{r1_2}-]",
        zero1 = temp1.zero(cur_reg),
        zero2 = temp2.zero(cur_reg),
        r1_1 = r1.to_bf(cur_reg),
        t1_1 = temp1.to_bf(cur_reg),
        r1_2 = r1.to_bf(cur_reg),
    );

    if equals {
        out.push('+');
    }

    out.push_str(&format!(
        "{r2_1}[{t1_2}-{t2_1}+{r2_2}-]\
        {t2_2}[{r2_3}+{t2_3}-]\
        {t1_3}[{r1_3}-{t1_4}[-]]",
        r2_1 = r2.to_bf(cur_reg),
        t1_2 = temp1.to_bf(cur_reg),
        t2_1 = temp2.to_bf(cur_reg),
        r2_2 = r2.to_bf(cur_reg),
        t2_2 = temp2.to_bf(cur_reg),
        r2_3 = r2.to_bf(cur_reg),
        t2_3 = temp2.to_bf(cur_reg),
        t1_3 = temp1.to_bf(cur_reg),
        r1_3 = r1.to_bf(cur_reg),
        t1_4 = temp1.to_bf(cur_reg),
    ));

    out
}

/// Returns a BF String of commands to replace 'r' with
/// the evaluation of 'r' == 'c'.
/// # Arguments
/// * 'r' - The register to evaluate against 'c'. Will be replaced with
///         the value of the expression 'r1' == 'c'.
/// * 'c' - The constant to evaluate against 'r'.
/// * 'temp' - A temporary register. Will be cleared after evaluation.
/// * 'cur_reg' - The current register the pointer points to.
pub fn bf_equals_const(
    r: &Register,
    c: u32,
    temp: &Register,
    cur_reg: &mut Register,
) -> String {
    bf_equality_const(r, c, temp, cur_reg, true)
}

/// Returns a BF String of commands to replace 'r' with
/// the evaluation of 'r' != 'c'.
/// # Arguments
/// * 'r' - The register to evaluate against 'c'. Will be replaced with
///         the value of the expression 'r1' != 'c'.
/// * 'c' - The constant to evaluate against 'r'.
/// * 'temp' - A temporary register. Will be cleared after evaluation.
/// * 'cur_reg' - The current register the pointer points to.
pub fn bf_not_equals_const(
    r: &Register,
    c: u32,
    temp: &Register,
    cur_reg: &mut Register,
) -> String {
    bf_equality_const(r, c, temp, cur_reg, false)
}

// returns r1 == c if equals is true, else returns r1 != c
fn bf_equality_const(
    r: &Register,
    c: u32,
    temp: &Register,
    cur_reg: &mut Register,
    equals: bool,
) -> String {
    let mut out = format!(
        "{zero}\
        {r_1}[{t_1}+{r_2}-]",
        zero = temp.zero(cur_reg),
        r_1 = r.to_bf(cur_reg),
        t_1 = temp.to_bf(cur_reg),
        r_2 = r.to_bf(cur_reg),
    );

    if equals {
        out.push('+');
    }

    out.push_str(&format!(
        "{t_2}{constant}\
        [{r_3}-{t_3}[-]]",
        t_2 = temp.to_bf(cur_reg),
        constant = bf_expand_const(-(c as i32)),
        r_3 = r.to_bf(cur_reg),
        t_3 = temp.to_bf(cur_reg),
    ));

    out
}

/// Expands 'c' to to a string of BF commands to increment/decrement the value of 'c'.
/// # Arguments
/// * 'c' - The constant value to expand.
/// # Returns
///   A string of "+" of length c, or a string of "-" of length -c if c < 0.
pub fn bf_expand_const(c: i32) -> String {
    let ch = if c > 0 { '+' } else { '-' };

    std::iter::repeat(ch)
        .take(c.abs() as usize)
        .collect::<String>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reg_to_bf_test() {
        let reg = Register(3);
        let mut cur_reg = Register(1);

        assert_eq!(reg.to_bf(&mut cur_reg), String::from(">>"));
        assert_eq!(cur_reg, reg);
    }

    #[test]
    fn reg_zero_test() {
        let reg = Register(1);
        let mut cur_reg = Register(3);

        assert_eq!(reg.zero(&mut cur_reg), String::from("<<[-]"));
        assert_eq!(cur_reg, reg);
    }

    #[test]
    fn equals_test() {
        let r1 = Register(0);
        let r2 = Register(1);
        let t1 = Register(2);
        let t2 = Register(3);
        let mut cur_reg = Register(0);

        assert_eq!(
            bf_equals(&r1, &r2, &t1, &t2, &mut cur_reg),
            String::from(">>[-]>[-]<<<[>>+<<-]+>[>->+<<-]>>[<<+>>-]<[<<->>[-]]")
        )
    }

    #[test]
    fn not_equals_test() {
        let r1 = Register(0);
        let r2 = Register(1);
        let t1 = Register(2);
        let t2 = Register(3);
        let mut cur_reg = Register(0);

        assert_eq!(
            bf_not_equals(&r1, &r2, &t1, &t2, &mut cur_reg),
            String::from(">>[-]>[-]<<<[>>+<<-]>[>->+<<-]>>[<<+>>-]<[<<->>[-]]")
        )
    }

    #[test]
    fn equals_const_test() {
        let r = Register(0);
        let t = Register(1);
        let mut cur_reg = Register(0);

        assert_eq!(
            bf_equals_const(&r, 3, &t, &mut cur_reg),
            String::from(">[-]<[>+<-]+>---[<->[-]]")
        )
    }

    #[test]
    fn not_equals_const_test() {
        let r = Register(0);
        let t = Register(1);
        let mut cur_reg = Register(0);

        assert_eq!(
            bf_not_equals_const(&r, 3, &t, &mut cur_reg),
            String::from(">[-]<[>+<-]>---[<->[-]]")
        )
    }
}
