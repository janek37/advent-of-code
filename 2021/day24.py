# sorry to anyone reading this, it's a steaming pile of crap
# but I'm really proud of this pile of crap

class Expression:
    needs_paren = True

    def value(self, inputs):
        pass

    def paren_str(self):
        s = str(self)
        if self.needs_paren:
            return f"({s})"
        else:
            return s

    def possible_values(self):
        if not hasattr(self, '_possible_values'):
            self._possible_values = self._get_possible_values()
        return self._possible_values

    def solve(self, values):
        pass


class Constant(Expression):
    needs_paren = False

    def __init__(self, value):
        self._value = value

    def __str__(self):
        return str(self._value)

    def value(self, inputs):
        return self._value

    def _get_possible_values(self):
        return {self._value}


class Input(Expression):
    needs_paren = False

    def __init__(self, num):
        self._num = num

    def __str__(self):
        return f"i{self._num}"

    def value(self, inputs):
        return inputs[self._num]

    def _get_possible_values(self):
        return {1, 2, 3, 4, 5, 6, 7, 8, 9}


class BinaryExpression(Expression):
    def simplify(self):
        if isinstance(self._exp1, Constant) and isinstance(self._exp2, Constant):
            return Constant(self.value({}))
        return self

    def value(self, inputs):
        return self._op(self._exp1.value(inputs), self._exp2.value(inputs))

    def _get_possible_values(self):
        values1 = self._exp1.possible_values()
        values2 = self._exp2.possible_values()
        return {
            self._op(v1, v2)
            for v1 in values1
            for v2 in values2
            if self._validate_args(v1, v2)
        }

    def _validate_args(self, arg1, arg2):
        return True


class Sum(BinaryExpression):
    def __init__(self, exp1, exp2):
        self._exp1 = exp1
        self._exp2 = exp2

    def __str__(self):
        return f"{self._exp1.paren_str()} + {self._exp2.paren_str()}"

    def _op(self, v1, v2):
        return v1 + v2

    def simplify(self):
        if isinstance(self._exp1, Constant) and self._exp1._value == 0:
            return self._exp2
        elif isinstance(self._exp2, Constant) and self._exp2._value == 0:
            return self._exp1
        elif isinstance(self._exp2, Constant) and isinstance(self._exp1, Sum):
            s = self._exp1
            if isinstance(s._exp2, Constant):
                return Sum(s._exp1, Constant(s._exp2._value + self._exp2._value))
        if isinstance(self._exp1, Conditional) and isinstance(self._exp2, Conditional):
            if self._exp1.condition_str() == self._exp2.condition_str():
                return Conditional(
                    self._exp1._exp1,
                    self._exp1._exp2,
                    Sum(self._exp1._if_eq, self._exp2._if_eq).simplify(),
                    Sum(self._exp1._if_neq, self._exp2._if_neq).simplify(),
                )
        elif isinstance(self._exp1, Conditional):
            cond = self._exp1
            return Conditional(
                cond._exp1,
                cond._exp2,
                Sum(cond._if_eq, self._exp2).simplify(),
                Sum(cond._if_neq, self._exp2).simplify(),
            )
        elif isinstance(self._exp2, Conditional):
            cond = self._exp2
            return Conditional(
                cond._exp1,
                cond._exp2,
                Sum(self._exp1, cond._if_eq).simplify(),
                Sum(self._exp1, cond._if_neq).simplify(),
            )
        return super().simplify()


class Product(BinaryExpression):
    def __init__(self, exp1, exp2):
        self._exp1 = exp1
        self._exp2 = exp2

    def __str__(self):
        return f"{self._exp1.paren_str()} * {self._exp2.paren_str()}"

    def _op(self, v1, v2):
        return v1 * v2

    def simplify(self):
        if isinstance(self._exp1, Constant) and self._exp1._value == 0:
            return Constant(0)
        elif isinstance(self._exp2, Constant) and self._exp2._value == 0:
            return Constant(0)
        if isinstance(self._exp1, Constant) and self._exp1._value == 1:
            return self._exp2
        elif isinstance(self._exp2, Constant) and self._exp2._value == 1:
            return self._exp1
        if isinstance(self._exp2, Conditional):
            cond = self._exp2
            return Conditional(
                cond._exp1,
                cond._exp2,
                Product(self._exp1, cond._if_eq).simplify(),
                Product(self._exp1, cond._if_neq).simplify(),
            )
        return super().simplify()


class Quotient(BinaryExpression):
    def __init__(self, exp1, exp2):
        self._exp1 = exp1
        self._exp2 = exp2

    def __str__(self):
        return f"{self._exp1.paren_str()} / {self._exp2.paren_str()}"

    def _op(self, v1, v2):
        return int(v1 / v2)

    def simplify(self):
        if isinstance(self._exp2, Constant) and self._exp2._value == 1:
            return self._exp1
        if isinstance(self._exp2, Constant):
            c = self._exp2._value
            if isinstance(self._exp1, Sum):
                s = self._exp1
                if isinstance(s._exp1, Product):
                    p = s._exp1
                    if isinstance(p._exp2, Constant) and p._exp2._value == c:
                        if s._exp2.possible_values().issubset(range(c)):
                            return p._exp1
            if self._exp1.possible_values().issubset(range(c)):
                return Constant(0)
        if isinstance(self._exp1, Conditional):
            cond = self._exp1
            return Conditional(
                cond._exp1,
                cond._exp2,
                Quotient(cond._if_eq, self._exp2).simplify(),
                Quotient(cond._if_neq, self._exp2).simplify(),
            )
        return super().simplify()


class Remainder(BinaryExpression):
    def __init__(self, exp1, exp2):
        self._exp1 = exp1
        self._exp2 = exp2

    def __str__(self):
        return f"{self._exp1.paren_str()} % {self._exp2.paren_str()}"

    def _op(self, v1, v2):
        return v1 % v2

    def _validate_args(self, arg1, arg2):
        return arg1 >= 0

    def simplify(self):
        if isinstance(self._exp2, Constant):
            c = self._exp2._value
            if isinstance(self._exp1, Sum):
                s = self._exp1
                if isinstance(s._exp1, Product):
                    p = s._exp1
                    if isinstance(p._exp2, Constant) and p._exp2._value == c:
                        if s._exp2.possible_values().issubset(range(c)):
                            return s._exp2
            if self._exp1.possible_values().issubset(range(c)):
                return self._exp1
        if isinstance(self._exp1, Conditional):
            cond = self._exp1
            return Conditional(
                cond._exp1,
                cond._exp2,
                Remainder(cond._if_eq, self._exp2).simplify(),
                Remainder(cond._if_neq, self._exp2).simplify(),
            )
        return super().simplify()


class Conditional(Expression):
    def __init__(self, exp1, exp2, if_eq, if_neq):
        self._exp1 = exp1
        self._exp2 = exp2
        self._if_eq = if_eq
        self._if_neq = if_neq

    def __str__(self):
        return f"{self._if_eq.paren_str()} if ({self.condition_str()}) else {self._if_neq.paren_str()}"

    def condition_str(self, equal=True):
        op = '=' if equal else '!='
        return f"{self._exp1} {op} {self._exp2}"

    def _get_possible_values(self):
        return self._if_eq.possible_values() | self._if_neq.possible_values()

    def simplify(self):
        values1 = self._exp1.possible_values()
        values2 = self._exp2.possible_values()
        if values1 & values2:
            if len(values1) == len(values2) == 1:
                return self._if_eq
        else:
            return self._if_neq
        if isinstance(self._exp1, Conditional):
            if isinstance(self._exp2, Constant) and self._exp2._value == 0:
                return Conditional(self._exp1._exp1, self._exp1._exp2, self._if_neq, self._if_eq)
        return self


COMMANDS = {
    'add': Sum,
    'mul': Product,
    'div': Quotient,
    'mod': Remainder,
}


def parse_operand(operand, context):
    if operand in 'wxyz':
        return context[operand]
    else:
        return Constant(int(operand))


def interpret_instructions(instructions):
    input_count = 0
    context = {'w': Constant(0), 'x': Constant(0), 'y': Constant(0), 'z': Constant(0)}
    for instruction in instructions:
        spl = instruction.split()
        command = spl[0]
        var = spl[1]
        operand = parse_operand(spl[2], context) if len(spl) > 2 else None
        if command == 'inp':
            context[var] = Input(input_count)
            input_count += 1
        elif command == 'eql':
            context[var] = Conditional(context[var], operand, Constant(1), Constant(0)).simplify()
        else:
            context[var] = COMMANDS[command](context[var], operand).simplify()
    return context


def find_zero_conditions(cond):
    # it turns out that all conditions are positive
    if isinstance(cond._if_eq, Conditional):
        for conditions in find_zero_conditions(cond._if_eq):
            yield (cond,) + conditions
    else:
        # the last if_eq is just 0
        yield (cond,)


def main():
    import sys
    context = interpret_instructions(list(sys.stdin)[:18*14])
    z = context['z']
    for conditions in find_zero_conditions(z):
        max_model_number = [None] * 14
        min_model_number = [None] * 14
        condition_strings = set(cond.condition_str() for cond in conditions)
        for cond in conditions:
            left, right = cond._exp1, cond._exp2
            while isinstance(left, Conditional) and left.condition_str() in condition_strings:
                left = left._if_eq
            # it happens that left is always Input + C and right is always Input
            left_input = left._exp1._num
            c = left._exp2._value
            right_input = right._num
            if c >= 0:
                max_model_number[right_input] = 9
                max_model_number[left_input] = 9 - c
                min_model_number[left_input] = 1
                min_model_number[right_input] = c + 1
            else:
                max_model_number[left_input] = 9
                max_model_number[right_input] = 9 + c
                min_model_number[right_input] = 1
                min_model_number[left_input] = 1 - c
        print(''.join(str(n) for n in max_model_number))
        print(''.join(str(n) for n in min_model_number))


if __name__ == '__main__':
    main()
