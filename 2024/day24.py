import sys
from typing import Iterator, NamedTuple


class GateExpression(NamedTuple):
    label: str
    position: int


class Mismatch(NamedTuple):
    expr1: "GateExpression | Mismatch"
    expr2: "GateExpression | Mismatch"
    gate: str
    position: int


def parse_input(lines: Iterator[str]) -> tuple[dict[str, int], list[tuple[str, str, str, str]]]:
    inputs = {}
    gates = []
    part = 1
    for line in lines:
        line = line.strip()
        if line == "":
            part = 2
            continue
        if part == 1:
            wire, value = line.split(": ")
            inputs[wire] = int(value)
        else:
            wire1, gate, wire2, _, output = line.split()
            gates.append((wire1, gate, wire2, output))
    return inputs, gates


def simulate_gates(inputs: dict, gates: list):
    wire_values = inputs.copy()
    waiting_z_wires = {output for _, _, _, output in gates if output.startswith("z")}
    while waiting_z_wires:
        for wire1, gate, wire2, output in gates:
            if wire1 in wire_values and wire2 in wire_values:
                w1 = wire_values[wire1]
                w2 = wire_values[wire2]
                if gate == "AND":
                    result = w1 & w2
                elif gate == "OR":
                    result = w1 | w2
                elif gate == "XOR":
                    result = w1 ^ w2
                else:
                    raise
                wire_values[output] = result
                if output in waiting_z_wires:
                    waiting_z_wires.remove(output)
    return wire_values


def get_output_value(wire_values: dict[str, int]) -> int:
    result = 0
    for wire, value in wire_values.items():
        if value and wire.startswith("z"):
            place = int(wire[1:])
            result += 1 << place
    return result


def find_swaps(gates: list[tuple[str, str, str, str]]) -> Iterator[tuple[str, str]]:
    while True:
        swap = find_swap(gates)
        if swap:
            yield swap
            swap_outputs(swap[0], swap[1], gates)
        else:
            break


def find_swap(gates: list[tuple[str, str, str, str]]) -> tuple[str, str]:
    gates_by_output = {output: (wire1, gate, wire2) for wire1, gate, wire2, output in gates}
    expressions = {get_expr(output, gates_by_output): output for output in gates_by_output}
    for expr, output in expressions.items():
        expr = get_expr(output, gates_by_output)
        if expr.position == -1 and output.startswith("z"):
            expr1, expr2, gate, _ = expr
            position = int(output[1:])
            if gate == "XOR":
                wire1, _, wire2 = gates_by_output[output]
                other_wire = {expr: wire for expr, wire in zip((expr1, expr2), (wire2, wire1))}
                if GateExpression("Carry", position) in (expr1, expr2):
                    return (
                        other_wire[GateExpression("Carry", position)],
                        expressions[GateExpression("Xor", position)],
                    )
                if GateExpression("Xor", position) in (expr1, expr2):
                    return (
                        other_wire[GateExpression("Xor", position)],
                        expressions[GateExpression("Carry", position)],
                    )
        elif expr.position < 0:
            continue
        elif expr.label == "Digit" and output != f"z{expr.position:02d}":
            return output, f"z{expr.position:02d}"


def get_expr(output, gates_by_output) -> GateExpression | Mismatch:
    """
        Digit(0) = Xor(0)
        Digit(N) = Carry(N) XOR Xor(N)
        Carry(1) = And(0)
        Carry(N+1) = Partial(N) OR And(N)
        Partial(N) = Carry(N) AND Xor(N)
        Xor(N) = x(N) XOR y(N)
        And(N) = x(N) AND y(N)
    """
    if output[0] in "xy":
        return GateExpression(output[0], int(output[1:]))
    wire1, gate, wire2 = gates_by_output[output]
    expr1 = get_expr(wire1, gates_by_output)
    expr2 = get_expr(wire2, gates_by_output)
    if expr1.position == expr2.position:
        labels = {expr1.label, expr2.label}
        if labels == {"x", "y"}:
            if gate == "XOR":
                return GateExpression("Xor" if expr1.position > 0 else "Digit", expr1.position)
            elif gate == "AND":
                if expr1.position == 0:
                    return GateExpression("Carry", 1)
                else:
                    return GateExpression("And", expr1.position)
        if labels == {"Carry", "Xor"} and gate in ("AND", "XOR"):
            return GateExpression("Partial" if gate == "AND" else "Digit", expr1.position)
        if labels == {"Partial", "And"} and gate == "OR":
            return GateExpression("Carry", expr1.position + 1)
    if expr1.position < 0 or expr2.position < 0:
        return Mismatch(expr1, expr2, gate, -2)
    return Mismatch(expr1, expr2, gate, -1)


def swap_outputs(output1, output2, gates: list[tuple[str, str, str, str]]) -> None:
    for i, (wire1, gate, wire2, output) in enumerate(gates):
        if output == output1:
            new_output = output2
        elif output == output2:
            new_output = output1
        else:
            new_output = output
        gates[i] = (wire1, gate, wire2, new_output)


def main():
    inputs, gates = parse_input(sys.stdin)
    print(get_output_value(simulate_gates(inputs, gates)))
    swap1, swap2, swap3, swap4 = find_swaps(gates)
    print(",".join(sorted(swap1 + swap2 + swap3 + swap4)))


if __name__ == '__main__':
    main()
