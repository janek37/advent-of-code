import sys
from typing import Iterator


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


def print_z_exprs(gates: list[tuple[str, str, str, str]]) -> None:
    gates_by_output = {output: (wire1, gate, wire2) for wire1, gate, wire2, output in gates}
    z_exprs = {}
    for output in gates_by_output:
        # if output.startswith("z"):
        z_exprs[output] = get_expr(output, gates_by_output)
    for z, z_expr in sorted(z_exprs.items()):
        if z_expr.count(" ") == 2:
            print(z, "=", z_expr, gates_by_output[z])
        if z_expr.startswith("Digit") and z != "z" + z_expr[5:]:
            print(z, "=", z_expr)
        if z_expr == "Xor24":
            print(z, "=", z_expr)


def get_expr(output, gates_by_output):
    if output not in gates_by_output:
        return output
    wire1, gate, wire2 = gates_by_output[output]
    if wire1[1:] == wire1[1:] and min(wire1, wire2)[0] == "x" and max(wire1, wire2)[0] == "y" and gate == "XOR":
        return "Xor" + wire1[1:]
    if {wire1, wire2} == {"x00", "y00"} and gate == "AND":
        return "Carry00"
    if wire1[1:] == wire1[1:] and min(wire1, wire2)[0] == "x" and max(wire1, wire2)[0] == "y" and gate == "AND":
        return "And" + wire1[1:]
    expr1 = get_expr(wire1, gates_by_output)
    expr2 = get_expr(wire2, gates_by_output)
    if gate in ("AND", "XOR"):
        carry = other = None
        if expr1.startswith("Carry"):
            carry, other = expr1, expr2
        if expr2.startswith("Carry"):
            carry, other = expr2, expr1
        if carry and other and other.startswith("Xor"):
            if int(carry[5:]) + 1 == int(other[3:]):
                return ("Partial" if gate == "AND" else "Digit") + other[3:]
    if gate == "OR":
        partial, other = None, None
        if expr1.startswith("Partial"):
            partial, other = expr1, expr2
        if expr2.startswith("Partial"):
            partial, other = expr2, expr1
        if partial and other and other.startswith("And"):
            if partial[7:] == other[3:]:
                return "Carry" + partial[7:]
    if gate == "XOR":
        carry = other = None
        if expr1.startswith("Carry"):
            carry, other = expr1, expr2
        if expr2.startswith("Carry"):
            carry, other = expr2, expr1
        if carry and other and other.startswith("Xor"):
            if int(carry[5:]) + 1 == int(other[3:]):
                return "Partial" + other[3:]
    return f"({expr1} {gate} {expr2})"


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
    swap_outputs("z07", "swt", gates)
    swap_outputs("z13", "pqc", gates)
    swap_outputs("rjm", "wsv", gates)
    swap_outputs("z31", "bgs", gates)
    print(",".join(sorted(["z07", "swt", "z13", "pqc", "rjm", "wsv", "z31", "bgs"])))
    # print_z_exprs(gates)


if __name__ == '__main__':
    main()
