import math
import sys
import abc
from collections import deque
from collections.abc import Iterator
from enum import Enum


class Pulse(Enum):
    LOW = 0
    HIGH = 1


class Module(abc.ABC):
    def __init__(self, outputs: list[str]):
        self.outputs = outputs

    @abc.abstractmethod
    def process_pulse(self, input_name: str, pulse: Pulse) -> Pulse | None:
        pass

    def reset(self):
        pass


class FlipFlop(Module):
    def __init__(self, outputs: list[str]):
        super().__init__(outputs)
        self._state = False

    def process_pulse(self, input_name: str, pulse: Pulse) -> Pulse | None:
        if pulse is Pulse.LOW:
            self._state = not self._state
            return Pulse.HIGH if self._state else Pulse.LOW

    def reset(self):
        self._state = False


class Conjunction(Module):
    def __init__(self, outputs: list[str]):
        super().__init__(outputs)
        self._memory = {}

    def add_input(self, input_name: str) -> None:
        self._memory[input_name] = Pulse.LOW

    def process_pulse(self, input_name: str, pulse: Pulse) -> Pulse:
        self._memory[input_name] = pulse
        return Pulse.LOW if all(pulse is Pulse.HIGH for pulse in self._memory.values()) else Pulse.HIGH

    def reset(self):
        for input_name in self._memory:
            self._memory[input_name] = Pulse.LOW


class Broadcaster(Module):
    def process_pulse(self, input_name: str, pulse: Pulse) -> Pulse:
        return pulse


def parse_input(lines: Iterator[str]) -> Iterator[tuple[str, list[str]]]:
    for line in lines:
        name, outputs = line.split(' -> ')
        yield name, outputs.split(', ')


def get_modules(module_data: Iterator[tuple[str, list[str]]]) -> dict[str, Module]:
    modules = {}
    for module_code, outputs in module_data:
        if module_code[0] in '%&':
            module_class = FlipFlop if module_code[0] == '%' else Conjunction
            module_name = module_code[1:]
        else:
            module_class = Broadcaster
            module_name = module_code
        modules[module_name] = module_class(outputs)

    for module_name, module in modules.items():
        for output in module.outputs:
            module = modules.get(output)
            if isinstance(module, Conjunction):
                module.add_input(module_name)
    return modules


def count_pulses(modules: dict[str, Module]) -> dict[Pulse, int]:
    counter = {Pulse.LOW: 0, Pulse.HIGH: 0}
    queue = deque([('button', 'broadcaster', Pulse.LOW)])
    while queue:
        input_name, module_name, pulse = queue.pop()
        counter[pulse] += 1
        if module_name in modules:
            new_pulse = modules[module_name].process_pulse(input_name, pulse)
            if new_pulse is not None:
                for output in modules[module_name].outputs:
                    queue.appendleft((module_name, output, new_pulse))
    return counter


def get_counter_ranges(modules: dict[str, Module]) -> Iterator[int]:
    for broadcaster_output in modules['broadcaster'].outputs:
        bits = ''
        flip_flop = modules[broadcaster_output]
        while True:
            if any(isinstance(modules[output], Conjunction) for output in flip_flop.outputs):
                bits = '1' + bits
            else:
                bits = '0' + bits
            for output in flip_flop.outputs:
                if isinstance(modules[output], FlipFlop):
                    flip_flop = modules[output]
                    break
            else:
                break
        yield int(bits, 2)


def main():
    module_data = parse_input(line.rstrip('\n') for line in sys.stdin)
    modules = get_modules(module_data)
    counters = [count_pulses(modules) for i in range(1000)]
    print(sum(counter[Pulse.LOW] for counter in counters) * sum(counter[Pulse.HIGH] for counter in counters))
    # far from general (which would be pretty impossible)
    print(math.lcm(*get_counter_ranges(modules)))


if __name__ == '__main__':
    main()
