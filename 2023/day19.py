import operator
import sys
from collections.abc import Iterator
from functools import reduce
from typing import NamedTuple


PartGroup = dict[str, tuple[int, int]]


class Rule(NamedTuple):
    category: str
    relation: str
    threshold: int
    target: str

    def check(self, part: dict[str, int]) -> bool:
        if self.relation == '<':
            return part[self.category] < self.threshold
        else:
            return part[self.category] > self.threshold

    def split_group(self, part_group: PartGroup) -> tuple[PartGroup | None, PartGroup | None]:
        if self.relation == '<':
            min_, max_ = part_group[self.category]
            if max_ < self.threshold:
                return part_group, None
            elif min_ < self.threshold:
                part1 = dict(part_group)
                part1[self.category] = (min_, self.threshold - 1)
                part2 = dict(part_group)
                part2[self.category] = (self.threshold, max_)
                return part1, part2
            else:
                return None, part_group
        else:
            min_, max_ = part_group[self.category]
            if min_ > self.threshold:
                return part_group, None
            elif max_ > self.threshold:
                part1 = dict(part_group)
                part1[self.category] = (min_, self.threshold)
                part2 = dict(part_group)
                part2[self.category] = (self.threshold + 1, max_)
                return part2, part1
            else:
                return None, part_group


class Workflow(NamedTuple):
    rules: list[Rule]
    final_rule: str


def parse_input(lines: Iterator[str]) -> tuple[dict[str, Workflow], list[dict[str, int]]]:
    workflows = {}
    for line in lines:
        if not line:
            break
        name = line.split('{')[0]
        rules = line.split('{')[1][:-1].split(',')
        parsed_rules = [
            Rule(rule[0], rule[1], int(rule[2:].split(':')[0]), rule.split(':')[1])
            for rule in rules[:-1]
        ]
        workflows[name] = Workflow(parsed_rules, rules[-1])
    parts = []
    for line in lines:
        part_data = (s.split('=') for s in line[1:-1].split(','))
        parts.append({key: int(value) for key, value in part_data})
    return workflows, parts


def process_part(part: dict[str, int], workflows: dict[str, Workflow]) -> bool:
    workflow = workflows['in']
    while True:
        for rule in workflow.rules:
            if rule.check(part):
                new_workflow_key = rule.target
                break
        else:
            new_workflow_key = workflow.final_rule
        if new_workflow_key in 'AR':
            return new_workflow_key == 'A'
        workflow = workflows[new_workflow_key]


def mass_process(workflows: dict[str, Workflow]) -> int:
    stack = [({"x": (1, 4000), "m": (1, 4000), "a": (1, 4000), "s": (1, 4000)}, "in", 0)]
    accepted_groups = []
    while stack:
        part_group, workflow_name, rule_index = stack.pop()
        rule = workflows[workflow_name].rules[rule_index]
        passing, failing = workflows[workflow_name].rules[rule_index].split_group(part_group)

        if rule.target in 'AR':
            if rule.target == 'A':
                accepted_groups.append(passing)
        else:
            stack.append((passing, rule.target, 0))

        if rule_index + 1 == len(workflows[workflow_name].rules):
            new_workflow_name = workflows[workflow_name].final_rule
            new_index = 0
        else:
            new_workflow_name = workflow_name
            new_index = rule_index + 1
        if new_workflow_name in 'AR':
            if new_workflow_name == 'A':
                accepted_groups.append(failing)
        else:
            stack.append((failing, new_workflow_name, new_index))

    return sum(count_part_group(part_group) for part_group in accepted_groups)


def count_part_group(part_group: PartGroup) -> int:
    return reduce(operator.mul, (part_group[c][1] - part_group[c][0] + 1 for c in 'xmas'))


def main():
    workflows, parts = parse_input(line.rstrip('\n') for line in sys.stdin)
    accepted_parts = [part for part in parts if process_part(part, workflows)]
    print(sum(sum(part.values()) for part in accepted_parts))
    print(mass_process(workflows))


if __name__ == '__main__':
    main()
