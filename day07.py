import sys
from dataclasses import dataclass
from typing import Iterable, Optional, MutableMapping


@dataclass
class CD:
    dir: str


@dataclass
class File:
    name: str
    size: int

    def total_size(self):
        return self.size


@dataclass
class Dir:
    name: str


@dataclass
class Directory:
    parent: Optional["Directory"]
    contents: MutableMapping[str, File | "Directory"]

    def total_size(self):
        return sum(item.total_size() for item in self.contents.values())


@dataclass
class LS:
    output: list[File | Dir]


def parse_input(lines: Iterable[str]) -> Iterable[CD | LS]:
    current_ls = None
    for line in lines:
        line = line.strip()
        if current_ls and line.startswith('$'):
            yield current_ls
            current_ls = None
        if line.startswith('$ cd '):
            yield CD(line[5:])
        elif line == '$ ls':
            current_ls = LS([])
        else:
            assert not line.startswith('$'), line
            part1, name = line.split()
            if part1 == 'dir':
                item = Dir(name)
            else:
                item = File(name, size=int(part1))
            current_ls.output.append(item)
    if current_ls:
        yield current_ls


def parse_directory_structure(commands: Iterable[CD | LS]) -> Directory:
    root = Directory(parent=None, contents={})
    current_dir = root
    for command in commands:
        if isinstance(command, CD):
            if command.dir == '..':
                current_dir = current_dir.parent
            elif command.dir == '/':
                pass  # assume only the first command is `$ cd /`
            else:
                # assume we only enter directories after we list them
                current_dir = current_dir.contents[command.dir]
        else:  # LS
            for item in command.output:
                if isinstance(item, File):
                    current_dir.contents[item.name] = item
                else:  # Dir
                    # assume each directory is being listed only once
                    subdir = Directory(parent=current_dir, contents={})
                    current_dir.contents[item.name] = subdir
    return root


def find_total_sizes(root: Directory) -> Iterable[int]:
    stack = [root]
    while stack:
        directory = stack.pop()
        yield directory.total_size()
        for item in directory.contents.values():
            if isinstance(item, Directory):
                stack.append(item)


def main():
    commands = list(parse_input(sys.stdin))
    root = parse_directory_structure(commands)
    small_total_sizes = [s for s in find_total_sizes(root) if s <= 100000]
    print(sum(small_total_sizes))
    to_delete = root.total_size() - 40000000
    assert to_delete > 0
    print(min(s for s in find_total_sizes(root) if s >= to_delete))


if __name__ == '__main__':
    main()
