HEX2BIN = {
    '0': [0, 0, 0, 0],
    '1': [0, 0, 0, 1],
    '2': [0, 0, 1, 0],
    '3': [0, 0, 1, 1],
    '4': [0, 1, 0, 0],
    '5': [0, 1, 0, 1],
    '6': [0, 1, 1, 0],
    '7': [0, 1, 1, 1],
    '8': [1, 0, 0, 0],
    '9': [1, 0, 0, 1],
    'A': [1, 0, 1, 0],
    'B': [1, 0, 1, 1],
    'C': [1, 1, 0, 0],
    'D': [1, 1, 0, 1],
    'E': [1, 1, 1, 0],
    'F': [1, 1, 1, 1],
}

SUM = 0
PRODUCT = 1
MIN = 2
MAX = 3
LITERAL = 4
GREATER_THAN = 5
LESS_THAN = 6
EQUAL_TO = 7


def parse_hex(hex_str):
    bits = []
    for hexdigit in hex_str:
        bits += HEX2BIN[hexdigit]
    return bits


def bits2int(bits):
    if not bits:
        return 0
    else:
        return bits2int(bits[:-1])*2 + bits[-1]


class BitReader:
    def __init__(self, bits):
        self.bits = bits
        self.ind = 0

    def read_bits(self, bit_count):
        bits = self.bits[self.ind : self.ind+bit_count]
        self.ind += bit_count
        return bits

    def read(self, bit_count):
        return bits2int(self.read_bits(bit_count))


def read_literal(bit_reader):
    bits = []
    while True:
        end_marker = bit_reader.read(1)
        bits += bit_reader.read_bits(4)
        if end_marker == 0:
            return bits2int(bits)


def read_packet_with_size(bit_reader):
    start = bit_reader.ind
    version = bit_reader.read(3)
    type_id = bit_reader.read(3)
    if type_id == LITERAL:
        ind = 6
        value = read_literal(bit_reader)
    else:
        length_type_id = bit_reader.read(1)
        if length_type_id == 0:
            total_body_length = bit_reader.read(15)
            subpackets = read_subpackets_by_total_length(bit_reader, total_body_length)
        else:
            subpacket_count = bit_reader.read(11)
            subpackets = read_subpackets_by_count(bit_reader, subpacket_count)
        value = subpackets
    return (version, type_id, value), bit_reader.ind - start


def read_subpackets_by_total_length(bit_reader, total_length):
    length = 0
    packets = []
    while length < total_length:
        packet, size = read_packet_with_size(bit_reader)
        length += size
        packets.append(packet)
    return packets


def read_subpackets_by_count(bit_reader, count):
    return [read_packet_with_size(bit_reader)[0] for i in range(count)]


def versions(packet):
    yield packet[0]
    if packet[1] != LITERAL:
        for subpacket in packet[2]:
            yield from versions(subpacket)


def evaluate(packet):
    _version, type_id, value = packet
    if type_id == LITERAL:
        return value
    values = [evaluate(subpacket) for subpacket in value]
    if type_id == SUM:
        return sum(values)
    if type_id == PRODUCT:
        product = 1
        for v in values:
            product *= v
        return product
    if type_id == MIN:
        return min(values)
    if type_id == MAX:
        return max(values)
    first, second = values
    if type_id == GREATER_THAN:
        return int(first > second)
    if type_id == LESS_THAN:
        return int(first < second)
    if type_id == EQUAL_TO:
        return int(first == second)


def represent(packet):
    _version, type_id, value = packet
    if type_id == LITERAL:
        return str(value)
    args = (represent(subpacket) for subpacket in value)
    return f"({type_id}:{' '.join(args)})"


def main():
    import sys
    bits = parse_hex(sys.stdin.read().strip())
    bit_reader = BitReader(bits)
    packet, size = read_packet_with_size(bit_reader)
    print(sum(versions(packet)))
    print(evaluate(packet))
    # print(represent(packet))


if __name__ == '__main__':
    main()
