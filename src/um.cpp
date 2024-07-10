#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <netinet/in.h>
#include <sstream>
#include <stack>

class Array {
  size_t length;
  std::unique_ptr<uint32_t[]> array;

public:
  Array(size_t sz = 0) : length(sz), array(new uint32_t[sz]) {
    std::fill(&array[0], &array[0] + length, 0);
  }
  Array(const Array &other) {
    length = other.length;
    array.reset(new uint32_t[length]);
    std::copy(&other.array[0], &other.array[0] + length, &array[0]);
  }
  ~Array() { array.reset(nullptr); }
  Array &operator=(const Array &other) {
    if (this == &other) {
      return *this;
    }

    if (length != other.length) {
      array.reset(new uint32_t[other.length]);
      length = other.length;
    }
    std::copy(&other.array[0], &other.array[0] + other.length, &array[0]);
    return *this;
  }

  size_t getLength() const { return length; }

  uint32_t &operator[](std::ptrdiff_t i) {
    if (length <= i) {
      throw std::string("Array index out of bounds");
    }
    return array[i];
  }

  Array operator+(const Array &b) const {
    Array result(length + b.length);
    std::copy(&array[0], &array[0] + length, &result.array[0]);
    std::copy(&b.array[0], &b.array[0] + b.length, &result.array[length]);
    return result;
  }
};

std::string toStr(uint32_t i) {
  std::stringstream str;
  str << i;
  return str.str();
}

class Um {
  uint32_t finger = 0;
  std::map<uint32_t, Array> memory;
  uint32_t registers[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  uint32_t freeIndex = 1;
  std::stack<uint32_t> unusedIndexes;
  bool debug = false;

  std::string printRegs(uint32_t a, uint32_t b, uint32_t c) {
    std::stringstream str;
    str << a << "(" << registers[a] << ") " << b << "(" << registers[b] << ") "
        << c << "(" << registers[c] << ")";
    return str.str();
  }

public:
  Um(Array &program) { memory[0] = program; }
  void eval() {
    while (true) {
      // if (memory[0].getLength() <= finger) {
      //   throw std::string("Finger points outside of memory: ") +
      //   toStr(finger);
      // }
      auto op = memory[0][finger];
      if (debug)
        std::cout << "Opcode : " << std::hex << op << std::dec << std::endl;
      auto a = 0b111 & (op >> 6);
      auto b = 0b111 & (op >> 3);
      auto c = 0b111 & op;
      switch (0b1111 & (op >> 28)) {
      case 0:
        if (debug)
          std::cout << "CondMove " << printRegs(a, b, c) << std::endl;
        if (registers[c] != 0) {
          registers[a] = registers[b];
        }
        ++finger;
        break;
      case 1:
        if (debug)
          std::cout << "Array index " << printRegs(a, b, c) << std::endl;
        if (auto search = memory.find(registers[b]); search == memory.end()) {
          throw std::string("Attempt to access unallocated array");
        }
        registers[a] = memory[registers[b]][registers[c]];
        ++finger;
        break;
      case 2:
        if (debug)
          std::cout << "Array amend " << printRegs(a, b, c) << std::endl;
        if (auto search = memory.find(registers[a]); search == memory.end()) {
          throw std::string("Attempt to to access unallocated array");
        }
        memory[registers[a]][registers[b]] = registers[c];
        ++finger;
        break;
      case 3:
        if (debug)
          std::cout << "Plus " << printRegs(a, b, c) << std::endl;
        registers[a] = registers[b] + registers[c];
        ++finger;
        break;
      case 4:
        if (debug)
          std::cout << "Mul " << printRegs(a, b, c) << std::endl;
        registers[a] = registers[b] * registers[c];
        ++finger;
        break;
      case 5:
        if (debug)
          std::cout << "Div " << printRegs(a, b, c) << std::endl;
        if (registers[c] == 0) {
          throw std::string("Division by zero");
        }
        registers[a] = registers[b] / registers[c];
        ++finger;
        break;
      case 6:
        if (debug)
          std::cout << "Not-And " << printRegs(a, b, c) << std::endl;
        registers[a] = ~(registers[b] & registers[c]);
        ++finger;
        break;
      case 7:
        return;
      case 8:
        if (debug)
          std::cout << "Allocate " << printRegs(a, b, c) << std::endl;
        if (unusedIndexes.empty()) {
          auto i = freeIndex++;
          memory[i] = Array(registers[c]);
          registers[b] = i;
        } else {
          auto i = unusedIndexes.top();
          unusedIndexes.pop();
          memory[i] = Array(registers[c]);
          registers[b] = i;
        }
        ++finger;
        break;
      case 9:
        if (debug)
          std::cout << "Abandon " << printRegs(a, b, c) << std::endl;
        if (registers[c] == 0) {
          throw std::string("Attempt to abandon array 0");
        }
        memory.erase(registers[c]);
        unusedIndexes.push(registers[c]);
        ++finger;
        break;
      case 10:
        if (debug)
          std::cout << "Output " << printRegs(a, b, c) << std::endl;
        if (registers[c] > 255) {
          throw std::string("Output is greater than 255");
        }
        std::cout << static_cast<char>(registers[c] & 0xff);
        ++finger;
        break;
      case 11: {
        if (debug)
          std::cout << "Input " << printRegs(a, b, c) << std::endl;
        uint32_t input = getchar();
        if (input > 255) {
          throw std::string("Input is greater than 255");
        }
        registers[c] = input;
        ++finger;
        break;
      }
      case 12: {
        if (debug)
          std::cout << "Load " << printRegs(a, b, c) << std::endl;
        auto newProg = registers[b];
        if (newProg != 0) {
          if (auto search = memory.find(newProg); search == memory.end()) {
            throw std::string("Attempt to load absent array");
          }
          memory[0] = memory[newProg];
        }
        finger = registers[c];
        break;
      }
      case 13:
        if (debug)
          std::cout << "Ortho dst " << (0b111 & (op >> 25)) << ", val "
                    << (op & 0x01FFFFFF) << std::endl;
        registers[0b111 & (op >> 25)] = op & 0x01FFFFFF;
        ++finger;
        break;
      default:
        throw std::string("Invalid operation: ") + toStr(op >> 28);
      }
    }
  }

  void dumpState() {
    std::cout << std::endl << "Machine state" << std::endl;
    std::cout << "  Finger: " << toStr(finger) << std::endl;

    std::cout << "  Rregisters: ";
    for (size_t i = 0; i < 8; ++i) {
      std::cout << toStr(registers[i]);
      if (i != 7) {
        std::cout << ", ";
      }
    }
    std::cout << std::endl;

    std::cout << "  Memory:" << std::endl;
    for (const auto &[key, value] : memory) {
      std::cout << "    Id: " << key << ", len: " << value.getLength()
                << std::endl;
    }
  }
};

Array readFile(const char *filename) {
  std::ifstream in(filename, std::ifstream::ate | std::ifstream::binary);
  auto fz = in.tellg();
  in.seekg(std::ifstream::beg);
  Array result(fz / 4);
  for (std::ptrdiff_t i = 0; i < result.getLength(); ++i) {
    uint32_t tmp;
    in.read((char *)&tmp, 4);
    result[i] = ntohl(tmp);
  }
  return result;
}

int main(int argc, char **argv) {
  Array program;

  for (int i = 1; i < argc; ++i) {
    program = program + readFile(argv[i]);
  }

  Um machine(program);

  try {
    machine.eval();
  } catch (std::string err) {
    std::cout << "Failure: " << err << std::endl;
    machine.dumpState();
  }

  return 0;
}
