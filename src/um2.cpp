#include <cstdint>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <unordered_map>
#include <stack>
#include <string>
#include <vector>

using Platter = uint32_t;
using Slate = std::vector<Platter>;

class Array
{
    Slate data;

public:
    Array();
    Array(Platter size, Platter value);
    Array(Slate new_data);

    Platter operator[](Platter idx) const { return data[idx]; }
    Platter &operator[](Platter idx) { return data[idx]; }
    Platter size() const { return data.size(); }
};

class Machine
{
    Platter registers[8];
    Platter finger;
    std::unordered_map<Platter, Array> arrays;
    std::stack<Platter> pages;
    Platter maxPage;

public:
    Machine(const Slate &program);
    virtual ~Machine();
    std::string run();
    void print_state();

private:
    bool condMove(Platter op);
    bool arrayIndex(Platter op);
    bool arrayAmend(Platter op);
    bool add(Platter op);
    bool mul(Platter op);
    bool div(Platter op);
    bool notAnd(Platter op);
    bool halt(Platter op);
    bool alloc(Platter op);
    bool abandon(Platter op);
    bool output(Platter op);
    bool input(Platter op);
    bool loadProgram(Platter op);
    bool orthography(Platter op);
};

enum Op
{
    COND_MOVE = 0,
    ARR_INDEX,
    ARR_AMEND,
    ADD,
    MUL,
    DIV,
    NAND,
    HALT,
    ALLOC,
    ABANDON,
    OUTPUT,
    INPUT,
    LOAD,
    ORTHO,
};

inline Op op_index(Platter op) { return static_cast<Op>((op >> 28) & 0b1111); }
inline Platter reg_a(Platter op) { return (op >> 6) & 0b111; }
inline Platter reg_b(Platter op) { return (op >> 3) & 0b111; }
inline Platter reg_c(Platter op) { return op & 0b111; }

int main(int argc, char **argv)
{

    if (argc > 1)
    {
        Slate program;
        for (int i = 1; i < argc; ++i)
        {
            std::cout << "Loading file: " << argv[i] << std::endl;
            Platter buf[4] = {0, 0, 0, 0};
            std::ifstream input(argv[i], std::ios::binary);
            while (input.good())
            {
                for (size_t j = 0; j < 4 && input.good(); ++j)
                {
                    buf[j] = input.get() & 0xFF;
                }
                if (input.good())
                {
                    Platter platter = buf[0] << 24 | buf[1] << 16 | buf[2] << 8 | buf[3];
                    program.push_back(platter);
                }
            }
            input.close();
        }

        auto m = Machine(program);

        std::cout << m.run() << std::endl;

        return 0;
    }
    else
    {
        std::cout << "Usage: " << argv[0] << " <file with program>" << std::endl;
        return -1;
    }

    return 0;
}

Array::Array() { }

Array::Array(Slate new_data)
    : data(new_data)
{ }

Array::Array(Platter size, Platter value)
    : data(size, value)
{ }

Machine::Machine(const Slate &program)
    : registers{0, 0, 0, 0, 0, 0, 0, 0}
    , finger(0)
    , pages()
    , maxPage(1)
{
    arrays[0] = Array(Slate(program.begin(), program.end()));
}

Machine::~Machine() { }

std::string Machine::run()
{
    bool result = true;

    while (result && finger < arrays[0].size())
    {
        Platter op = arrays[0][finger];
        switch (op_index(op))
        {
        case COND_MOVE:
            condMove(op);
            break;
        case ARR_INDEX:
            result = arrayIndex(op);
            break;
        case ARR_AMEND:
            result = arrayAmend(op);
            break;
        case ADD:
            result = add(op);
            break;
        case MUL:
            result = mul(op);
            break;
        case DIV:
            result = div(op);
            break;
        case NAND:
            result = notAnd(op);
            break;
        case HALT:
            result = halt(op);
            break;
        case ALLOC:
            result = alloc(op);
            break;
        case ABANDON:
            result = abandon(op);
            break;
        case OUTPUT:
            result = output(op);
            break;
        case INPUT:
            result = input(op);
            break;
        case LOAD:
            result = loadProgram(op);
            break;
        case ORTHO:
            result = orthography(op);
            break;
        default:
            result = false;
            std::cout << std::hex << "Finger: " << finger << ", op: " << op << std::endl;
            return "Invalid instruction";
        }
    }

    return "Program finished: Finger points outside program";
}

bool Machine::condMove(Platter op)
{
    Platter a = reg_a(op);
    Platter b = reg_b(op);
    Platter c = reg_c(op);
    if (registers[c] != 0)
    {
        registers[a] = registers[b];
    }

    ++finger;
    //std::cout << "CondMove" << std::endl;
    return true;
}

bool Machine::arrayIndex(Platter op)
{
    Platter a = reg_a(op);
    Platter b = reg_b(op);
    Platter c = reg_c(op);
    if (arrays.find(registers[b]) == arrays.end())
    {
        std::cout << "ARR_INDEX address nonexistent slate" << std::endl;
        return false;
    }
    if (registers[c] >= arrays[registers[b]].size())
    {
        std::cout << "ARR_INDEX out of slate bounds" << std::endl;
        return false;
    }
    registers[a] = arrays[registers[b]][registers[c]];

    ++finger;
    //std::cout << "Arr_index" << std::endl;
    return true;
}

bool Machine::arrayAmend(Platter op)
{
    Platter a = reg_a(op);
    Platter b = reg_b(op);
    Platter c = reg_c(op);
    if (arrays.find(registers[a]) == arrays.end())
    {
        std::cout << "ARR_AMEND address nonexistent slate" << std::endl;
        return false;
    }
    if (registers[b] >= arrays[registers[a]].size())
    {
        std::cout << "ARR_AMEND out of slate bounds" << std::endl;
        return false;
    }
    arrays[registers[a]][registers[b]] = registers[c];

    ++finger;
    //std::cout << "ArrAmend" << std::endl;
    return true;
}

bool Machine::add(Platter op)
{
    Platter a = reg_a(op);
    Platter b = reg_b(op);
    Platter c = reg_c(op);
    registers[a] = registers[b] + registers[c];

    ++finger;
    //std::cout << "Add" << std::endl;
    return true;
}

bool Machine::mul(Platter op)
{
    Platter a = reg_a(op);
    Platter b = reg_b(op);
    Platter c = reg_c(op);
    registers[a] = registers[b] * registers[c];

    ++finger;
    //std::cout << "Multiplication" << std::endl;
    return true;
}

bool Machine::div(Platter op)
{
    Platter a = reg_a(op);
    Platter b = reg_b(op);
    Platter c = reg_c(op);
    if (0 == registers[c])
    {
        std::cout << "Division by zero" << std::endl;
        return false;
    }
    registers[a] = registers[b] / registers[c];

    ++finger;
    //std::cout << "Division" << std::endl;
    return true;
}

bool Machine::notAnd(Platter op)
{
    Platter a = reg_a(op);
    Platter b = reg_b(op);
    Platter c = reg_c(op);
    registers[a] = ~(registers[b] & registers[c]);

    ++finger;
    //std::cout << "Not And" << std::endl;
    return true;
}

bool Machine::halt(Platter op)
{
    std::cout << "Machine is halted" << std::endl;
    return false;
}

bool Machine::alloc(Platter op)
{
    Platter b = reg_b(op);
    Platter c = reg_c(op);
    Platter i = 1;
    // do
    // {
    //     if (arrays.find(i) == arrays.end())
    //     {
    //         break;
    //     }
    //     ++i;
    // } while (true);
    if (pages.empty())
    {
        i = maxPage++;
    }
    else
    {
        i = pages.top();
        pages.pop();
    }

    arrays[i] = Array(registers[c], 0);

    registers[b] = i;

    ++finger;
    //std::cout << "Allocate" << std::endl;
    return true;
}

bool Machine::abandon(Platter op)
{
    Platter c = reg_c(op);
    if (registers[c] == 0)
    {
        std::cout << "Trying to abandon 0 array" << std::endl;
        return false;
    }
    if (arrays.find(registers[c]) != arrays.end())
    {
        arrays.erase(registers[c]);
        pages.push(registers[c]);
    }
    else
    {
        std::cout << "Attempt to abandon nonexistent array" << std::endl;
        return false;
    }

    ++finger;
    //std::cout << "Abandon" << std::endl;
    return true;
}

bool Machine::output(Platter op)
{
    Platter c = reg_c(op);
    std::cout << static_cast<char>(registers[c] & 0xFF);

    ++finger;
    //std::cout << "Output" << std::endl;
    return true;
}

bool Machine::input(Platter op)
{
    Platter c = reg_c(op);
    registers[c] = static_cast<Platter>(std::getchar() & 0xFF);

    ++finger;
    //std::cout << "Input" << std::endl;
    return true;
}

bool Machine::loadProgram(Platter op)
{
    Platter b = reg_b(op);
    Platter c = reg_c(op);
    if (0 != registers[b])
    {
        if (arrays.find(registers[b]) == arrays.end())
        {
            std::cout << "Trying to load nonexistent array" << std::endl;
            return false;
        }

        arrays[0] = arrays[registers[b]];
    }
    finger = registers[c];
    //std::cout << "Load program" << std::endl;
    return true;
}

bool Machine::orthography(Platter op)
{
    Platter a = (op >> 25) & 0b111;
    registers[a] = op & 0x01FFFFFF;

    ++finger;
    //std::cout << "Orthography" << std::endl;
    return true;
}

void Machine::print_state()
{
    std::cout << std::endl;
    std::cout << "Registers: " << std::endl << std::hex; // << std::setfill('0') << std::setw(8);
    std::cout << "    " << registers[0] << ", " << registers[1] << ", " << registers[2] << ", " << registers[3]
              << std::endl;
    std::cout << "    " << registers[4] << ", " << registers[5] << ", " << registers[6] << ", " << registers[7]
              << std::endl;
    std::cout << "Finger: " << finger << std::endl;
    std::cout << std::dec << std::setw(0) << std::endl;
    std::cout << "Slates: " << std::endl;
    for (auto const &kv : arrays)
    {
        std::cout << "(" << kv.first << ", " << kv.second.size() << "), ";
    }
    std::cout << std::endl;
}
