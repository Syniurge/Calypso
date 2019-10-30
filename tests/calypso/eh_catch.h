#include <exception>

const unsigned magic_num = 0x12345678;

class ooops : public std::exception
{
public:
    unsigned a;

    const char* what() const throw() {
        return (a == magic_num) ? "Ooops!" : "<INVALID>";
    }

    ooops() { a = magic_num; }
    ~ooops() { a = 0; }
};

namespace test
{
    void throwOoops() {
        throw ooops();
    }

    void throwFloat() {
        throw 20.16f;
    }
}
