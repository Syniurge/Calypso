#include <exception>

class ooops : public std::exception
{
public:
    unsigned a;
    const char* what() const throw() { return "Ooops!"; }

    ooops() { a = 0x12345678; }
};

namespace test
{
    void throwOoops();
    void throwFloat();
}
