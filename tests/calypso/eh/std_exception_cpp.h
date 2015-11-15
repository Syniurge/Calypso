#include <exception>

class ooops : public std::exception {
public:
    unsigned a;
    const char* what() const noexcept { return "Ooops!"; }

    ooops() { a = 0x12345678; }
};
