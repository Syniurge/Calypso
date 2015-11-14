#include <exception>

struct ooops : std::exception {
    const char* what() const _GLIBCXX_USE_NOEXCEPT { return "Ooops!"; }
};
