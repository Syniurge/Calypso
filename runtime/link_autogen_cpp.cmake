# Links automatically generated __cpp*.o files during the compilation of druntime into a single object file
set(CORE_CPP_O)
file(GLOB CORE_CPP_O ${CMAKE_CURRENT_BINARY_DIR}/__cpp-*.o)
string (REPLACE ";" " " CORE_CPP_O_STR "${CORE_CPP_O}")
execute_process(COMMAND ${CMAKE_LINKER} -Ur -o "${cpp_monolith_o}" ${CORE_CPP_O})
