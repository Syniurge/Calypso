# Links automatically generated __cpp*.o files during the compilation of druntime into a single object file
set(CORE_CPP_O)
file(GLOB CORE_CPP_O ${calypsort_cppdir}/__cpp-*.o)
if(MSVC)
    execute_process(COMMAND ${CMAKE_LINKER} /OUT:"${cpp_monolith_o}" ${CORE_CPP_O})
else()
    execute_process(COMMAND ${CMAKE_LINKER} -r -o "${cpp_monolith_o}" ${CORE_CPP_O})
endif()
