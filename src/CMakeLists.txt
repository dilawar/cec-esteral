find_program(NOWEB_FOUND noweb)

if(NOWEB_FOUND)
    message( STATUS "noweb found" )
    FILE( GLOB NOWEB_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/*.nw" )
    foreach( NOSRC ${NOWEB_SRCS})
        message( STATUS "Converting noweb to cpp ${NOSRC}" )
        execute_process( 
            COMMAND noweb ${NOSRC} 
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            )
    endforeach( NOSRC )
endif()

include_directories( ${CMAKE_CURRENT_SOURCE_DIR} )
include_directories( ${CMAKE_SOURCE_DIR}/libantlr )

add_library( cec SHARED 
    ${CMAKE_CURRENT_SOURCE_DIR}/AST.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ASTGRC.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/beeble_statemachine.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/BLIF.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/CPrinter.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/cverilog.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/cverilog_gates.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/Dismantle.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/EsterelLexer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/EsterelParser.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/EsterelPrinter.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/EsterelTreeParser.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ExpandModules.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/GRCBAL.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/GRCC2.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/GRCPrinter.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/GRCsim.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/IR.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/pdg2beeblebrox.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/pdg2beeblebrox_utils.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/prblif.cpp
    )

target_link_libraries( cec cecantlr )

install( TARGETS cec LIBRARY
    DESTINATION lib 
    )
