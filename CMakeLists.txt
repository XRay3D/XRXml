cmake_minimum_required(VERSION 4.00)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(XRXml LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
# set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  add_compile_options(-freflection-latest -fexpansion-statements -stdlib=libc++)
  add_link_options(-fuse-ld=lld -lc++ -lc++abi)
else()
  # add_compile_options(-fmodules-ts)
endif()

if(XRXml_USE_MODULES)
  add_library(XRXml)
  target_sources(XRXml PUBLIC FILE_SET modules_public TYPE CXX_MODULES FILES
                              modules/xrxml.cppm)

  target_compile_features(XRXml PUBLIC cxx_std_26)
  target_compile_definitions(XRXml PUBLIC XRXml_USE_MODULES)

  if(CMAKE_CXX_COMPILER_IMPORT_STD)
    target_compile_definitions(XRXml PRIVATE XRXml_USE_STD_MODULE)
    message(STATUS "Using `import std;`")
  else()
    message(STATUS "`import std;` is not available")
  endif()
  target_include_directories(XRXml PUBLIC include)
else()
  add_library(XRXml INTERFACE)
  target_include_directories(XRXml INTERFACE include)
endif()

add_library(XR::Xml ALIAS XRXml)
