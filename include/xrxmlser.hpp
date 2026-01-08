#pragma once

#include "xrxml.hpp"
#include <algorithm>
#include <cassert>
#include <charconv>
#include <cstdlib>
#include <memory>
#include <meta>
#include <optional>
#include <print>
#include <ranges>
#include <stack>
#include <unordered_map>
#include <vector>

namespace XML {

namespace r = std ::ranges;
namespace v = std ::views;
using std ::print;
using std ::println;
using std ::string_view;

// =============== Serializer ===============

enum class _DontSkip : bool {
} inline constexpr DontSkip{true};

struct Type {
    enum eType {
        Null,
        Array,
        Attr,
        Element,
        Ignore,
        Name,
        Root,
    } type{};
    bool canSkip = true;
    const char* name = nullptr;
    consteval auto operator()(string_view name) const -> Type {
        return {
            .type = type,
            .canSkip = canSkip,
            .name = std::define_static_string(name),
        };
    }
    consteval auto operator()(string_view name, _DontSkip ds) const -> Type {
        return {
            .type = type,
            .canSkip = !std::to_underlying(ds),
            .name = std::define_static_string(name),
        };
    }
    consteval auto operator()(_DontSkip ds) const -> Type {
        return {
            .type = type,
            .canSkip = !std::to_underlying(ds),
            .name = name,
        };
    }

    constexpr bool isNull() const noexcept { return type == Null; }
    constexpr bool isArray() const noexcept { return type == Array; }
    constexpr bool isAttr() const noexcept { return type == Attr; }
    constexpr bool isElement() const noexcept { return type == Element; }
    constexpr bool isIgnore() const noexcept { return type == Ignore; }
    constexpr bool isName() const noexcept { return type == Name && name != ""sv; }
    constexpr bool isRoot() const noexcept { return type == Root; }

    constexpr auto operator<=>(const Type&) const noexcept = default;
    constexpr operator string_view() const noexcept { return name ? name : ""sv; };
} inline constexpr               //
    ArrayF{Type::Array, false},  // DontSkip
    Array{Type::Array},          //
    AttrF{Type::Attr, false},    // DontSkip
    Attr{Type::Attr},            //
    ElemF{Type::Element, false}, // DontSkip
    Elem{Type::Element},         //
    Ignore{Type::Ignore},        //
    Name{Type::Name},            //
    Root{Type::Root, false};     // DontSkip

namespace meta = std ::meta;

// template <typename T>
// consteval std::optional<T> annotationOfType(meta::info r) {
//     auto v = annotations_of(r, ^^T);
//     std::optional<T> result{};
//     for(meta::info a: v) {
//         if(auto val = meta::extract<T>(a); result.has_value() && *result != val)
//             throw "inconsistent annotations";
//         else {
//             result = std::move(val);
//         }
//     }
//     return result;
// }

#if GR

#define annotations_of annotations_of_with_type

template <typename T>
consteval std::optional<T> annotation_of_type(meta::info r) {
    auto v = annotations_of(r, ^^T);
    std::optional<T> result{};
    for(meta::info a: v) {
        if(result.has_value()) {
            if(extract<T>(a) != result) {
                throw "inconsistent annotations";
            }
        } else {
            result = extract<T>(a);
        }
    }
    return result;
}
#endif

template <meta::info INFO> concept HasXmlAnnotation
    = meta::annotations_of(INFO, ^^Type).size() == 1;
template <meta::info INFO> concept CanSkip
    = HasXmlAnnotation<INFO> && annotation_of_type<Type>(INFO)->canSkip;
template <meta::info INFO> concept IsRoot
    = HasXmlAnnotation<INFO> && annotation_of_type<Type>(INFO)->isRoot();
template <meta::info INFO> concept IsElem
    = HasXmlAnnotation<INFO> && annotation_of_type<Type>(INFO)->isElement();
template <meta::info INFO> concept IsAttr
    = HasXmlAnnotation<INFO> && annotation_of_type<Type>(INFO)->isAttr();
template <meta::info INFO> concept IsArr
    = HasXmlAnnotation<INFO> && annotation_of_type<Type>(INFO)->isArray();

template <typename A> concept IsArithmetic = std ::is_arithmetic_v<A>;
template <typename E> concept IsEnum = std ::is_enum_v<E>;
template <typename F> concept IsFloating = std ::is_floating_point_v<F>;
template <typename I> concept IsIntegral = std ::is_integral_v<I>;
template <typename C> concept IsClass = std ::is_class_v<C>;
template <typename R> concept IsRange = requires(R r) { r::begin(r); r::end(r); };
template <typename T1, typename T2> concept IsSame = std ::is_same_v<T1, T2>;
template <typename T> concept IsOptional = IsSame<T, std::optional<typename T::value_type>>;

inline constexpr const char Black[]{"\033[30m"};
inline constexpr const char Blue[]{"\033[34m"};
inline constexpr const char Cyan[]{"\033[36m"};
inline constexpr const char Gray[]{"\033[90m"};
inline constexpr const char Green[]{"\033[32m"};
inline constexpr const char Magenta[]{"\033[35m"};
inline constexpr const char Red[]{"\033[31m"};
inline constexpr const char White[]{"\033[37m"};
inline constexpr const char Yellow[]{"\033[33m"};

inline constexpr const char Cancel[]{"\033[39m"};
template <size_t N>
struct Str {
    static constexpr size_t SIZE = N;
    char data[N + 1]{};
    constexpr Str(const char (&str)[N]) noexcept { r::copy(str, data); }
    constexpr operator string_view() const noexcept { return data; }
};
// template <size_t N>
// Str(const char (&)[N]) -> Str<N>;

struct Null { };

template <Str Color>
struct Log {
    template <typename... Args>
    static constexpr void operator()(std::format_string<Args...> format, Args&&... args) {
        print(Color);
        println(format, std::forward<Args>(args)...);
        print(Cancel);
    }
};

inline constexpr auto logBlack = Log<Black>{};
inline constexpr auto logBlue = Log<Blue>{};
inline constexpr auto logCyan = Log<Cyan>{};
inline constexpr auto logGray = Log<Gray>{};
inline constexpr auto logGreen = Log<Green>{};
inline constexpr auto logMagenta = Log<Magenta>{};
inline constexpr auto logRed = Log<Red>{};
inline constexpr auto logWhite = Log<White>{};
inline constexpr auto logYellow = Log<Yellow>{};

template <IsEnum Enum>
inline constexpr auto toEnum(string_view name) -> Enum {
#if 0
    static constexpr auto NAMES = [] {// TODO pair Name/Val
        constexpr auto _1 = define_static_array(
            enumerators_of(^^Enum)
            | v::transform(meta::display_string_of)
            | v::transform(&std::string_view::data));

        constexpr auto _2 = define_static_array(
            enumerators_of(^^Enum)
            | v::transform(meta::annotation_of_type<Type>)
            | v::filter([](const std::optional<Type>& opt) { return opt.has_value(); })
            | v::transform([](const std::optional<Type>& opt) { return opt->name; }));

        std::array<string_view, _1.size() + _2.size()> ret;
        r::copy(_2, r::copy(_1, ret.begin()).out);
        return ret;
    }();
    if(r::find(NAMES, name));
#else
    template for(constexpr meta::info ENUM:
        std::define_static_array(enumerators_of(^^Enum))) {
        if(display_string_of(ENUM) == name) return [:ENUM:];
        if constexpr(constexpr auto annotation = annotation_of_type<Type>(ENUM))
            if(*annotation == name) return [:ENUM:];
    }
#endif
    return Enum{};
}

template <IsEnum Enum>
inline constexpr auto toString(Enum e) -> string_view {
    // clang-format off
    switch(e) {
    template for(constexpr meta::info ENUM: std::define_static_array(enumerators_of(^^Enum)))
    case [:ENUM:]:
        if constexpr(constexpr auto annotation = annotation_of_type<Type>(ENUM);
            annotation && annotation->isName())
            return *annotation;
        else
            return display_string_of(ENUM);
    default: return ""sv;
    }
    // clang-format on
}

static consteval auto members(meta::info info) {
    static constexpr auto CTX = meta::access_context::unprivileged();
    return std::define_static_array([info] consteval {
        auto members = nonstatic_data_members_of(info, CTX);
        [&members](this auto self, auto&& bases) -> void {
            for(meta::info base: bases | v::transform(meta::type_of)) {
                self(bases_of(base, CTX));
                members.append_range(meta::nonstatic_data_members_of(base, CTX));
            }
        }(bases_of(info, CTX));
        return members;
    }());
}

template <typename... Functors>
struct Overload final : Functors... {
    using Functors::operator()...;
};

struct Serializer {
    Serializer(string_view path)
        : path{path}, node{document.root()} { }

    template <typename T>
    void operator>>(T& data) try {
        if(document.load(path))
            load<^^T>(data, node);
    } catch(std::exception& ex) {
        println(stderr, "{} {}", __FUNCTION__, ex.what());
    }

    template <typename T>
    void operator<<(const T& data) try {
        save<^^T>(data, node);
        document.save(path, 4);
    } catch(std::exception& ex) {
        println(stderr, "{} {}", __FUNCTION__, ex.what());
    }

private:
    string_view path;
    Document document;

    static constexpr auto CTX = meta::access_context::unchecked();

    static consteval auto nameOf(meta::info info) -> string_view {
        string_view A_NAME{[info] consteval -> string_view {
            if(auto annotation = annotation_of_type<Type>(info))
                return *annotation;
            return string_view{};
        }()};
        string_view T_NAME{is_type(info)
                ? display_string_of(info)
                : display_string_of(type_of(info))};
        string_view F_NAME{is_type(info)
                ? ""
                : display_string_of(info)};
        // logYellow("nameOf -> A: {}, T: {}, F: {}", A_NAME, T_NAME, F_NAME);
        return A_NAME.size() ? A_NAME : (F_NAME.size() ? F_NAME : T_NAME);
    }

    template <typename T>
    static consteval auto nameOf() -> string_view { return nameOf(^^T); }

    template <meta::info INFO>
    static consteval auto typeOf() -> Type::eType {
        if constexpr(HasXmlAnnotation<INFO>)
            return annotation_of_type<Type>(INFO)->type;
        return {};
    }
    template <typename T>
    static consteval auto typeOf() -> Type::eType { return typeOf<^^T>(); }

    // ======================================================================

#if 0
    using ptree = boost::property_tree::ptree;

    ptree& node;

    template <meta::info INFO, typename T>
    static void load(T& data, ptree& node) {
 static       constexpr string_view NAME_OF{nameOf(INFO)};
        logRed("name {}", NAME_OF);

        node.get_optional(NAME_OF);

        // const Data* val = IsAttr<INFO> ? node->attr(NAME_OF)
        //                          : node->firstChild(NAME_OF);
        // if(!val) {
        //     // logRed("data {} {}", NAME_OF, display_string_of(^^T));
        //     data = {};
        //     return;
        // }
        if constexpr(IsSame<T, std::string>) {
            data = node.get<T>(NAME_OF, {});
        } else if constexpr(IsEnum<T>) {
            data = toEnum<string_view>(node.get<T>(NAME_OF, {}));
        } else if constexpr(IsArithmetic<T>) {
            data = node.get<T>(NAME_OF, {});
        } else {
            auto tree = node.get_child_optional(NAME_OF);
            static_assert(members<T>().size(), display_string_of(^^T));
            template for(constexpr meta::info MEMBER: members<T>())
                load<MEMBER>(data.[:MEMBER:], node);
        }
    }

#else

    Element* node;
    std::stack<Element*> stack;

    // ======================================================================

    template <typename T>
    static void save(const T& data, Element* node) { save<^^T>(data, node); }

    static void save(Null, Element*) { } // NOTE do nothing

    template <meta::info INFO, typename... Ts>
    static void save(const std::variant<Ts...>& data, Element* node) {
        data.visit([node](auto&& arg) { save(arg, node); });
    }

    template <meta::info INFO, typename T>
    static void save(const T& data, Element* node) {
        static constexpr string_view NAME_OF{nameOf(INFO)};
#if 0
        // clang-format off
        Overload{
            [](const std::string& data, Element* node) requires IsAttr<INFO> {
                if(CanSkip<INFO> && data.empty()) return;
                node->addAttribute(NAME_OF, data);
            },
            []<IsArithmetic A>(const A& data, Element* node) requires IsAttr<INFO> {
                if(CanSkip<INFO> && data == A{}) return;
                std::array<char, 32> buf{};
                node->addAttribute(NAME_OF,
                    std::string{buf.begin(),
                        std::to_chars(buf.begin(), buf.end(), data).ptr});
            },
            []<IsEnum E>(const E& data, Element* node) requires IsAttr<INFO> {
                if(CanSkip<INFO> && data == E{}) return;
                node->addAttribute(NAME_OF, toString(data));
            },

            [](const std::string& data, Element* node) {
                if(CanSkip<INFO> && data.empty()) return;
                new Element{node, NAME_OF, data};
            },
            []<IsArithmetic A>(const A& data, Element* node) {
                std::array<char, 32> buf{};
                new Element{
                    node, NAME_OF,
                    std::string{buf.begin(), std::to_chars(buf.begin(), buf.end(), data).ptr}
                };
            },
            []<IsEnum E>(const E& data, Element* node) {
                if(CanSkip<INFO> && data == E{}) return;
                new Element{node, NAME_OF, toString(data)};
            },
            []<IsRange R>(const R& data, Element* node) requires IsArr<INFO> {
                if(CanSkip<INFO> && data.size() == 0u) return;
                node = new Element{node, NAME_OF};
                for(auto&& var: data) save(var, node);
            },
            []<IsRange R>(const R& data, Element* node) requires (!IsArr<INFO>) {
                for(auto&& var: data) save(var, node);
            },
            []<IsClass C>(const C& data, Element* node) {
                node = new Element{node, NAME_OF};
                static_assert(members<C>().size(), display_string_of(^^C));
                template for(constexpr meta::info MEMBER: members<C>())
                    save<MEMBER>(data.[:MEMBER:], node);
            },
            [](const T& data, Element* node) {
                logRed("data {} {}", NAME_OF, display_string_of(^^T));
            },
        }/*(data, node)*/;
        // clang-format on
#else

        if constexpr(IsAttr<INFO>) {

            Overload save{
                [](const std::string& data, Element* node) {
                    if(CanSkip<INFO> && data.empty()) return;
                    node->addAttribute(NAME_OF, data);
                },
                []<IsArithmetic A>(const A& data, Element* node) {
                    if(CanSkip<INFO> && data == A{}) return;
                    std::array<char, 32> buf{};
                    node->addAttribute(NAME_OF,
                        std::string{buf.begin(),
                            std::to_chars(buf.begin(), buf.end(), data).ptr});
                },
                []<IsEnum E>(const E& data, Element* node) {
                    if(CanSkip<INFO> && data == T{}) return;
                    node->addAttribute(NAME_OF, toString(data));
                },
                []<class Any>(const Any& data, Element* node) {
                    logRed("data {} {}", NAME_OF, display_string_of(^^Any));
                },
            };
            // constexpr auto save = [](const T& data, Element* node) {
            //     if constexpr(IsSame<T, std::string>) {
            //         if(CanSkip<INFO> && data == T{}) return;
            //         node->addAttribute(NAME_OF, data);
            //     } else if constexpr(IsArithmetic<T>) {
            //         if(CanSkip<INFO> && data == T{}) return;
            //         std::array<char, 32> buf{};
            //         node->addAttribute(NAME_OF,
            //             std::string{buf.begin(),
            //                 std::to_chars(buf.begin(), buf.end(), data).ptr});
            //     } else if constexpr(IsEnum<T>) {
            //         if(CanSkip<INFO> && data == T{}) return;
            //         node->addAttribute(NAME_OF, toString(data));
            //     } else
            //         logRed("data {} {}", NAME_OF, display_string_of(^^T));
            // };
            // if constexpr(IsSame<T, std::string>) {
            //     if(CanSkip<INFO> && data == T{}) return;
            //     node->addAttribute(NAME_OF, data);
            // } else if constexpr(IsArithmetic<T>) {
            //     if(CanSkip<INFO> && data == T{}) return;
            //     std::array<char, 32> buf{};
            //     node->addAttribute(NAME_OF,
            //         std::string{buf.begin(),
            //             std::to_chars(buf.begin(), buf.end(), data).ptr});
            // } else if constexpr(IsEnum<T>) {
            //     if(CanSkip<INFO> && data == T{}) return;
            //     node->addAttribute(NAME_OF, toString(data));
            // }
            if constexpr(IsOptional<T>) {
                if(data) save(*data, node);
            } else
                save(data, node);
        } else if constexpr(IsSame<T, std::string>) {
            node->addElement(NAME_OF, data);
        } else if constexpr(IsArithmetic<T>) {
            std::array<char, 32> buf{};
            node->addElement(NAME_OF,
                std::string{buf.begin(), std::to_chars(buf.begin(), buf.end(), data).ptr});
        } else if constexpr(IsEnum<T>) {
            node->addElement(NAME_OF, toString(data));
        } else if constexpr(IsArr<INFO>) {
            // if(NAME_OF == "Voids") logYellow("data {} {}", NAME_OF, display_string_of(^^T));
            if(CanSkip<INFO> && data.empty()) return;
            node = node->addElement(NAME_OF);
            for(auto&& var: data) save(var, node);
        } else if constexpr(IsRange<T>) {
            for(auto&& var: data) save(var, node);
        } else if constexpr(IsClass<T>) {
            node = node->addElement(NAME_OF);
            // static_assert(members<T>().size(), display_string_of(^^T));
            template for(constexpr meta::info MEMBER: members(^^T))
                save<MEMBER>(data.[:MEMBER:], node);
            if(CanSkip<INFO>
                && node->text().empty()
                && node->attributes().empty()
                && node->empty()) // remove if is all data is empty
                node->parent()->pop_back();
        } else
            logRed("data {} {}", NAME_OF, display_string_of(^^T));
#endif
    }

    // ======================================================================

    static void load(Null, Element*) { } // NOTE do nothing

    template <typename T>
    static void load(T& data, Element* node) { load<^^T>(data, node); }

    template <meta::info INFO, typename T>
    static void load(T& data, Element* node) {
        static constexpr string_view NAME_OF{nameOf(INFO)};
        const Data* val = IsAttr<INFO> ? node->attr(NAME_OF)
                                       : node->firstChild(NAME_OF);
        if(!val) return;

        if constexpr(IsSame<T, std::string>) {
            data = val->value();
        } else if constexpr(IsEnum<T>) {
            data = toEnum<T>(val->value());
        } else if constexpr(IsSame<T, bool>) {
            data = val->value() == "true"sv || val->value() == "1"sv;
        } else if constexpr(IsFloating<T>) {
            string_view sv = val->value();
            std::from_chars(sv.data(), sv.data() + sv.size(), data);
        } else if constexpr(IsIntegral<T>) {
            string_view sv = val->value();
            int format = 10;
            if(sv.starts_with("0x")) sv = sv.substr(2), format = 16;
            std::from_chars(sv.data(), sv.data() + sv.size(), data, format);
        } else {
            logRed("data {} {}", NAME_OF, display_string_of(^^T));
        }
    }

    template <meta::info INFO, typename... Ts>
    static void load(std::variant<Ts...>& data, Element* node) { // for ,Node* ndall
        struct Pair {
            string_view name;
            void (*func)(std::variant<Ts...>& data, Element* node);
        };
        static constexpr std::array NAMES{
            Pair{nameOf(^^Ts), +[](std::variant<Ts...>& data, Element* node) {
                     load<^^Ts>(data.template emplace<Ts>(), node);
                 }}
            ...
        };

        for(auto&& node: *node) {
            for(auto [name, load]: NAMES) {
                if(node->name() == name)
                    return load(data, node.get());
            }
        }
        return;
    }

    template <meta::info INFO, typename... Ts>
    static void load(std::vector<std::variant<Ts...>>& data, Element* node) {

        static const std::unordered_map loaders{
            std::pair{
                      nameOf(^^Ts),
                      +[](std::variant<Ts...>& var, Element* node) {
                    load<^^Ts>(var.template emplace<Ts>(), node);
                }}
            ...
        };

        static constexpr std::array NAMES{nameOf(^^Ts)...};

        decltype(std::span{*node}) span;

        if constexpr(IsArr<INFO>) {
            static constexpr string_view NAME_OF{nameOf(INFO)};
            if(node = node->firstChild(NAME_OF); !node) return;
            span = *node;
        } else {
            auto begin = r::find_first_of(*node, NAMES, {}, &Data::name);
            if(begin == node->end()) return;
            span = {begin, node->end()};
        }

        if(span.empty()) return;
        if constexpr(requires { data.resize(0u); })
            data.resize(span.size());
        for(auto dst = data.begin(); auto&& node: span)
            loaders.at(node->name())(*dst++, node.get());
    }

    template <meta::info INFO, typename T>
    static void load(std::optional<T>& data, Element* node) {
        static constexpr string_view NAME_OF{nameOf(INFO)};
        // if(NAME_OF == "size") asm("int3");

        const Data* val = IsAttr<INFO> ? node->attr(NAME_OF)
                                       : node->firstChild(NAME_OF);
        if(!val) return;
        load<INFO>(data.emplace(T{}), node);
    }

    template <meta::info INFO, typename T>
        requires IsElem<INFO>
    static void load(std::vector<T>& data, Element* node) {
        static constexpr string_view NAME_OF{nameOf(^^T)};
        auto begin = r::find(*node, NAME_OF, &Data::name);
        if(begin == node->end()) return;
        auto end = r::find_last(*node, NAME_OF, &Data::name);
        assert(end.begin() != node->end());
        if constexpr(requires { data.resize(0u); }) {
            std::span span{begin, ++end.begin()};
            data.resize(span.size());
            for(auto dst = data.begin(); auto&& src: span) load(*dst++, src.get());
        } else
            static_assert(false, display_string_of(^^T)); // TODO
    }

    template <meta::info INFO, typename T>
        requires IsArr<INFO>
    static void load(T& data, Element* node) {
        static constexpr string_view NAME_OF{nameOf(INFO)};
        if(node = node->firstChild(NAME_OF); !node) return;
        if constexpr(requires { data.resize(0u); }) {
            data.resize(node->size());
            for(auto dst = data.begin(); auto&& src: *node) load(*dst++, src.get());
        } else
            static_assert(false, display_string_of(^^T)); // TODO
    }

    template <meta::info INFO, typename T>
        requires IsRoot<INFO> || ((IsClass<T> || IsElem<INFO>) && !IsRange<T>)
    static void load(T& data, Element* node) {
        static constexpr string_view NAME_OF{nameOf(INFO)};
        if(node->name() != NAME_OF)
            if(node = node->firstChild(NAME_OF); !node)
                return;
        // static_assert(members<T>().size(), display_string_of(^^T));
        template for(constexpr meta::info MEMBER: members(^^T))
            load<MEMBER>(data.[:MEMBER:], node);
    }
#endif
};

} // namespace XML
// LXML_END_MODULE_EXPORT
