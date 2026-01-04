#pragma once

#include <algorithm>
#include <cassert>
#include <memory>
#include <print>
#include <ranges>
#include <vector>

// #include <boost/property_tree/ptree.hpp>
// #include <boost/property_tree/xml_parser.hpp>
// #include <boost/pfr.hpp>

#ifdef LXML_INTERFACE_UNIT
#define LXML_BEGIN_MODULE_EXPORT export {
#define LXML_END_MODULE_EXPORT   }
#define LXML_EXPORT              export
#else
#define LXML_BEGIN_MODULE_EXPORT
#define LXML_END_MODULE_EXPORT
#define LXML_EXPORT
#endif

LXML_BEGIN_MODULE_EXPORT
// using namespace std::string_literals;
using namespace std ::string_view_literals;
namespace r = std ::ranges;
namespace v = std ::views;
LXML_END_MODULE_EXPORT

// LXML_BEGIN_MODULE_EXPORT
LXML_EXPORT
namespace XML {

using std ::print;
using std ::println;
using std ::string_view;

#if 0

struct Document {
    std::string buf;
    boost::property_tree::ptree root;
    string_view version;
    string_view encoding;
    bool load(string_view path) {
        std::unique_ptr<FILE, decltype([](FILE* fp) { if(fp) fclose(fp); })>
            file(fopen(std::string{path}.c_str(), "r"), {});

        if(!file) {
            println(stderr, "Could not load file from '{}'", path);
            return false;
        }

        fseek(file.get(), 0, SEEK_END);
        int size = ftell(file.get());
        fseek(file.get(), 0, SEEK_SET);

        buf.resize(size);
        fread(buf.data(), 1, size, file.get());

        return parse(buf);
    }
    constexpr bool parse(string_view xmlCode) { // XML-код для парсинга
        // Создаем поток
        std::stringstream stream{xmlCode};
        try {
            // Читаем XML
            boost::property_tree::read_xml(stream, root);
        } catch(boost::property_tree::xml_parser_error err) {
            println(stderr, "XML parser error!\n{}:{}: {}",
                err.filename(),
                err.line(),
                err.message());
            return false;
        }
        return true;
    }
    bool write(string_view path, int indent) {
        // Создаем поток
        std::stringstream stream;
        try {
            // Записываем в другой поток
            boost::property_tree::write_xml(stream, root);
        } catch(boost::property_tree::xml_parser_error err) {
            println(stderr, "XML parser error!\n{}:{}: {}",
                err.filename(),
                err.line(),
                err.message());
            throw;
        }

        std::unique_ptr<FILE, decltype([](FILE* fp) { if(fp) fclose(fp); })>
            file(fopen(std::string{path}.c_str(), "w"), {});

        if(!file) {
            println(stderr, "Could not open file '{}'", path);
            return false;
        }
        println(file.get(), "{}", stream.str());
        return true;
    }
};

#else

// =============== Definitions ===============
struct Data {
    string_view key;
    std::variant<string_view, std::string> value_;
    string_view value() const {
        return value_.visit([](auto&& arg) -> string_view { return arg; });
    }
};

using Attribute = Data;

using AttributeList = std::vector<Data>;
using Elements = std::vector<struct Element*>;

struct Element : Data, std::vector<std::unique_ptr<Element>> {
    struct Element* parent{};
    AttributeList attributes{};

    explicit Element(Element* parent = nullptr);

    explicit Element(Element* parent, string_view key, string_view val = {})
        : Data{key, val}, parent{parent} {
        if(parent) parent->emplace_back(this);
    }

    explicit Element(Element* parent, string_view key, std::string val)
        : Data{key, val}, parent{parent} {
        if(parent) parent->emplace_back(this);
    }

    ~Element() = default;
    Elements children(string_view name);

    string_view attrVal(string_view key) const {
        auto it = r::find(attributes, key, &Attribute::key);
        return (it != attributes.end()) ? it.base()->value() : ""sv;
    }

    const Attribute* attr(string_view key) const {
        auto it = r::find(attributes, key, &Attribute::key);
        return (it != attributes.end()) ? it.base() : nullptr;
    }

    Element* firstChild(string_view name) {
        auto it = r::find(*this, name, &Data::key);
        return it != end() ? it->get() : nullptr;
    }

    const Element* firstChild(string_view name) const {
        auto it = r::find(*this, name, &Data::key);
        return it != end() ? it->get() : nullptr;
    }

    string_view name() const noexcept { return key; }
    string_view text() const noexcept { return value(); }

    void setName(string_view newTag) noexcept;
    void setText(string_view newText) noexcept { value_ = newText; }
};

struct Document {
    std::string buf;
    Element root;
    string_view version;
    string_view encoding;
    bool load(string_view path);
    constexpr bool parse(string_view path);
    bool write(string_view path, int indent);
};

// =============== Implementation ===============

// =============== Node ===============
inline void Element::setName(string_view newTag) noexcept {
    if(key.size()) return;
    if(newTag.starts_with('<'))
        newTag = newTag.substr(1);
    if(size_t i = newTag.find_first_of("\r\n\t /"sv); i < newTag.size())
        newTag = newTag.substr(0, i);
    key = newTag;
}

inline Element::Element(Element* parent)
    : parent{parent} {
    if(parent) parent->emplace_back(this);
}

inline Elements Element::children(string_view name) {
    Elements list;
    auto filter = [name](auto&& child) { return child->name() == name; };
    list.assign_range(*this | v::filter(filter) | v::transform(&std::unique_ptr<Element>::get));
    return list;
}

// =============== Document ===============
inline bool Document::load(string_view path) {
    std::unique_ptr<FILE, decltype([](FILE* fp) { if(fp) fclose(fp); })>
        file(fopen(std::string{path}.c_str(), "r"), {});

    if(!file) {
        println(stderr, "Could not load file from '{}'", path);
        return false;
    }

    fseek(file.get(), 0, SEEK_END);
    int size = ftell(file.get());
    fseek(file.get(), 0, SEEK_SET);

    root.clear();
    buf.resize(size);
    fread(buf.data(), 1, size, file.get());

    return parse(buf);
}

inline constexpr bool Document::parse(string_view buf) {
    string_view lex;
    size_t i;

    Element* currNode = &root;
    // Remove bom
    if(buf.starts_with("\xEF\xBB\xBF"sv))
        buf = buf.substr(3);

    enum class TagType {
        START,
        INLINE
    };

    static constexpr auto parseAttrs = +[](string_view& buf, Element& node) -> TagType {
        Attribute attr;
        TagType tt;
        size_t i{};
        while(i < buf.size()) {
            i = buf.find_first_of(attr.key.empty() ? " '\"=>"sv : "'\""sv);
            switch(buf[i]) {
            case ' ': {
                node.setName(buf.substr(1, i));
                buf = buf.substr(++i);
                continue;
            } break;
            case '\'':
            case '"': {
                if(attr.key.empty()) {
                    println(stderr, "Value has no key");
                    return TagType::START;
                }
                i = buf.find_first_of("'\"");
                attr.value_ = buf.substr(0, i);
                buf = buf.substr(++i);
                node.attributes.emplace_back(attr);
                attr.key = {};
                attr.value_ = {};
                continue;
            } break;
            case '=': {
                attr.key = buf.substr(0, i++);
                buf = buf.substr(++i);
                continue;
            } break;
            case '>': {
                if(buf.data()[i - 1] == '/') {
                    node.setName(buf.substr(0, i));
                    tt = TagType::INLINE;
                } else {
                    node.setName(buf.substr(1, i - 1));
                    tt = TagType::START;
                }
                buf = buf.substr(i);
                return tt;
            } break;
            default: break;
            }
        }
        std::unreachable();
        // return TagType::START;
    };

    while((i = buf.find_first_of('<')) < buf.size()) {
        if(buf.front() == '>') lex = buf.substr(1, i - 1);
        if(size_t i = lex.find_first_not_of("\r\n\t "sv); i > lex.size()) lex = {};
        buf = buf.substr(i);
        // Inner text
        if(lex.size()) {
            if(!currNode) {
                println(stderr, "Text outside of document");
                return false;
            }
            currNode->setText(lex);
            lex = {};
        }

        switch(buf[1]) {
        case '/': // End of nodelex
            i = buf.find_first_of('>');
            lex = buf.substr(2, i - 2);
            if(!currNode) {
                println(stderr, "Already at the root");
                return false;
            }
            if(size_t i = lex.find(' '); i < lex.size())
                lex = lex.substr(0, i);
            if(currNode->name() != lex) {
                println(stderr, "Mismatched tags ({} != {})", currNode->name(), lex);
                return false;
            }
            currNode = currNode->parent;
            buf = buf.substr(i);
            continue;
        case '!': // Special nodes
                  // Comments
            if(buf.starts_with("<!--"sv)) {
                while(!lex.ends_with("-->"sv)) {
                    if(i = buf.find_first_of('>', i); i > buf.size()) {
                        println(stderr, "Mismatched end of comment at {}", buf.data() - this->buf.data());
                        return false;
                    } else lex = buf.substr(0, ++i);
                }
                // (new Element{currNode})->setText(lex);
                buf = buf.substr(i);
                continue;
            }
            std::unreachable();
        case '?': { // Declaration tags
            Element desc;
            parseAttrs(buf, desc);
            version = desc.attrVal("version"sv);
            encoding = desc.attrVal("encoding"sv);
            continue;
        }
        default: // New node
            currNode = new Element{currNode};
            // Start name
            if(parseAttrs(buf, *currNode) == TagType::INLINE) {
                currNode = currNode->parent;
                continue;
            }
            // Is name name if none
            assert(currNode->name().size());
            // Reset lexer
            lex = {};
            continue;
        }
    }
    return true;
}

inline bool Document::write(string_view path, int indent) {
    std::unique_ptr<FILE, decltype([](FILE* fp) { if(fp) fclose(fp); })>
        file(fopen(std::string{path}.c_str(), "w"), {});

    if(!file) {
        println(stderr, "Could not open file '{}'", path);
        return false;
    }

    if(version.empty()) version = "1.0";
    if(encoding.empty()) encoding = "UTF-8";

    println(file.get(), R"(<?xml version="{}" encoding="{}"?>)", version, encoding);
    auto nodeOut = [file = file.get(), indent](this auto&& nodeOut, const Element* node, int times = 0) -> void {
        const auto indentTag = v::repeat(' ', indent * times);
        const auto indentAttr = v::repeat(' ', indent * ++times - 1);
        for(auto&& child: *node) {
            // if(times > 0) print(file, "{:{}}", " "sv, indent * times);
            print(file, "{:s}", indentTag);
            if(child->text().starts_with("<!--"sv)) {
                println(file, "{}", child->text());
                continue;
            }

            print(file, "<{}", child->name());
            r::sort(child->attributes, {}, &Data::key); // NOTE remove noise in diff
            for(Attribute attr: child->attributes) {
                // if(attr.value().empty()) continue;
                if(child->attributes.size() > 8)
                    print(file, "\n{:s}", indentAttr);
                print(file, R"( {}="{}")", attr.key, attr.value());
            }
            if(child->size() == 0 && child->text().empty())
                println(file, "/>");
            else {
                print(file, ">");
                if(child->size() == 0)
                    println(file, "{}</{}>", child->text(), child->name());
                else {
                    println(file, "");
                    nodeOut(child.get(), times);
                    // if(times > 0) print(file, "{:{}}", " "sv, indent * times);
                    print(file, "{:s}", indentTag);
                    println(file, "</{}>", child->name());
                }
            }
        }
    };

    nodeOut(&root);
    // fclose(file);
    return true;
}

#endif

} // namespace XML
// LXML_END_MODULE_EXPORT
