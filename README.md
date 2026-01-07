# XRXml

Простая библиотека парсинга XML с сериализацией структур С++ с помощью рефлексии.

Парсер невладеющий, все данные в нодах хранятся в ```std::string_view``` из буфера, хранящегося в парсере.

Пример использования:
```cpp
// XML::Array  - The field name tag contains a tag with the type name from the array.
// XML::ArrayF - The `F` suffix means "Ignore if empty or null" in serialization for all annotations instead of an array of elements in the array.
// XML::Elem   - Insert an array of elements without a root tag .
// XML::Attr   - XML attribute.
// XML::Ignore - Ignore serialization.
// XML::Name   - Alternate serialization name in XML format.
// XML::Root   - Root element in XML format.

enum class Type {
    I_O[[= XML::Name("I/O")]], // Serializer use name I/O 
    Power,
    MonoIO,
};

struct Temperature {
    [[= XML::Attr]] int32_t Max;
    [[= XML::Attr]] int32_t Min;
};

struct[[= XML::Name("IP")]] Ip {
    [[= XML::Attr]] std::optional<std::string> ClockEnableMode;
    [[= XML::Attr("xs:config")]] std::optional<std::string> ConfigFile;
    [[= XML::Attr]] std::string InstanceName;
    [[= XML::Attr]] std::string Name;
    [[= XML::Attr]] std::string Version;
};

struct[[= XML::Root]] Mcu {
    std::string Core;
    uint32_t Frequency;
    uint32_t Ram;
    uint32_t IONb;
    std::string Die;
    uint32_t Flash;
    Voltage Voltage;
    Temperature Temperature;
    [[= XML::Elem]] std::vector<Ip> IP;
    [[= XML::Array]] std::vector<Pin> Pins;
    [[= XML::Attr]] std::optional<std::string> ClockTree;
    [[= XML::Attr]] std::optional<std::string> DBVersion;
    [[= XML::Attr]] std::optional<std::string> Family;
    [[= XML::Attr]] bool HasPowerPad;
};

Mcu mcu;

XML::Serializer("old_file.xml") >> mcu;
XML::Serializer("new_file.xml") << mcu;

```
Более сложный пример с наследованиями и ```std::variant```: https://github.com/XRay3D/TopoR/tree/reflection_cs_attributes/TopoR
