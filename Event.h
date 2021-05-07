#pragma once
#include <string_view>
#include <typeindex>

namespace cppbus {
    // Base event class.
    // Every event class for the EventBus must be derived from this one.
    class Event {
    public:
        virtual ~Event() = default;

        // Does not have to be unique, does not participate in event routing,
        // can contain any characters.
        // This default implementation is useful for defining 'one-time', temporary events, e.g.
        // void f() { struct TempEvent : public Event {}; bus.subscribe<TempEvent>(...); ... }
        virtual std::string_view getName() const
        {
            return typeid(*this).name();
        }

        template<typename EventType>
        static std::type_index getStaticIndex()
        {
            return std::type_index(typeid(EventType));
        }

        static std::type_index getRuntimeIndex(const Event & ev)
        {
            return std::type_index(typeid(ev));
        }

    protected:
        Event() = default;  // Do not allow direct creation of this base class
    };
}
