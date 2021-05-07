#pragma once
#include <functional>
#include <type_traits>

#include "Event.h"
#include "EventBus.h"

namespace cppbus {
    // A simple base class for defining an event subscriber
    // that would automatically unsibscribe from all events when destructed.
    class EventSubscriber {
    public:
        EventSubscriber(EventBus & bus) noexcept :
            _subscriberMainBus(bus),
            _subscriberSubId(bus.allocateSubID())
        {
        }

        EventSubscriber(const EventSubscriber & other) = delete;
        EventSubscriber & operator=(const EventSubscriber &) = delete;

        virtual ~EventSubscriber()
        {
            mainBusUnsubscribe();
        }

        template<typename EventType>
        void mainBusSubscribe(EventBus::GenericCallback callback)
        {
            _subscriberMainBus.subscribe<EventType>(std::move(callback), _subscriberSubId);
        }

        template<typename Head, typename... Tail>
        auto mainBusSubscribe(Head && head, Tail &&... tail) ->
            std::enable_if_t<!std::is_same_v<Head, EventBus::GenericCallback>>
        {
            _subscriberMainBus.subscribe(std::forward<Head>(head), std::forward<Tail>(tail)..., _subscriberSubId);
        }

        void mainBusUnsubscribe()
        {
            if (_subscriberSubId != EventBus::SubID::invalid()) {
                _subscriberMainBus.unsubscribe(_subscriberSubId);
            }
        }

        virtual EventBus & getMainBus() noexcept final
        {
            return _subscriberMainBus;
        }

        virtual EventBus & getMainBus() const noexcept final
        {
            return _subscriberMainBus;
        }

    protected:
        virtual EventBus::SubID getMainBusSubId() const noexcept final
        {
            return _subscriberSubId;
        }

    private:
        EventBus & _subscriberMainBus;
        EventBus::SubID _subscriberSubId;
    };
}
