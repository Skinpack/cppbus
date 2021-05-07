#pragma once
#include <atomic>
#include <cassert>
#include <cinttypes>
#include <climits>
#include <functional>
#include <memory>
#include <optional>
#include <tuple>
#include <type_traits>
#include <typeindex>
#include <typeinfo>
#include <unordered_map>

#include "Event.h"
#include "EventsRelation.h"

namespace cppbus {
    namespace detail {
        template<typename T>
        struct remove_cvref {
            using type = std::remove_const_t<std::remove_reference_t<T>>;
        };

        template<typename T>
        using remove_cvref_t = typename remove_cvref<T>::type;

        template<typename Base, typename Derived>
        struct is_public_base_of
            : public std::integral_constant<bool,
            std::is_base_of_v<Base, Derived> &&
            std::is_convertible_v<const Derived *, const Base *>
            > {
        };

        template<typename Base, typename Derived>
        static inline constexpr bool is_public_base_of_v = is_public_base_of<Base, Derived>::value;

        template<typename From, typename To, typename _Enable = void>
        struct can_static_cast : public std::false_type {};

        template<typename From, typename To>
        struct can_static_cast<From, To, std::void_t<decltype(static_cast<To>(std::declval<From>()))>> : public std::true_type {};

        template<typename From, typename To>
        static inline constexpr bool can_static_cast_v = can_static_cast<From, To>::value;

        template<typename T>
        struct method_traits;

        template<typename T, typename RetT, typename... Args>
        struct method_traits<RetT(T::*)(Args...)> {
            using this_type = T;
            using ret_type = RetT;

            template<size_t ArgIdx>
            using arg_type = typename std::tuple_element<ArgIdx, std::tuple<Args...>>::type;

            static constexpr size_t args_count = sizeof...(Args);
            static constexpr bool is_const = false;
            static constexpr bool is_lvalue = false;
            static constexpr bool is_rvalue = false;
        };

        template<typename T, typename RetT, typename... Args>
        struct method_traits<RetT(T::*)(Args...) const> : public method_traits<RetT(T::*)(Args...)> {
            static constexpr bool is_const = true;
        };

        template<typename T, typename RetT, typename... Args>
        struct method_traits<RetT(T::*)(Args...) &> : public method_traits<RetT(T::*)(Args...)> {
            static constexpr bool is_lvalue = true;
        };

        template<typename T, typename RetT, typename... Args>
        struct method_traits<RetT(T::*)(Args...) &&> : public method_traits<RetT(T::*)(Args...)> {
            static constexpr bool is_rvalue = true;
        };

        template<typename T, typename RetT, typename... Args>
        struct method_traits<RetT(T::*)(Args...) const &> : public method_traits<RetT(T::*)(Args...)> {
            static constexpr bool is_const = true;
            static constexpr bool is_lvalue = true;
        };

        template<typename T, typename RetT, typename... Args>
        struct method_traits<RetT(T::*)(Args...) const &&> : public method_traits<RetT(T::*)(Args...)> {
            static constexpr bool is_const = true;
            static constexpr bool is_rvalue = true;
        };

        template<typename F>
        struct is_method_ptr : public std::false_type {};

        template<typename ThisType, typename RetType, typename... Args>
        struct is_method_ptr<RetType(ThisType::*)(Args...)> : public std::true_type {};

        template<typename ThisType, typename RetType, typename... Args>
        struct is_method_ptr<RetType(ThisType::*)(Args...) const> : public std::true_type {};

        template<typename ThisType, typename RetType, typename... Args>
        struct is_method_ptr<RetType(ThisType::*)(Args...) &> : public std::true_type {};

        template<typename ThisType, typename RetType, typename... Args>
        struct is_method_ptr<RetType(ThisType::*)(Args...) &&> : public std::true_type {};

        template<typename ThisType, typename RetType, typename... Args>
        struct is_method_ptr<RetType(ThisType::*)(Args...) const &> : public std::true_type {};

        template<typename ThisType, typename RetType, typename... Args>
        struct is_method_ptr<RetType(ThisType::*)(Args...) const &&> : public std::true_type {};

        template<typename F>
        inline constexpr bool is_method_ptr_v = is_method_ptr<F>::value;

        template<typename F, typename _Enable = void>
        struct is_functor : public std::false_type {};

        template<typename F>
        struct is_functor<F,
            std::enable_if_t<is_method_ptr_v<decltype(&std::remove_reference_t<F>::operator())>>>
            : public std::true_type {
        };

        template<typename F>
        inline constexpr bool is_functor_v = is_functor<F>::value;

        template<typename F, typename _Enable = void>
        struct functor_traits;

        template<typename F>
        struct functor_traits<F, std::enable_if_t<is_functor_v<F>>>
            : public method_traits<decltype(&std::remove_reference_t<F>::operator())> {
        };
    }

    class EventBus {
    public:
        struct
#if __has_cpp_attribute(nodiscard) >= 201907L  // nodiscard("message")
            [[nodiscard("The client MUST unsubscribe during its destruction to prevent calling invalid callbacks")]]
#else
            [[nodiscard]]
#endif
        SubID {
            constexpr SubID() noexcept : SubID(invalid()) {}
            constexpr SubID(const SubID & other) noexcept : value(other.value) {}
            constexpr SubID & operator=(const SubID & other) noexcept
            {
                value = other.value;
                return *this;
            }
            ~SubID() noexcept = default;

            // Comparison operators
            constexpr bool operator<(const SubID & other) const noexcept
            {
                return value < other.value;
            }

            constexpr bool operator<=(const SubID & other) const noexcept
            {
                return value <= other.value;
            }

            constexpr bool operator==(const SubID & other) const noexcept
            {
                return value == other.value;
            }

            constexpr bool operator>=(const SubID & other) const noexcept
            {
                return value >= other.value;
            }

            constexpr bool operator>(const SubID & other) const noexcept
            {
                return value > other.value;
            }

            constexpr bool operator!=(const SubID & other) const noexcept
            {
                return value != other.value;
            }

            constexpr size_t hash() const noexcept
            {
                return robin_hood_hash_int(value);
            }

            constexpr bool isValid() const noexcept
            {
                return *this != invalid();
            }

            constexpr operator bool() const noexcept
            {
                return isValid();
            }

            constexpr static SubID invalid() noexcept
            {
                return SubID(static_cast<InternalValue>(-1));
            }

        private:
            friend class EventBus;
            using InternalValue = uint64_t;

            constexpr explicit SubID(InternalValue newVal) : value(newVal) {}

            static SubID createUnique() noexcept
            {
                return SubID(freeID++);
            }

            template <typename T>
            static constexpr T rotr(T x, uint32_t k) noexcept
            {
                return (x >> k) | (x << (CHAR_BIT * sizeof(T) - k));
            }

            // Snippet source: https://github.com/martinus/robin-hood-hashing
            static constexpr size_t robin_hood_hash_int(uint64_t x) noexcept
            {
                // inspired by lemire's strongly universal hashing
                // https://lemire.me/blog/2018/08/15/fast-strongly-universal-64-bit-hashing-everywhere/
                //
                // Instead of shifts, we use rotations so we don't lose any bits.
                //
                // Added a final multiplcation with a constant for more mixing. It is most important that
                // the lower bits are well mixed.
                auto h1 = x * UINT64_C(0xA24BAED4963EE407);
                auto h2 = rotr(x, 32U) * UINT64_C(0x9FB21C651E98DF25);
                auto h = rotr(h1 + h2, 32U);
                return static_cast<size_t>(h);
            }

            InternalValue value;
            static inline std::atomic<InternalValue> freeID = 1;
        };

        struct SubIDHasher {
            constexpr std::size_t operator()(const EventBus::SubID & key) const noexcept
            {
                return key.hash();
            }
        };

        using GenericCallback = std::function<void(const Event &)>;

        EventBus() = default;
        ~EventBus() = default;

        static SubID allocateSubID() noexcept
        {
            return SubID::createUnique();
        }

        /*
        * subscribe() overloads:
        * 1. subscribe(F && functor, const SubID & subId):
        *   Accepts an object that has non-overloaded operator() accepting
        *   one public child of Event and returning void.
        *   Copies functor.
        *
        * 2. subscribe(void(*fnPtr)(ArgType)), const SubID & subId:
        *   Accepts a function pointer.
        *
        * 3. subscribe(ThisPtrType thisPtr, MethodType methodPtr, const SubID & subId):
        *   Accepts a pointer to `this` and a method of a decltype(*this) class.
        *
        * 4. subscribe<EventType>(GenericCallback callback, const SubID & subId)
        *   An 'old-style' overload. Accepts EventType as a template parameter and
        *   a generic callback of a form (const Event &) -> void.
        *   Copies callback.
        */

        // 1
        template<typename F>
        auto subscribe(F && functor, const SubID & subId) -> std::enable_if_t<detail::is_functor_v<F>>
        {
            assert(subId != SubID::invalid());

            using FunctorTraits = detail::functor_traits<F>;
            static_assert(FunctorTraits::args_count == 1,
                          "A functor passed in EventBus::subscribe() must accept exactly one argument");
            static_assert(std::is_same_v<typename FunctorTraits::ret_type, void>,
                          "A functor passed in EventBus::subscribe() must not return a value");
            static_assert(FunctorTraits::is_rvalue == false,
                          "A functor passed in EventBus::subscribe() must have a "
                          "non-rvalue operator()");

            using FunctorArg = typename FunctorTraits::template arg_type<0>;
            using FunctorArgBase = detail::remove_cvref_t<FunctorArg>;

            static_assert(detail::is_public_base_of_v<Event, FunctorArgBase>,
                          "A functor passed in EventBus::subscribe() must accept a public "
                          "child of Event");
            static_assert(!std::is_reference_v<FunctorArg> || (std::is_const_v<std::remove_reference_t<FunctorArg>>),
                          "A functor passed in EventBus::subscribe() must accept either a non-reference "
                          "argument or a (const T &) one");

            subscribeImpl<FunctorArgBase>(
                [func = std::forward<F>(functor)](const Event & ev) mutable {
                if constexpr (detail::can_static_cast_v<const Event &, const FunctorArgBase &>) {
                    func(static_cast<const FunctorArgBase &>(ev));
                } else {
                    func(dynamic_cast<const FunctorArgBase &>(ev));
                }
            },
                subId);
        }

        // 2
        template<typename ArgType>
        auto subscribe(void(*fnPtr)(ArgType), const SubID & subId) -> void
        {
            assert(subId != SubID::invalid());
            assert(fnPtr != nullptr);

            using ArgBase = detail::remove_cvref_t<ArgType>;
            static_assert(detail::is_public_base_of_v<Event, ArgBase>,
                          "A function passed in EventBus::subscribe() must accept a public "
                          "child of Event");
            static_assert(!std::is_reference_v<ArgType> || (std::is_const_v<std::remove_reference_t<ArgType>>),
                          "A function passed in EventBus::subscribe() must accept either a non-reference "
                          "argument or a (const T &) one");

            subscribeImpl<ArgBase>([fnPtr](const Event & ev) {
                if constexpr (detail::can_static_cast_v<const Event &, const ArgBase &>) {
                    fnPtr(static_cast<const ArgBase &>(ev));
                } else {
                    fnPtr(dynamic_cast<const ArgBase &>(ev));
                }
                                   }, subId);
        }

        // 3
        template<typename ThisPtrType, typename MethodType>
        auto subscribe(ThisPtrType thisPtr, MethodType methodPtr, const SubID & subId)
            -> std::enable_if_t<
            detail::is_method_ptr_v<MethodType> &&
            std::is_pointer_v<ThisPtrType>
            >
        {
            assert(subId != SubID::invalid());

            using ThisType = std::remove_pointer_t<ThisPtrType>;

            using MethodTraits = detail::method_traits<MethodType>;
            static_assert(std::is_same_v<std::remove_cv_t<ThisType>, typename MethodTraits::this_type>,
                          "A method passed in EventBus::subscribe() must have "
                          "decltype(*this) == std::remove_cv_t<ThisType>");
            static_assert(MethodTraits::args_count == 1,
                          "A method passed in EventBus::subscribe() must accept exactly one argument");
            static_assert(std::is_same_v<typename MethodTraits::ret_type, void>,
                          "A method passed in EventBus::subscribe() must not return a value");

            using ArgType = typename MethodTraits::template arg_type<0>;
            using ArgBase = detail::remove_cvref_t<ArgType>;
            static_assert(detail::is_public_base_of_v<Event, ArgBase>,
                          "A method passed in EventBus::subscribe() must accept a public "
                          "child of Event");
            static_assert(!std::is_reference_v<ArgType> || (std::is_const_v<std::remove_reference_t<ArgType>>),
                          "A method passed in EventBus::subscribe() must accept either a non-reference "
                          "argument or a (const T &) one");

            subscribeImpl<ArgBase>([methodPtr, thisPtr](const Event & ev) mutable {
                if constexpr (detail::can_static_cast_v<const Event &, const ArgBase &>) {
                    std::invoke(methodPtr, thisPtr, static_cast<const ArgBase &>(ev));
                } else {
                    std::invoke(methodPtr, thisPtr, dynamic_cast<const ArgBase &>(ev));
                }
                                   }, subId);
        }

        // 4
        template<typename EventType>
        auto subscribe(GenericCallback callback, const SubID & subId) -> void
        {
            assert(subId != SubID::invalid());

            subscribeImpl<EventType>(std::move(callback), subId);
        }

        void unsubscribe(const SubID & subId)
        {
            if (subId != SubID::invalid()) {
                for (auto & [typeIndex, clientsMap] : callbacks) {
                    clientsMap.erase(subId);
                }
            }
        }

        // Checks if any callbacks are registered for EventType or 
        // any of EventType's registered parents (recursively)
        template<typename EventType>
        bool haveReceivers() const
        {
            static_assert(detail::is_public_base_of_v<Event, EventType>,
                          "EventType must be a public subtype of Event");
            return haveReceiversImpl(Event::getStaticIndex<EventType>());
        }

        void publish(const Event & ev) const
        {
            publishImpl(Event::getRuntimeIndex(ev), ev);
        }

        template<typename EventType, typename... Args>
        bool publishEmplace(Args &&... args) const
        {
            static_assert(detail::is_public_base_of_v<Event, EventType>,
                          "EventType must be a public subtype of Event");
            auto typeIdx = Event::getStaticIndex<EventType>();

            std::optional<EventType> eventOpt{};  // Initially empty
            publishEmplaceImpl<EventType>(typeIdx, eventOpt, std::forward<Args>(args)...);

            return eventOpt.has_value();
        }

    private:
        template<typename EventType>
        void subscribeImpl(GenericCallback callback, const SubID & subId)
        {
            static_assert(detail::is_public_base_of_v<Event, EventType>,
                          "EventType must be a public subtype of Event");

            callbacks[Event::getStaticIndex<EventType>()].insert_or_assign(subId, std::move(callback));
        }

        bool haveReceiversImpl(const std::type_index & typeIdx) const
        {
            if (callbacks.find(typeIdx) != callbacks.end()) {
                return true;
            }

            auto range = EventsRelation::RelationManager::getParentsRange(typeIdx);
            for (auto it = range.first; it != range.second; ++it) {
                if (haveReceiversImpl(it->second)) {  // Recursive call, DFS
                    return true;
                }
            }

            return false;
        }

        void publishImpl(const std::type_index & typeIdx, const Event & ev) const
        {
            if (auto it = callbacks.find(typeIdx); it != callbacks.end()) {
                for (const auto & [subId, callback] : it->second) {
                    callback(ev);
                }
            }

            auto range = EventsRelation::RelationManager::getParentsRange(typeIdx);
            for (auto it = range.first; it != range.second; ++it) {
                publishImpl(it->second, ev);  // Recursive call, DFS
            }
        }

        template<typename EventType, typename... Args>
        void publishEmplaceImpl(const std::type_index & typeIdx,
                                std::optional<EventType> & evOpt,
                                Args &&... args) const
        {
            if (auto it = callbacks.find(typeIdx); it != callbacks.end()) {
                for (const auto & [subId, callback] : it->second) {
                    if (!evOpt.has_value()) {
                        evOpt.emplace(std::forward<Args>(args)...);
                    }
                    callback(evOpt.value());
                }
            }

            auto range = EventsRelation::RelationManager::getParentsRange(typeIdx);
            for (auto it = range.first; it != range.second; ++it) {
                if (!evOpt.has_value()) {
                    publishEmplaceImpl<EventType>(it->second, evOpt, std::forward<Args>(args)...);
                } else {
                    publishImpl(it->second, evOpt.value());
                }
            }
        }

        // event_type -> (subscriber_id -> callback)
        std::unordered_map<std::type_index, std::unordered_map<SubID, GenericCallback, SubIDHasher>> callbacks{};
    };
}
