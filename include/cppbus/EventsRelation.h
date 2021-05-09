#pragma once
#include <type_traits>
#include <typeindex>
#include <typeinfo>
#include <utility>
#include <unordered_map>

namespace cppbus {
    namespace EventsRelation {
        namespace detail {
            // A type that is returned by the registerRelation() call during dynamic-initialization
            struct registratorType {
                /*
                [basic.stc.static]/2:
                    If a variable with static storage duration has initialization or a destructor with
                    side effects, it shall not be eliminated even if it appears to be unused, except that
                    a class object or its copy/move may be eliminated as specified in [class.copy.elision].
                [intro.execution]/7:
                    Reading an object designated by a volatile glvalue ([basic.lval]), modifying an object,
                    calling a library I/O function, or calling a function that does any of those operations
                    are all side effects [...]
                */
                registratorType() :
                    side_effect_1(42)
                {
                    side_effect_2 = side_effect_1;
                }

            private:
                volatile char side_effect_1;
                volatile char side_effect_2;
            };
        }

        struct RelationManager final {
            using RelationsMap = std::unordered_multimap<std::type_index, std::type_index>;

            template<typename Child, typename... Parents>
            friend struct registrator;

            static std::pair<RelationsMap::const_iterator, RelationsMap::const_iterator>
                getParentsRange(const std::type_index & type);

        private:

            RelationManager() = delete;

            // A function that registers a Parent <- Child pair relation
            template<typename Child, typename Parent>
            static void registerRelationPair()
            {
                static_assert(std::is_base_of_v<Parent, Child>, "Child must be a subtype of Parent");
                static_assert(!std::is_same_v<Parent, Child>, "Child must not be the same type as Parent");
                registerRelationImpl(typeid(Child), typeid(Parent));
            }

            // A function that registers a 1-N relation betwen a child and its parents;
            // this function is used as an initializator for a static storage duration class member
            // of the registrator class
            template<typename Child, typename Parent, typename... ParentsRest>
            static auto registerRelation()
            {
                registerRelationPair<Child, Parent>();
                if constexpr (sizeof...(ParentsRest) > 0) {
                    return registerRelation<Child, ParentsRest...>();
                } else {
                    return detail::registratorType();
                }
            }

            static void registerRelationImpl(const std::type_info & child, const std::type_info & parent);
        };

        // A registrator class.
        // To register a Parents <- Child relation user must derive their type from
        // the correct specialization of this class
        template<typename Child, typename... Parents>
        struct registrator {
        private:
            // The registerRelation() function would be called during dynamic initialization of this
            // static storage duration variable.
            static volatile inline detail::registratorType _registrator
                = RelationManager::registerRelation<Child, Parents...>();

        protected:
            /*
            [basic.start.dynamic]/6:
                It is implementation-defined whether the dynamic initialization of a non-block inline variable
                with static storage duration is sequenced before the first statement of main or is deferred.
                If it is deferred, it strongly happens before any non-initialization odr-use of that variable.
            [basic.start.dynamic]/4:
                A non-initialization odr-use is an odr-use ([basic.def.odr]) not caused directly or indirectly
                by the initialization of a non-block static or thread storage duration variable.
            By odr-using the _registrator here we are making sure that any Child constructor is odr-using it as well,
            thus guaranteeing that the compiler would call registerRelation() strongly before the Child constructor
            */
            constexpr registrator() noexcept
            {
                // Taking an address of a variable, even in a discarded statement, is an odr-use
                (void)&_registrator;
            }

        private:
            /*
            If registrator() constructor is never 'non-initialization odr-used'
            (no Child objects are created besides non-local static ones), then the only
            instantiation of the registrator class is an implicit instantiation caused by deriving Child;

            [temp.inst]/3:
                The implicit instantiation of a class template specialization causes
                (3.1) -- the implicit instantiation of the declarations, but not of the definitions, of
                    the non-deleted class member functions, member classes, scoped member enumerations,
                    static data members, member templates, and friends; and
                (3.2) -- the implicit instantiation of the definitions of deleted member functions,
                    unscoped member enumerations, and member anonymous unions.

            [basic.def.odr]/8:
                [...] A constructor for a class is odr-used as specified in [dcl.init].
            (In other words, "A constructor (including default constructors) for a class
             is odr-used by the initialization that selects it.")

            The problem is that if there is no Child() constructor calls, then the compiler is not required to
            even DEFINE our registrator() constructor, and since there is only one odr-use of the static inline
            variable _registrator, the compiler is not required to generate a definition for it as well. And since
            it is not generating a definition, the initialization is also not generated.

            But [temp.inst]/3.2 requires that any implicit instantiation of a class template also
            causes the DEFINITION instantiation of an unscoped member enumerations. Next,
            [dcl.spec.auto.general]/12:
                Return type deduction for a templated entity that is a function or function template
                with a placeholder in its declared type occurs when the definition is instantiated
                even if the function body contains a return statement with a non-type-dependent operand
            Thus, using the sizeof(*_force_registrator_instantiation()) causes the implicit instantiation
            of definition of _force_registrator_instantiation(), and since this function is odr-using _registrator,
            its definition (and initialization) is also generated.

            The only problem is that by [basic.start.dynamic]/6 the implementation MAY defer the dynamic
            initialization of _registrator to the point where it is 'non-initialization odr-used', and since without
            the Child() constructor call we don't have any way to non-initialization odr-use it, we also have no way to
            make sure that the compiler invokes registerRelation() before main() or before any other point of execution.
            The good news, though, is that all three modern compilers obediently initializes _registrator before the first
            statement of main().
            */
            static auto _force_registrator_instantiation()
            {
                return &_registrator;
            }

            enum {
                _ROTARTSIGER_ETAITNATSNI = sizeof(*_force_registrator_instantiation()),
            };
        };

        namespace detail {
            inline RelationManager::RelationsMap & getEventsRelationMapping()
            {
                static RelationManager::RelationsMap mapping{};
                return mapping;
            }

            inline const RelationManager::RelationsMap & cgetEventsRelationMapping()
            {
                return getEventsRelationMapping();
            }
        }

        inline std::pair<RelationManager::RelationsMap::const_iterator, RelationManager::RelationsMap::const_iterator>
            RelationManager::getParentsRange(const std::type_index & typeIdx)
        {
            return detail::cgetEventsRelationMapping().equal_range(typeIdx);
        }

        inline void RelationManager::registerRelationImpl(const std::type_info & child, const std::type_info & parent)
        {
            auto & mapping = detail::getEventsRelationMapping();
            mapping.emplace(std::type_index(child), std::type_index(parent));
        }
    }
}
