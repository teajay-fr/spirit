/*=============================================================================
    Copyright (c) 2001-2013 Joel de Guzman
    Copyright (c) 2014 Thomas Bernard

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(SPIRIT_KEYWORD_DETAIL_JAN_06_2013_1015AM)
#define SPIRIT_KEYWORD_DETAIL_JAN_06_2013_1015AM

#if defined(_MSC_VER)
#pragma once
#endif

#include <boost/spirit/home/x3/support/traits/attribute_of.hpp>
#include <boost/spirit/home/x3/support/traits/attribute_category.hpp>
#include <boost/spirit/home/x3/support/traits/make_attribute.hpp>
#include <boost/spirit/home/x3/support/traits/has_attribute.hpp>
#include <boost/spirit/home/x3/support/traits/is_substitute.hpp>
#include <boost/spirit/home/x3/support/traits/container_traits.hpp>
#include <boost/spirit/home/x3/core/detail/parse_into_container.hpp>
#include <boost/spirit/home/x3/support/traits/string_traits.hpp>

#include <boost/fusion/include/begin.hpp>
#include <boost/fusion/include/end.hpp>
#include <boost/fusion/include/advance.hpp>
#include <boost/fusion/include/empty.hpp>
#include <boost/fusion/include/front.hpp>
#include <boost/fusion/include/iterator_range.hpp>
#include <boost/fusion/include/as_deque.hpp>
#include <boost/fusion/include/mpl.hpp>
#include <boost/fusion/include/advance.hpp>

#include <boost/mpl/copy_if.hpp>
#include <boost/mpl/not.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/identity.hpp>

#include <boost/type_traits/add_reference.hpp>
#include <boost/type_traits/is_same.hpp>

namespace boost { namespace spirit { namespace x3
{
    template <typename Left, typename Right>
    struct keyword;
}}}

namespace boost { namespace spirit { namespace x3 { namespace detail
{

    template <typename Parser, typename Context, typename Enable = void>
    struct keyword_list_size
    {
        static int const value = traits::has_attribute<typename Parser::right_type, Context>::value;
    };

    template <typename Parser, typename Context>
    struct keyword_list_size_subject
      : keyword_list_size<typename Parser::subject_type, Context> {};

    template <typename Parser, typename Context>
    struct keyword_list_size<Parser, Context
      , typename enable_if_c<(Parser::Right::is_pass_through_unary)>::type >
      : keyword_list_size_subject<typename Parser::Right, Context> {};

    template <typename L, typename R, typename Context>
    struct keyword_list_size<keyword<L, R>, Context>
    {
        static int const value =
            keyword_list_size<L, Context>::value +
            keyword_list_size<R, Context>::value;
    };

    template <typename Attribute, int Start, int End, typename Result, typename Enable = void>
    struct get_attribute
    {
      typedef typename fusion::result_of::at_c<Attribute,Start>::type type;
      static typename add_reference<type>::type call(Attribute &s)
      {
        return fusion::at_c<Start>(s);
      }

    };
    template <typename Attribute, int Start, int End, typename Result>
   struct get_attribute<Attribute, Start, End, Result, typename enable_if_c<
       (is_same<Attribute, unused_type>::value || is_same<Result, unused_type>::value)>::type >
   {
     typedef unused_type type;
     static unused_type call(Attribute &s)
     {
       return unused;
     }
   };

    template <typename L, typename R, typename Attribute, typename Context, int Offset
      , typename Enable = void>
    struct partition_keyword_attribute
    {
        static int const l_size = keyword_list_size<L, Context>::value;
        static int const r_size = keyword_list_size<R, Context>::value;

        // If you got an error here, then you are trying to pass
        // a fusion sequence with the wrong number of elements
        // as that expected by the (sequence) parser.
        static_assert(
            fusion::result_of::size<Attribute>::value - Offset == (l_size + r_size )
          , "Attribute does not have the expected size."
        );

        typedef typename fusion::result_of::begin<Attribute>::type l_begin;
        typedef typename fusion::result_of::advance_c<l_begin, Offset>::type l_start;
        typedef typename fusion::result_of::advance_c<l_start, l_size>::type l_end;
        typedef typename fusion::result_of::advance_c<l_start, r_size>::type r_end;
        typedef fusion::iterator_range<l_start, l_end> l_part;
        typedef fusion::iterator_range<l_end, r_end> r_part;
        typedef get_attribute<Attribute, Offset, l_size, l_part> l_get_attr;
        typedef get_attribute<Attribute, l_size+Offset, l_size+r_size+Offset, r_part> r_get_attr;

        static l_part left(Attribute& s)
        {
            auto i = fusion::begin(s);
            return l_part(i, fusion::advance_c<l_size>(i));
        }

        static r_part right(Attribute& s)
        {
            return r_part(
                fusion::advance_c<l_size>(fusion::begin(s))
              , fusion::end(s));
        }
    };

    template <typename Subject, typename Partition>
    struct keyword_parser_holder
    {
       typedef Partition partition_attribute;

       keyword_parser_holder(const Subject *parser)
         : parser(parser)
       {

       }
       const Subject *parser;
    };

    template <typename L, typename R, typename Context, typename Attribute, typename Enable = void>
    struct get_kwd_parser_types
    {
      typedef typename traits::attribute_of< typename R::right_type, Context>::type right_attribute_type;
      static int const l_size = keyword_list_size<L, Context>::value;
      static int const r_size = keyword_list_size<R, Context>::value;

      typedef get_attribute<Attribute, l_size, l_size+r_size, right_attribute_type> get_right_attribute;
      typedef keyword_parser_holder< typename R::right_type, get_right_attribute> right_kwd_parser_holder;

      typedef get_kwd_parser_types<
                                  typename L::left_type
                                , typename L::right_type
                                , Context
                                , Attribute > left_get_kwd_parser;
        typedef typename
            mpl::push_front<
                typename left_get_kwd_parser::type
              , right_kwd_parser_holder
            >::type type;

            template <typename Keywords>
            static void add_keyword(Keywords &keywords, const keyword<L, R> &parser)
            {
              left_get_kwd_parser::add_keyword(keywords,parser.left);
              keywords.add(traits::get_string_begin<char>(parser.right.left.str)
                           ,traits::get_string_end<char>(parser.right.left.str)
                           ,right_kwd_parser_holder(parser.right.right.derived_ptr()));
            }
    };

   template <typename L, typename R, typename Context, typename Attribute>
    struct get_kwd_parser_types<L,R,Context,Attribute, typename enable_if_c< R::is_keyword && L::is_keyword >::type>
    {
      typedef typename traits::attribute_of< typename R::right_type, Context>::type right_attribute_type;
      typedef typename traits::attribute_of< typename L::right_type, Context>::type left_attribute_type;
      static int const l_size = keyword_list_size<L, Context>::value;
      static int const r_size = keyword_list_size<R, Context>::value;

      typedef get_attribute<Attribute, l_size, l_size+r_size, right_attribute_type> get_right_attribute;
      typedef get_attribute<Attribute, 0, l_size, left_attribute_type> get_left_attribute;
      typedef keyword_parser_holder< typename R::right_type, get_right_attribute> right_kwd_parser_holder;
      typedef keyword_parser_holder< typename L::right_type, get_left_attribute> left_kwd_parser_holder;


        typedef typename
            mpl::vector<
                left_kwd_parser_holder
              , right_kwd_parser_holder
            > type;



            template <typename Keywords>
            static void add_keyword(Keywords &keywords, const keyword<L, R> &parser)
            {
              keywords.add(traits::get_string_begin<char>(parser.left.left.str)
                           ,traits::get_string_end<char>(parser.left.left.str)
                           ,left_kwd_parser_holder(parser.left.right.derived_ptr()));

              keywords.add(traits::get_string_begin<char>(parser.right.left.str)
                           ,traits::get_string_end<char>(parser.right.left.str)
                           ,right_kwd_parser_holder(parser.right.right.derived_ptr()));
            }
    };


    template <typename L, typename R, typename Context, typename Attribute>
    struct keyword_parser_variant
    {
        // Get all sequence attribute types
        typedef typename get_kwd_parser_types<L, R, Context, Attribute>::type all_types;

        typedef typename make_variant_over<all_types>::type type;

    };


    template <typename Iterator, typename Context>
    struct keyword_parse_visitor_unused : boost::static_visitor<bool>
    {
        keyword_parse_visitor_unused(Iterator &first, Iterator const& last, Context const& context)
          : first(first)
          , last(last)
          , context(context)
        {

        }
        template <typename Parser>
        bool operator()(Parser &parser) const
        {
          Iterator save = first;
          if (parser.parser->parse(first, last, context, unused))
              return true;
          first = save;
          return false;
        }
        Iterator & first;
        Iterator const& last;
        Context const& context;
    };

    template <typename Iterator, typename Context, typename Attribute>
    struct keyword_parse_visitor : boost::static_visitor<bool>
    {
        keyword_parse_visitor(Iterator &first, Iterator const& last, Context const& context, Attribute &attr )
          : first(first)
          , last(last)
          , context(context)
          , attr(attr)

        {

        }
        template <typename Parser>
        bool operator()(Parser &parser) const
        {
          typedef typename Parser::partition_attribute partition_attribute;
          typename partition_attribute::type current_attr = partition_attribute::call(attr);
          Iterator save = first;
          if (parser.parser->parse(first, last, context, current_attr))
              return true;
          first = save;
          return false;
        }
        Iterator & first;
        Iterator const& last;
        Context const& context;
        Attribute &attr;
    };
}}}}

#endif
