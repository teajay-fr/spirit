/*=============================================================================
    Copyright (c) 2001-2013 Joel de Guzman
    Copyright (c) 2014 Thomas Bernard

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(SPIRIT_KEYWORD_JAN_07_2013_1131AM)
#define SPIRIT_KEYWORD_JAN_07_2013_1131AM

#if defined(_MSC_VER)
#pragma once
#endif

#include <boost/spirit/home/x3/support/traits/attribute_of.hpp>
#include <boost/spirit/home/x3/core/parser.hpp>
#include <boost/spirit/home/x3/operator/detail/sequence.hpp>
#include <boost/spirit/home/x3/string/symbols.hpp>
#include <boost/spirit/home/x3/extensions/directive/keyword.hpp>
#include <boost/spirit/home/x3/extensions/operator/detail/keyword.hpp>

namespace boost { namespace spirit { namespace x3
{
    template <typename Left, typename Right>
    struct keyword : binary_parser< Left, Right, keyword<Left , Right>>
    {
        typedef binary_parser<Left, Right, keyword< Left, Right>> base_type;

        static const bool is_keyword = false;
        keyword(Left left, Right right)
            : base_type(left, right) {

        }

        template <typename Iterator, typename Context>
        bool parse(
            Iterator& first, Iterator const& last
          , Context const& context, unused_type) const
        {
          typedef detail::get_kwd_parser_types<Left, Right, Context, unused_type> parser_types;
          typedef typename make_variant_over< typename parser_types::type>::type parser_variant;
          typedef tst<char, parser_variant> Lookup;
          static Lookup lookup;
          if(lookup.empty())
            {
              parser_types::add_keyword(lookup,*this);
            }
            x3::skip_over(first, last, context);
            while(parser_variant *subparser =
                    lookup.find(first,last,tst_pass_through()))
            {
                if(!boost::apply_visitor(
                      detail::keyword_parse_visitor_unused<Iterator,Context>(first,last,context),*subparser))
                  return false;
                x3::skip_over(first, last, context);
            }
            return true;
        }

        template <typename Iterator, typename Context, typename Attribute>
        bool parse(
            Iterator& first, Iterator const& last
          , Context const& context, Attribute& attr) const
        {
          typedef detail::get_kwd_parser_types<Left, Right, Context, Attribute> parser_types;
          typedef typename make_variant_over< typename parser_types::type>::type parser_variant;
          typedef tst<char, parser_variant> Lookup;
          static Lookup lookup;
          if(lookup.empty())
            {
              parser_types::add_keyword(lookup,*this);
            }
            x3::skip_over(first, last, context);
            while(parser_variant *subparser =
                    lookup.find(first,last,tst_pass_through()))
            {
                if(!boost::apply_visitor(
                      detail::keyword_parse_visitor<Iterator,Context,Attribute>(first,last,context,attr),*subparser))
                  return false;
                x3::skip_over(first, last, context);
            }
            return true;
        }
    };

    template <typename Left, typename Right>
    inline keyword<
        typename extension::as_parser<Left>::value_type
      , typename extension::as_parser<Right>::value_type>
    operator||(Left const& left, Right const& right)
    {
        return {as_parser(left), as_parser(right)};
    }
}}}

namespace boost { namespace spirit { namespace x3 { namespace traits
{
    template <typename LeftKey, typename LeftSubject, typename RightKey, typename RightSubject, typename Context>
    struct attribute_of<x3::keyword<keyword_parser<LeftKey,LeftSubject>, keyword_parser<RightKey,RightSubject>>, Context>
        : x3::detail::attribute_of_sequence<LeftSubject, RightSubject, Context> {};

}}}}

#endif
