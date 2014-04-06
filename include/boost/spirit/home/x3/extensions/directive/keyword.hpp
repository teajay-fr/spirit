/*=============================================================================
    Copyright (c) 2014 Thomas Benard

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/
#if !defined(BOOST_SPIRIT_X3_KEYWORD_NOV_29_2013)
#define BOOST_SPIRIT_X3_KEYWORD_NOV_29_2013

#if defined(_MSC_VER)
#pragma once
#endif

#include <boost/spirit/home/x3/core/parser.hpp>
#include <boost/spirit/home/x3/core/skip_over.hpp>
#include <boost/spirit/home/x3/string/detail/string_parse.hpp>
#include <boost/spirit/home/x3/support/utility/utf8.hpp>
#include <boost/spirit/home/support/char_encoding/ascii.hpp>
#include <boost/spirit/home/support/char_encoding/standard.hpp>
#include <boost/spirit/home/support/char_encoding/standard_wide.hpp>

#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/add_reference.hpp>
#include <string>


namespace boost { namespace spirit { namespace x3 {

     template <typename Left, typename Right>
     struct keyword_parser : binary_parser<Left,Right, keyword_parser<Left,Right>>
     {
       typedef binary_parser<Left, Right, keyword_parser<Left,Right> > base_type;
      
       static const bool has_attribute = true;
       static const bool is_keyword = true;

       keyword_parser(Left const& left, Right const& right)
         : base_type(left,right) {}

       template <typename Iterator, typename Context, typename Attribute>
       bool parse(Iterator& first, Iterator const& last
                  , Context const& context, Attribute& attr) const
       {
         Iterator save = first;
         if (! this->left.parse(first, last, context, unused))
           {
             first = save;
             return false;
           }
         return this->right.parse(first,last,context,attr);
       }
     };
     struct kwd_gen
     {

       template <typename Subject1>
       struct kwd_level_1
       {
         kwd_level_1(Subject1 const& sub1)
           :first_subject(sub1)
         {
         }

         template <typename Subject2>
         keyword_parser< typename extension::as_parser<Subject1>::value_type
         ,typename extension::as_parser<Subject2>::value_type >
         operator[](Subject2 const& subject) const
         {
           typedef
           keyword_parser<typename extension::as_parser<Subject1>::value_type
               ,typename extension::as_parser<Subject2>::value_type >
               result_type;

           return {as_parser(first_subject),as_parser(subject)};
         }
         Subject1 first_subject;
       };
       template <typename Subject1>
       kwd_level_1<typename extension::as_parser<Subject1>::value_type >
       operator()(Subject1 const &sub1) const
       {
         return as_parser(sub1);
       }
     };
     kwd_gen const kwd = kwd_gen();
}}}

namespace boost { namespace spirit { namespace x3 { namespace traits
{
    template <typename Left, typename Right, typename Context>
    struct attribute_of<x3::keyword_parser<Left, Right>, Context>
        : attribute_of<Right, Context> {};

}}}}


#endif
