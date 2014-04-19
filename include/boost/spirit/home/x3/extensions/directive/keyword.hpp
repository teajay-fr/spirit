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

#include <boost/spirit/home/x3/string/symbols.hpp>

#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/add_reference.hpp>
#include <string>

namespace boost { namespace spirit { namespace x3 { namespace helper {
    template <int... Is>
    struct parser_index {};

    template <int N, int... Is>
    struct gen_parser_seq : gen_parser_seq<N - 1, N - 1, Is...> {};

    template <int... Is>
    struct gen_parser_seq<0, Is...> : parser_index<Is...> {};

    template <typename Context, typename Keywords, int... Is>
    struct attribute_index {};

    template <typename Context, typename Keywords, int N, int ... Is>
    struct gen_attribute_seq : gen_attribute_seq<Context, Keywords, N - 1, N - 1, Is...> {};

    template <typename Context, typename Keywords, int ... Is>
    struct gen_attribute_seq<Context, Keywords, 0, Is...> : attribute_index<Context, Keywords, Is...> {};



}}}}
        

namespace boost { namespace spirit { namespace x3 {

     template <typename Key, typename Subject>
     struct keyword_parser : unary_parser<Subject, keyword_parser<Key,Subject> >
     {
       typedef unary_parser<Subject, keyword_parser<Key,Subject> > base_type;
      
       static const bool has_attribute = true;

       keyword_parser(Key const &key, Subject const& subject)
         : base_type(subject)
         , key(key)
         {}

       template <typename Iterator, typename Context, typename Attribute>
       bool parse(Iterator& first, Iterator const& last
                  , Context const& context, Attribute& attr) const
       {
         Iterator save = first;
         if (! key.parse(first, last, context, unused))
           {
             first = save;
             return false;
           }
         return this->subject.parse(first,last,context,attr);
       }
       Key key;
     };

     struct kwd_gen
     {

       template <typename Key>
       struct kwd_level_1
       {
         kwd_level_1(Key const& key)
           :key(key)
         {
         }

         template <typename Subject>
         keyword_parser< Key
         ,typename extension::as_parser<Subject>::value_type >
         operator[](Subject const& subject) const
         {
           return {key,as_parser(subject)};
         }
         Key key;
       };
       
       template <typename Key>
       kwd_level_1<typename extension::as_parser<Key>::value_type >
       operator()(Key const &key) const
       {
         return {as_parser(key)};
       }
     };
     kwd_gen const kwd = kwd_gen();

     template <typename lookupType>
     void add_keywords(lookupType &lookup, int index)
     {}

     template <typename lookupType, typename keywordType, typename ... rest>
     void add_keywords(lookupType &lookup, int index, keywordType const &keyword, rest const&... keywords)
     {
             lookup.add(traits::get_string_begin<char>(keyword.key.str),
                        traits::get_string_end<char>(keyword.key.str),
                        index);
             add_keywords(lookup,index+1,keywords ...);
     }

     template <typename ...Keywords> 
     struct keywords_directive : parser< keywords_directive< Keywords ...> >
     {             

       typedef parser< keywords_directive< Keywords...> > base_type;

       static const int nb_keywords = sizeof...(Keywords);

       keywords_directive(Keywords&& ... keywords)
              : keywords(std::forward<Keywords>(keywords)...)
       {
               add_keywords(lookup,0,keywords...);
       }

       template <int ParserIdx, int AttributeIdx, typename Iterator, typename Context, typename Attribute>
               bool parse_subject(Iterator& first, Iterator const& last
                  , Context const& context, Attribute& attr) const
               {
                       return std::get<ParserIdx>(keywords).parse(first,last,context,fusion::at_c<AttributeIdx>(attr));
               }
       template <typename Iterator, typename Context, typename Attribute, int... Is, int... As>
       bool parse_internal(Iterator& first, Iterator const& last
                  , Context const& context, Attribute& attr, helper::parser_index<Is...>, helper::attribute_index<Context, std::tuple<Keywords...>, As...> ) const
       {
               typedef bool (keywords_directive::* subject_caller) (Iterator &, Iterator const&, Context const&, Attribute &) const;
               static subject_caller parse_functions[nb_keywords] =
               {
                &keywords_directive::parse_subject<Is,As,Iterator,Context,Attribute>...
               };
            x3::skip_over(first, last, context);

            while(int *subparser_idx =
                    lookup.find(first,last,tst_pass_through()))
            {
                if(!(this->*parse_functions[*subparser_idx])(first,last,context,attr))
                        return false;
                x3::skip_over(first, last, context);
            }
            return true;
       }


       template <typename Iterator, typename Context, typename Attribute>
       bool parse(Iterator& first, Iterator const& last
                  , Context const& context, Attribute& attr) const
       {
               return parse_internal(first,last,context,attr,helper::parser_index<nb_keywords-1>{}, helper::attribute_index<Context,std::tuple<Keywords...>, nb_keywords -1>{});
       }

#if 0
       ///////////////////////////////////////////////////////////////////////////
        // build_parser_tags
        //
        // Builds a boost::variant from an mpl::range_c in order to "mark" every
        // parser of the fusion sequence. The integer constant is used in the parser
        // dispatcher functor in order to use the parser corresponding to the recognised
        // keyword.
        ///////////////////////////////////////////////////////////////////////////

        template <typename Sequence>
        struct build_parser_tags
        {
            // Get the sequence size
            typedef typename mpl::size< Sequence >::type sequence_size;

            // Create an integer_c constant for every parser in the sequence
            typedef typename mpl::range_c<int, 0, sequence_size::value>::type int_range;

            // Transform the range_c to an mpl vector in order to be able to transform it into a variant
            typedef typename mpl::copy<int_range, mpl::back_inserter<mpl::vector<> > >::type type;

        };

        // Build an index mpl vector
        typedef typename build_parser_tags< Elements >::type parser_index_vector;

        // build a bool array and an integer array which will be used to
        // check that the repetition constraints of the kwd parsers are
        // met and bail out a soon as possible
        typedef boost::array<bool, sizeof...keywords> flags_type;
        typedef boost::array<int, sizeof...keywords> counters_type;


       template <typename Iterator, typename Context, typename Attribute>
       bool parse(Iterator& first, Iterator const& last
                  , Context const& context, Attribute& attr) const
       {

            // Select which parse function to call
            // We need to handle the case where kwd / ikwd directives have been mixed
            // This is where we decide which function should be called.
            return parse_impl(first, last, context, skipper, attr_, mpl::true_()
//                                typename string_keywords_type::requires_one_pass()
                             );
       }

        template <typename Iterator, typename Context
          , typename Skipper, typename Attribute>
        bool parse_impl(Iterator& first, Iterator const& last
          , Context& context, Skipper const& skipper
          , Attribute& attr_,mpl::true_ /* one pass */) const
          {

            // wrap the attribute in a tuple if it is not a tuple
            typename traits::wrap_if_not_tuple<Attribute>::type attr(attr_);

            flags_type flags(flags_init);
            //flags.assign(false);

            counters_type counters;
            counters.assign(0);

            typedef repository::qi::detail::parse_dispatcher<Elements,Iterator, Context, Skipper
                                    , flags_type, counters_type
                                    , typename traits::wrap_if_not_tuple<Attribute>::type
                                    , mpl::false_ > parser_visitor_type;

            parser_visitor_type parse_visitor(elements, first, last
                                             , context, skipper, flags
                                             , counters, attr);

            typedef repository::qi::detail::complex_kwd_function< parser_visitor_type > complex_kwd_function_type;

            complex_kwd_function_type
                     complex_function(first,last,context,skipper,parse_visitor);

            // We have a bool array 'flags' with one flag for each parser as well as a 'counter'
            // array.
            // The kwd directive sets and increments the counter when a successeful parse occured
            // as well as the slot of the corresponding parser to true in the flags array as soon
            // the minimum repetition requirement is met and keeps that value to true as long as
            // the maximum repetition requirement is met.
            // The parsing takes place here in two steps:
            // 1) parse a keyword and fetch the parser index associated with that keyword
            // 2) call the associated parser and store the parsed value in the matching attribute.

            while(true)
            {

                spirit::qi::skip_over(first, last, skipper);
                Iterator save = first;
                if (string_keywords_inst.parse(first, last,parse_visitor,skipper))
                {
                    save = first;
                }
                else {
                  // restore the position to the last successful keyword parse
                  first = save;
                  if(!complex_keywords_inst.parse(complex_function))
                  {
                    first = save;
                    // Check that we are leaving the keywords parser in a successfull state
                    BOOST_FOREACH(bool &valid,flags)
                    {
                      if(!valid)
                      {
                        return false;
                      }
                    }
                    return true;
                  }
                  else
                    save = first;
                }
            }
            return false;
          }

        // Handle the mixed kwd and ikwd case
        template <typename Iterator, typename Context
          , typename Skipper, typename Attribute>
        bool parse_impl(Iterator& first, Iterator const& last
          , Context& context, Skipper const& skipper
          , Attribute& attr_,mpl::false_ /* two passes */) const
          {

            // wrap the attribute in a tuple if it is not a tuple
            typename traits::wrap_if_not_tuple<Attribute>::type attr(attr_);

            flags_type flags(flags_init);
            //flags.assign(false);

            counters_type counters;
            counters.assign(0);

            typedef detail::parse_dispatcher<Elements, Iterator, Context, Skipper
                                    , flags_type, counters_type
                                    , typename traits::wrap_if_not_tuple<Attribute>::type
                                    , mpl::false_> parser_visitor_type;

           typedef detail::parse_dispatcher<Elements, Iterator, Context, Skipper
                                    , flags_type, counters_type
                                    , typename traits::wrap_if_not_tuple<Attribute>::type
                                    , mpl::true_> no_case_parser_visitor_type;


            parser_visitor_type parse_visitor(elements,first,last
                                             ,context,skipper,flags,counters,attr);
            no_case_parser_visitor_type no_case_parse_visitor(elements,first,last
                                             ,context,skipper,flags,counters,attr);

            typedef repository::qi::detail::complex_kwd_function< parser_visitor_type > complex_kwd_function_type;

            complex_kwd_function_type
                     complex_function(first,last,context,skipper,parse_visitor);


            // We have a bool array 'flags' with one flag for each parser as well as a 'counter'
            // array.
            // The kwd directive sets and increments the counter when a successeful parse occured
            // as well as the slot of the corresponding parser to true in the flags array as soon
            // the minimum repetition requirement is met and keeps that value to true as long as
            // the maximum repetition requirement is met.
            // The parsing takes place here in two steps:
            // 1) parse a keyword and fetch the parser index associated with that keyword
            // 2) call the associated parser and store the parsed value in the matching attribute.

            while(true)
            {
                spirit::qi::skip_over(first, last, skipper);
                Iterator save = first;
                // String keywords pass
                if (string_keywords_inst.parse(first,last,parse_visitor,no_case_parse_visitor,skipper))
                {
                    save = first;
                }
                else {
                  first = save;

                  if(!complex_keywords_inst.parse(complex_function))
                  {
                    first = save;
                    // Check that we are leaving the keywords parser in a successfull state
                    BOOST_FOREACH(bool &valid,flags)
                    {
                      if(!valid)
                      {
                        return false;
                      }
                    }
                    return true;
                  }
                  else
                  {
                    save = first;
                  }
                }
            }
            return false;
          }
#endif
       //typedef typename make_variant_over< boost::mpl::int_<helper::gen_seq<sizeof...(Keywords)> >... >::type keyword_index;
//       typedef std::tuple< boost::mpl::int_<helper::gen_seq<sizeof...(Keywords)> >... > keyword_index;
       tst<char, int> lookup;
       std::tuple<Keywords ...> keywords;
     }; 

     struct keywords_directive_gen
     {
             template <typename ... Keywords>
             keywords_directive<Keywords...>
             operator()(Keywords&&... keywords) const
             {
                     return {std::forward<Keywords>(keywords)...};
             }
     };     
     keywords_directive_gen const keywords = keywords_directive_gen();
}}}

namespace boost { namespace spirit { namespace x3 { namespace traits
{
    template <typename Key, typename Subject, typename Context>
    struct attribute_of<x3::keyword_parser<Key, Subject>, Context>
        : attribute_of<Subject, Context> {};

}}}}


#endif
