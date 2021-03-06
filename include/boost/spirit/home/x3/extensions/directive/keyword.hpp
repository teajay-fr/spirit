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

#include <string>

namespace boost { namespace spirit { namespace x3 { namespace detail {
    template <int... Is>
    struct parser_index {};

    template <int N, int... Is>
    struct gen_parser_seq : gen_parser_seq<N - 1, N - 1, Is...> {};

    template <int... Is>
    struct gen_parser_seq<0, Is...> : parser_index<Is...> {};

    template <int... Is>
    struct attribute_index {};

    template <typename Context, typename Keywords, int AttrIdx,int ParserIdx, int N, int ... Is>
    struct gen_attribute_seq;

    template <typename Context, typename Keywords, int AttrIdx, int ParserIdx, int N, bool HasAttr, int ... Is>
    struct gen_attribute_seq_detail : gen_attribute_seq< Context, Keywords, AttrIdx+1, ParserIdx +1, N-1, Is ..., AttrIdx> {};

    template <typename Context, typename Keywords, int AttrIdx, int ParserIdx, int N, int ... Is>
    struct gen_attribute_seq_detail<Context,Keywords,AttrIdx, ParserIdx, N, false, Is...> : gen_attribute_seq< Context, Keywords, AttrIdx, ParserIdx +1, N-1, Is..., -1 > {};
    
    template <typename Context, typename Keywords, int AttrIdx,int ParserIdx, int N, int ... Is>
    struct gen_attribute_seq : 
            gen_attribute_seq_detail<Context, Keywords, AttrIdx, ParserIdx, N, traits::has_attribute< typename std::tuple_element<ParserIdx, Keywords>::type, Context >::value, Is...> {};

    template <typename Context, typename Keywords, int AttrIdx,int ParserIdx, int ... Is>
    struct gen_attribute_seq<Context, Keywords, AttrIdx, ParserIdx, 0, Is...> : attribute_index<Is...> {};

    template <int N, int... Is>
    struct gen_unused_attribute_seq : gen_unused_attribute_seq<N - 1, - 1, Is...> {};

    template <int... Is>
    struct gen_unused_attribute_seq<0, Is...> : attribute_index<Is...> {};

    template <typename Attribute, int Index>
    struct get_attribute
    {
      typedef typename fusion::result_of::at_c<Attribute,Index>::type type;
      static typename add_reference<type>::type call(Attribute &s)
      {
        return fusion::at_c<Index>(s);
      }

    };
    
    template <typename Attribute>
    struct get_attribute<Attribute, -1 >
   {
     static unused_type& call(Attribute &s)
     {
       static unused_type result;
       return result;
     }
   };

}}}}
        

namespace boost { namespace spirit { namespace x3 {

     template <typename Key, typename Subject>
     struct keyword_parser : unary_parser<Subject, keyword_parser<Key,Subject> >
     {
       typedef unary_parser<Subject, keyword_parser<Key,Subject> > base_type;
      
       static const bool is_pass_through_unary =true;

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

       template <int ParserIdx, typename GetAttribute, typename Iterator, typename Context, typename Attribute>
               bool parse_subject(Iterator& first, Iterator const& last
                  , Context const& context, Attribute& attr) const
               {
                       return std::get<ParserIdx>(keywords).subject.parse(first,last,context,GetAttribute::call(attr));
               }
 
       template <typename Iterator, typename Context, typename Attribute, int... Is, int... As>
       bool parse_internal(Iterator& first, Iterator const& last
                  , Context const& context, Attribute& attr, detail::parser_index<Is...>, detail::attribute_index<As...> ) const
       {
               typedef bool (keywords_directive::* subject_caller) (Iterator &, Iterator const&, Context const&, Attribute &) const;
               static subject_caller parse_functions[nb_keywords] =
               {
                &keywords_directive::parse_subject<Is,detail::get_attribute<Attribute, As>,Iterator,Context,Attribute>...
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
               return parse_internal(
                               first,last,context,attr,
                               detail::gen_parser_seq<nb_keywords>{},
                                typename std::conditional< 
                                        std::is_same< typename std::remove_const<Attribute>::type, unused_type >::value, 
                                       detail::gen_unused_attribute_seq<nb_keywords>,
                                       detail::gen_attribute_seq<Context,std::tuple<Keywords...>,0,0,nb_keywords> 
                                       >::type{}
                               );
       }

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
