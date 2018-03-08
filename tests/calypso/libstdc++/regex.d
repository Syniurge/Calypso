// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

/**
 * std::regex example.
 */

module _regex_;

pragma (cppmap, "<regex>");
pragma (cppmap, "<string>");

import std.stdio, std.conv;
import (C++) std.regex, std.smatch;
import (C++) regex_constants = std.regex_constants._;
import (C++) std._ : cppstring = string, regex_search,
                sregex_iterator, distance, regex_replace;

void main()
{
    cppstring s = "Some people, when confronted with a problem, think "
        ~ "\"I know, I'll use regular expressions.\" "
        ~ "Now they have two problems.";

    auto self_regex = regex("REGULAR EXPRESSIONS",
            regex_constants.ECMAScript | regex_constants.icase);
    if (regex_search(s, self_regex))
        writeln("Text contains the phrase 'regular expressions'");

    auto word_regex = regex("(\\S+)");
    auto words_begin = sregex_iterator(s.begin, s.end, word_regex);
    auto words_end = sregex_iterator();

    writeln("Found ", distance(words_begin, words_end), " words");

    enum N = 6;
    writeln("Words longer than ", N, " characters:");
    for (auto i = words_begin; i != words_end; ++i) {
        smatch match = *i;
        cppstring match_str = match.str;
        if (match_str.size > N)
            writeln("  ", match_str.c_str.to!string);
    }

    auto long_word_regex = regex("(\\w{7,})");
    cppstring new_s = regex_replace(s, long_word_regex, "[$&]");
    writeln(new_s.c_str.to!string);
}
