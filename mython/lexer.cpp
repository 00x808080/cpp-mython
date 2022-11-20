#include "lexer.h"

#include <algorithm>
#include <charconv>
#include <unordered_map>

using namespace std;

namespace parse {

bool operator==(const Token& lhs, const Token& rhs) {
    using namespace token_type;

    if (lhs.index() != rhs.index()) {
        return false;
    }
    if (lhs.Is<Char>()) {
        return lhs.As<Char>().value == rhs.As<Char>().value;
    }
    if (lhs.Is<Number>()) {
        return lhs.As<Number>().value == rhs.As<Number>().value;
    }
    if (lhs.Is<String>()) {
        return lhs.As<String>().value == rhs.As<String>().value;
    }
    if (lhs.Is<Id>()) {
        return lhs.As<Id>().value == rhs.As<Id>().value;
    }
    return true;
}

bool operator!=(const Token& lhs, const Token& rhs) {
    return !(lhs == rhs);
}

std::ostream& operator<<(std::ostream& os, const Token& rhs) {
    using namespace token_type;

#define VALUED_OUTPUT(type) \
    if (auto p = rhs.TryAs<type>()) return os << #type << '{' << p->value << '}';

    VALUED_OUTPUT(Number);
    VALUED_OUTPUT(Id);
    VALUED_OUTPUT(String);
    VALUED_OUTPUT(Char);

#undef VALUED_OUTPUT

#define UNVALUED_OUTPUT(type) \
    if (rhs.Is<type>()) return os << #type;

    UNVALUED_OUTPUT(Class);
    UNVALUED_OUTPUT(Return);
    UNVALUED_OUTPUT(If);
    UNVALUED_OUTPUT(Else);
    UNVALUED_OUTPUT(Def);
    UNVALUED_OUTPUT(Newline);
    UNVALUED_OUTPUT(Print);
    UNVALUED_OUTPUT(Indent);
    UNVALUED_OUTPUT(Dedent);
    UNVALUED_OUTPUT(And);
    UNVALUED_OUTPUT(Or);
    UNVALUED_OUTPUT(Not);
    UNVALUED_OUTPUT(Eq);
    UNVALUED_OUTPUT(NotEq);
    UNVALUED_OUTPUT(LessOrEq);
    UNVALUED_OUTPUT(GreaterOrEq);
    UNVALUED_OUTPUT(None);
    UNVALUED_OUTPUT(True);
    UNVALUED_OUTPUT(False);
    UNVALUED_OUTPUT(Eof);

#undef UNVALUED_OUTPUT

    return os << "Unknown token :("sv;
}

Lexer::Lexer(std::istream& input)
        : input_(input)
{
    // Skip comments at the beginning
    while (input_.peek() == '#') {
        SkipSpaces();
        string str;
        getline(input_, str);
    }

    current_token_ = token_type::Newline{};
    current_token_ = NextToken();
}

const Token& Lexer::CurrentToken() const {
    return current_token_;
}

Token Lexer::NextToken() {
    if (current_token_ == token_type::Eof{}) {
        return token_type::Eof{};
    }
    if (current_token_ == token_type::Newline{}) {
        SkipEmptyLinesAndComments();
    }
    if (difference_previous_current_indents_ != 0) {
        return current_token_ = ParseIndentOrDedent();
    }

    SkipSpaces();
    char c = input_.get();
    if (isdigit(c)) {
        input_.putback(c);
        return current_token_ = ParseNumber();
    }
    else if (c == '\'' || c == '\"') {
        input_.putback(c);
        return current_token_ = ParseStringConstant();;
    }
    else if (c == '\n') {
        return current_token_ = token_type::Newline{};
    }
    else if (c == EOF) {
        if (current_token_ == token_type::Newline{} ||
            current_token_ == token_type::Indent{} ||
            current_token_ == token_type::Dedent{}) {

            return current_token_ = token_type::Eof{};
        }
        return current_token_ = token_type::Newline{};
    }
    else if (c == '#') {
        string tmp;
        getline(input_, tmp);
        SkipEmptyLinesAndComments();
        char c = input_.peek();
        if (c == '#') {
            return current_token_ = NextToken();
        }
        else if (c == EOF) {
            return current_token_ = token_type::Eof{};
        }
        return current_token_ = token_type::Newline{};
    }
    else if (c == '=' && input_.peek() != '=') {
        return current_token_ = token_type::Char{ '=' };
    }
    else if (comparison_symbols.count(c) && input_.peek() == '=') {
        input_.putback(c);
        return current_token_ = ParseComparisonOperator();
    }
    else if (special_symbols.count(c)) {
        return current_token_ = token_type::Char{ c };
    }
    else if (isalpha(c) || c == '_') {
        input_.putback(c);
        return current_token_ = ParseName();
    }
    else {
        throw LexerError("parsing error");
    }
}

// ==================================================== private ========================================================

void Lexer::SkipSpaces() {
    while (input_.peek() == ' ') {
        input_.ignore(1);
    }
}

void Lexer::SkipEmptyLinesAndComments() {
    size_t count_spaces = 0u;
    while (input_.peek() == ' ') {
        input_.ignore(1);
        ++count_spaces;
    }

    if (input_.peek() == '\n') {
        input_.ignore(1);
        SkipEmptyLinesAndComments();
        return;
    }
    else if (input_.peek() == '#') {
        string str;
        getline(input_, str);
        SkipEmptyLinesAndComments();
        return;
    }
    else if (count_spaces % 2 == 1) {
        throw LexerError("invalid indent");
    }

    difference_previous_current_indents_ = count_spaces / 2 - current_indent_;
    current_indent_ = count_spaces / 2;
}

Token Lexer::ParseNumber() {
    char c = input_.get();
    if (!isdigit(c)) {
        throw LexerError("expected number"s);
    }

    string number;
    while (isdigit(c)) {
        number += c;
        c = input_.get();
    }
    if (c != ' ' && c != '\n' && c != EOF && special_symbols.count(c) == 0) {
        throw LexerError("expected space/new line/end/file/special symbol"s);
    }
    input_.putback(c);
    return token_type::Number{stoi(number)};
}

Token Lexer::ParseStringConstant() {
    const char initial_quote = input_.get();
    if (initial_quote != '\'' && initial_quote != '\"') {
        throw LexerError("expected opening quote"s);
    }

    string str;
    while (true) {
        if (input_.peek() == EOF) {
            throw LexerError("expected closing quote");
        }

        char c = input_.get();
        if (c != initial_quote && c != '\\') {
            str += c;
        }
        else if (c == '\\') {
            c = input_.get();
            if (c == 't') str += '\t';
            else if (c == 'n') str += '\n';
            else str += c;
        }
        else break;
    }
    return token_type::String{str};
}

Token Lexer::ParseName() {
    char c = input_.get();
    if (!isalpha(c) && c != '_') {
        throw LexerError("invalid symbol/id");
    }

    string name = {c};
    c = input_.get();
    while (c == '_' || isdigit(c) || isalpha(c)) {
        name += c;
        c = input_.get();
    }

    input_.putback(c);
    if (token_type::keyword_to_token.count(name)) {
        return token_type::keyword_to_token.at(name);
    }
    return token_type::Id{name};
}

Token Lexer::ParseComparisonOperator() {
    string operation = {char(input_.get()), char(input_.get())};
    if (operation == "==") {
        return token_type::Eq{};
    }
    else if (operation == "!=") {
        return token_type::NotEq{};
    }
    else if (operation == "<=") {
        return token_type::LessOrEq{};
    }
    else if (operation == ">=") {
        return token_type::GreaterOrEq{};
    }
    else {
        throw LexerError("expected comparison operation"s);
    }
}

Token Lexer::ParseIndentOrDedent() {
    if (difference_previous_current_indents_ > 0) {
        --difference_previous_current_indents_;
        return token_type::Indent{};
    }
    else if (difference_previous_current_indents_ < 0) {
        ++difference_previous_current_indents_;
        return token_type::Dedent{};
    }
    throw LexerError("no indent/dedent");
}

}  // namespace parse