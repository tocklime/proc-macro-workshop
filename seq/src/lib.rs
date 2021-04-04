use std::ops::Range;

use if_chain::if_chain;
use proc_macro2::{Delimiter, Group, Literal, TokenStream, TokenTree};
use syn::{
    braced, parse::Parse, parse_macro_input, token::Brace, Ident, LitInt, RangeLimits, Token,
};

struct SeqInput {
    id: Ident,
    _in_token: Token![in],
    from: i64,
    dots_token: RangeLimits,
    to: i64,
    _braces: Brace,
    body: TokenStream,
}

impl Parse for SeqInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            id: input.parse()?,
            _in_token: input.parse()?,
            from: input.parse::<LitInt>()?.base10_parse()?,
            dots_token: input.parse()?,
            to: input.parse::<LitInt>()?.base10_parse()?,
            _braces: braced!(content in input),
            body: content.parse()?,
        })
    }
}

impl SeqInput {
    fn range(&self) -> Range<i64> {
        match self.dots_token {
            RangeLimits::HalfOpen(_) => self.from..self.to,
            RangeLimits::Closed(_) => self.from..(self.to + 1),
        }
    }
    fn match_repeated_block(&self, trees: &[TokenTree]) -> Option<TokenStream> {
        if_chain! {
            if let TokenTree::Punct(hash) = &trees[0];
            if hash.as_char() == '#';
            if let TokenTree::Punct(star) = &trees[2];
            if star.as_char() == '*';
            if let TokenTree::Group(g) = &trees[1];
            if g.delimiter() == Delimiter::Parenthesis;
            then {
                Some(g.stream())
            } else {
                None
            }
        }
    }
    fn match_single(&self, tree: &TokenTree) -> bool {
        if_chain! {
            if let TokenTree::Ident(i) = tree;
            if i == &self.id;
            then { true } else { false }
        }
    }
    fn match_triple<'a>(&self, trees: &'a [TokenTree]) -> Option<&'a Ident> {
        if_chain! {
            if let TokenTree::Ident(i) = &trees[0];
            if let TokenTree::Punct(hash) = &trees[1];
            if hash.as_char() == '#';
            if let TokenTree::Ident(n) = &trees[2];
            if n == &self.id;
            then { Some(i) } else { None }
        }
    }
    fn do_substitutions(&self, val: i64, body: TokenStream) -> TokenStream {
        let mut tokens: Vec<TokenTree> = body.into_iter().collect();
        let mut pos = 0;
        while pos < tokens.len() {
            if let TokenTree::Group(g) = &mut tokens[pos] {
                //found a group, recurse.
                let new_body = self.do_substitutions(val, g.stream());
                *g = Group::new(g.delimiter(), new_body);
            } else if self.match_single(&tokens[pos]) {
                //found an instance of the thing we're replacing. Replace it.
                tokens[pos] = TokenTree::Literal(Literal::i64_unsuffixed(val));
            } else if pos + 3 <= tokens.len() {
                if let Some(i) = self.match_triple(&tokens[pos..pos + 3]) {
                    //found an Ident-hash-N triple. replace it.
                    let new_ident = Ident::new(&format!("{}{}", i.to_string(), val), i.span());
                    tokens.splice(pos..pos + 3, std::iter::once(TokenTree::Ident(new_ident)));
                }
            }
            pos += 1;
        }
        tokens.into_iter().collect()
    }
    fn expand_repititions(&self, body: TokenStream, found: &mut bool) -> TokenStream {
        let mut tokens: Vec<TokenTree> = body.into_iter().collect();
        let mut pos = 0;
        while pos < tokens.len() {
            if let TokenTree::Group(g) = &mut tokens[pos] {
                //need to recurse into this group. let span = g.span(); let new_body = self.expand_repititions(g.stream(), found);
                let new_body = self.expand_repititions(g.stream(), found);
                *g = Group::new(g.delimiter(), new_body);
                pos += 1;
                continue;
            }
            if pos + 3 > tokens.len() {
                //not room for a repeated block.
                pos += 1;
                continue;
            }
            if let Some(to_repeat) = self.match_repeated_block(&tokens[pos..pos + 3]) {
                *found = true;
                let reps = self
                    .iterate_stream(&to_repeat)
                    .into_iter()
                    .collect::<Vec<_>>();
                let reps_len = reps.len();
                tokens.splice(pos..pos + 3, reps);
                pos += reps_len;
            } else {
                pos += 1;
            }
        }
        tokens.into_iter().collect()
    }
    fn iterate_stream(&self, s: &TokenStream) -> TokenStream {
        self.range()
            .map(|v| self.do_substitutions(v, s.clone()))
            .collect()
    }

    fn do_macro(&self) -> TokenStream {
        let mut found_repetitions = false;
        let s = self.expand_repititions(self.body.clone(), &mut found_repetitions);
        if found_repetitions {
            //great. subs already done.
            s
        } else {
            self.iterate_stream(&self.body)
        }
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    (parse_macro_input!(input as SeqInput)).do_macro().into()
}
