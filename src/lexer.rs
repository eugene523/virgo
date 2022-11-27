use std::fmt;
use std::collections::HashMap;

#[derive(Debug, Default, Copy, Clone)]
enum TokenType {

    #[default]
    Undefined,

    // Different kinds of brackets
    LParenthesis,   // (
    RParenthesis,   // )
    LBracket,       // [
    RBracket,       // ]

    // Single character tokens
    Comma,          // ,
    Dot,            // .
    Colon,          // :
    Semicolon,      // ;
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Caret,          // ^

    // One or two character tokens
    Eq,             // =
    NotEq,          // !=
    Greater,        // >
    GreaterEq,      // >=
    Less,           // <
    LessEq,         // <=

    AddAssign,      // +=
    SubAssign,      // -=
    MulAssign,      // *=
    DivAssign,      // /=
    PowAssign,      // ^=

    // Literals
    LInt,
    LReal,          // 5.1, 0.314e+1
    LStr,           // "It's Britney, bitch!"
    LId,            // SomeVariableName

    // Keywords
    None,
    True,
    False,
    And,
    Or,
    Not,
    If,
    Else,
    For,
    In,
    Jump,
    Quit,
    Skip,
    Return,
    Assert,

    // Scope tokens
    EnterScope,
    ExitScope,

    // End of file token
    EndOfFile
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

type Sid = u64;

#[derive(Debug, Default)]
struct Token {
    token_type: TokenType,
    lexeme: String,
    src_line: usize,
    src_col: usize,
    c_int: i64,
    c_real: f64,
    sid: Sid,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, src_line: usize, src_col: usize) -> Token {
        let mut t: Token = Default::default();
        t.token_type = token_type;
        t.lexeme = lexeme.to_string();
        t.src_line = src_line;
        t.src_col = src_col;
        return t;
    }

    pub fn new_no_lexeme(token_type: TokenType, src_line: usize, src_col: usize) -> Token {
        let mut t: Token = Default::default();
        t.token_type = token_type;
        t.src_line = src_line;
        t.src_col = src_col;
        return t;
    }

    pub fn new_literal_int(lexeme: String, src_line: usize, src_col: usize, c_int: i64) -> Token {
        let mut t = Token::new(TokenType::LInt, lexeme, src_line, src_col);
        t.c_int = c_int;
        return t;
    }

    pub fn new_literal_real(lexeme: String, src_line: usize, src_col: usize, c_real: f64) -> Token {
        let mut t = Token::new(TokenType::LReal, lexeme, src_line, src_col);
        t.c_real = c_real;
        return t;
    }

    pub fn new_literal_str(lexeme: String, src_line: usize, src_col: usize, sid: Sid) -> Token {
        let mut t = Token::new(TokenType::LStr, lexeme, src_line, src_col);
        t.sid = sid;
        return t;
    }

    pub fn new_literal_id(lexeme: String, src_line: usize, src_col: usize, sid: Sid) -> Token {
        let mut t = Token::new(TokenType::LId, lexeme, src_line, src_col);
        t.sid = sid;
        return t;
    }
}

struct StrPool {
    next_sid: Sid,
    sid_to_str: Vec<String>,
    str_to_sid: HashMap<String, Sid>,
}

impl StrPool {
    pub fn new() -> StrPool {
        StrPool {
            next_sid: 0,
            sid_to_str: Vec::new(),
            str_to_sid: HashMap::new()
        }
    }

    pub fn intern(&mut self, s: &str) -> Sid {
        let attempt = self.str_to_sid.get(s);
        match attempt {
            Some(sid) => *sid,
            None => {
                self.sid_to_str.push(s.to_string());
                let sid = self.next_sid;
                self.next_sid += 1;
                self.str_to_sid.insert(s.to_string(), sid);
                return sid;
            }
        }        
    }

    pub fn _get(&self, sid: Sid) -> String {
        let s = &self.sid_to_str[sid as usize];
        s.clone()
    }
}

const N_WHITESPACES_PER_LEVEL: usize = 4;

pub struct Lexer {
    keywords: HashMap<String, TokenType>,
    strpool: StrPool,
    src_chars: Vec<char>,
    src_line: usize,
    src_col: usize,
    start_pos: usize,
    pos: usize,
    nest_lvl: usize,
    n_opened: usize,
    tokens: Vec<Token>,
    has_err: bool,
}

impl Lexer {
    pub fn new() -> Lexer {
        let mut lexer = Lexer {
            keywords: HashMap::new(),
            strpool: StrPool::new(),
            src_chars: Vec::new(),
            src_line: 0,
            src_col: 0,
            start_pos: 0,
            pos: 0,
            nest_lvl: 0,
            n_opened: 0,
            tokens: Vec::new(),
            has_err: false,
        };
        
        lexer.keywords.insert("none".to_string(), TokenType::None);
        lexer.keywords.insert("true".to_string(), TokenType::True);
        lexer.keywords.insert("false".to_string(), TokenType::False);
        lexer.keywords.insert("and".to_string(), TokenType::And);
        lexer.keywords.insert("or".to_string(), TokenType::Or);
        lexer.keywords.insert("not".to_string(), TokenType::Not);
        lexer.keywords.insert("if".to_string(), TokenType::If);
        lexer.keywords.insert("else".to_string(), TokenType::Else);
        lexer.keywords.insert("for".to_string(), TokenType::For);
        lexer.keywords.insert("in".to_string(), TokenType::In);
        lexer.keywords.insert("jump".to_string(), TokenType::Jump);
        lexer.keywords.insert("quit".to_string(), TokenType::Quit);
        lexer.keywords.insert("skip".to_string(), TokenType::Skip);
        lexer.keywords.insert("return".to_string(), TokenType::Return);
        lexer.keywords.insert("assert".to_string(), TokenType::Assert);

        return lexer;
    }

    pub fn tokenize(&mut self, src: &str) {
        self.src_chars = src.chars().collect();
        self.scan_tokens();
    }

    fn scan_tokens(&mut self) {
        self.process_new_line();
        while !self.is_at_end() {
            if self.has_err { return; }
            self.start_pos = self.pos;
            self.scan_token();
        }
        self.process_end_of_file();
    }

    fn scan_token(&mut self) {
        let saved_src_line = self.src_line;
        let saved_src_col = self.src_col;
        let c = self.advance();
        match c {
            '(' => {
                self.n_opened += 1;
                self.add_token(TokenType::LParenthesis, saved_src_line, saved_src_col);
            },
            ')' => {
                self.n_opened -= 1;
                self.add_token(TokenType::RParenthesis, saved_src_line, saved_src_col);
            },
            '[' => {
                self.n_opened += 1;
                self.add_token(TokenType::LBracket, saved_src_line, saved_src_col);
            },
            ']' => {
                self.n_opened -= 1;
                self.add_token(TokenType::RBracket, saved_src_line, saved_src_col);
            },
            ',' => { self.add_token(TokenType::Comma, saved_src_line, saved_src_col); },
            '.' => { self.add_token(TokenType::Dot, saved_src_line, saved_src_col); },
            ':' => { self.add_token(TokenType::Colon, saved_src_line, saved_src_col); },
            ';' => { self.add_token(TokenType::Semicolon, saved_src_line, saved_src_col); },
            '+' => {
                if self.match_char('=') { 
                    self.add_token(TokenType::AddAssign, saved_src_line, saved_src_col); 
                } else { 
                    self.add_token(TokenType::Plus, saved_src_line, saved_src_col); 
                }
            },
            '-' => {
                if self.match_char('=') { 
                    self.add_token(TokenType::SubAssign, saved_src_line, saved_src_col); 
                } else { 
                    self.add_token(TokenType::Minus, saved_src_line, saved_src_col); 
                }
            },
            '*' => {
                if self.match_char('=') { 
                    self.add_token(TokenType::MulAssign, saved_src_line, saved_src_col); 
                } else { 
                    self.add_token(TokenType::Star, saved_src_line, saved_src_col); 
                }
            },
            '/' => {
                if self.match_char('=') { 
                    self.add_token(TokenType::DivAssign, saved_src_line, saved_src_col); 
                } else { 
                    self.add_token(TokenType::Slash, saved_src_line, saved_src_col); 
                }
            },
            '^' => {
                if self.match_char('=') { 
                    self.add_token(TokenType::PowAssign, saved_src_line, saved_src_col); 
                } else { 
                    self.add_token(TokenType::Caret, saved_src_line, saved_src_col); 
                }
            },
            '=' => { self.add_token(TokenType::Eq, saved_src_line, saved_src_col); },
            '!' => { 
                if self.match_char('=') { 
                    self.add_token(TokenType::NotEq, saved_src_line, saved_src_col); 
                } else { 
                    self.report_error("Unexpected character."); 
                }
            },
            '>' => {
                if self.match_char('=') { 
                    self.add_token(TokenType::GreaterEq, saved_src_line, saved_src_col); 
                } else { 
                    self.add_token(TokenType::Greater, saved_src_line, saved_src_col); 
                }
            },
            '<' => {
                if self.match_char('=') { 
                    self.add_token(TokenType::LessEq, saved_src_line, saved_src_col); 
                } else { 
                    self.add_token(TokenType::Less, saved_src_line, saved_src_col); 
                }
            },
            '#'  => { self.process_comment(); },
            '\t' => { self.report_error("Tabs are not allowed."); },
            ' '  => { self.process_whitespace(); },
            '\r' => {},
            '\n' => { self.process_new_line(); },
            '"'  => { self.process_string(); },
            other => {
                if other.is_digit(10) { 
                    self.process_number(); 
                } else if other.is_alphanumeric() { 
                    self.process_word(); 
                } else {
                    let err_msg = format!("Unexpected character: 0x{:08x}", other as u32);
                    self.report_error(&err_msg); 
                }
            }
        }
    }

    fn is_keyword(&self, word: &str) -> bool {
        self.keywords.contains_key(word)
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.src_chars.len()
    }

    fn advance(&mut self) -> char {
        let c = self.src_chars[self.pos];
        self.pos += 1;
        if c == '\n' {
            self.src_col = 0;
            self.src_line += 1;
        } else {
            self.src_col += 1;
        }
        return c;
    }

    fn peek(&self) -> char {
        if self.is_at_end() { return '\0'; }
        self.src_chars[self.pos]
    }

    fn peek_next(&self) -> char {
        if self.pos + 1 >= self.src_chars.len() {
            return '\0';
        }
        return self.src_chars[self.pos + 1];
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() { return false; }
        if self.src_chars[self.pos] != expected { return false; }
        self.pos += 1;
        if expected == '\n' {
            self.src_col = 0;
            self.src_line += 1;
        } else {
            self.src_col += 1;
        }
        return true;
    }

    fn extract_lexeme(&self, begin_index: usize, end_index_exclusive: usize) -> String {
        self.src_chars[begin_index..end_index_exclusive].iter().collect()
    }

    fn process_new_line(&mut self) {
        /*
        If the number of currently opened parentheses, brackets or braces
        is more than zero, that means that expression continues
        and we should skip any whitespaces and return.
        */
        if self.n_opened > 0 {
            while self.match_char(' ') {}
            return;
        }

        let mut n_whitespaces: usize = 0;
        while self.match_char(' ') { 
            n_whitespaces += 1; 
        }

        if self.peek() == '\n' || self.peek() == '\r' || self.is_at_end() { 
            return; 
        }

        if n_whitespaces % N_WHITESPACES_PER_LEVEL != 0 {
            self.report_error("Nesting error.");
            return;
        }

        let curr_nest_lvl = n_whitespaces / N_WHITESPACES_PER_LEVEL;

        if curr_nest_lvl == self.nest_lvl { return; }

        if curr_nest_lvl > self.nest_lvl {
            if curr_nest_lvl - self.nest_lvl >= 2 {
                self.report_error("Nesting error.");
                return;
            }
            self.add_token(TokenType::EnterScope, self.src_line, self.src_col);
        } else {
            let diff_lvl = self.nest_lvl - curr_nest_lvl;
            for _ in 0..diff_lvl {
                self.add_token(TokenType::ExitScope, self.src_line, self.src_col);
            }
        }
        self.nest_lvl = curr_nest_lvl;
    }

    fn process_comment(&mut self) {
        /*
        One line comment starts with one symbol "#" like this:
        # This is a comment.

        Multiline comment starts with two symbols ("##"),
        and ends with the same two symbols ("##") like this:
        ##
        Write less comments.
        Especially multiline comments.
        ##

        It's possible (but not recommended) to have unclosed
        multiline comment if it ends with EOF.
        */

        if self.peek() == '#' {
            // Multiline comment.
            // Notice that we have already processed one '#" character 
            // in the global switch loop.
            self.advance();
            loop {
                if self.is_at_end() { break; }
                if self.peek() == '#' && self.peek_next() == '#' {
                    self.pos += 2;
                    break;
                }
                self.advance();
            }
        } else {
            // One line comment
            while self.peek() != '\n' && !self.is_at_end() { 
                self.advance(); 
            }
        }
    }

    fn process_whitespace(&mut self) {
        while self.match_char(' ') ||
              self.match_char('\r') {}
    }

    fn process_string(&mut self) {
        // We are on the first char right after the first quotation mark:
        // "Hello, World!"
        //  ^---pos
        let saved_src_line = self.src_line;
        let saved_src_col = self.src_col - 1; // Stepping back by one char.

        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
        }

        if self.is_at_end() { 
            self.report_error("Unterminated string.");
            return;
        }
  
        // "Hello, World!"
        //               ^---pos
        let lexeme: String = self.extract_lexeme(self.start_pos, self.pos + 1);
        let lexeme_sid = self.strpool.intern(&lexeme);
        let token = Token::new_literal_str(lexeme, saved_src_line, saved_src_col, lexeme_sid);
        self.tokens.push(token);
        self.advance(); // Moving to the next char right after the last quotation mark.

    }

    fn process_number(&mut self) {
        let saved_src_line = self.src_line;
        let saved_src_col = self.src_col - 1; // Stepping back by one char.

        let mut is_int = true;
        while self.peek().is_digit(10) { self.advance(); }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            is_int = false;
            self.advance();
            while self.peek().is_digit(10) { self.advance(); }
        }

        if self.peek() == 'e' || self.peek() == 'E' {
            is_int = false;
            self.advance();
            if self.peek().is_digit(10) {
                while self.peek().is_digit(10) { self.advance(); }
            } else if (self.peek() == '+' || self.peek() == '-') && self.peek_next().is_digit(10) {
                self.advance();
                while self.peek().is_digit(10) { self.advance(); }
            } else {
                self.report_error("Wrong exponential notation of a number.");
                return;
            }
        }

        let lexeme = self.extract_lexeme(self.start_pos, self.pos);
        let token = if is_int {
            let parsed_num: i64 = lexeme.parse().unwrap();
            Token::new_literal_int(lexeme, saved_src_line, saved_src_col, parsed_num)
        } else {
            let parsed_num: f64 = lexeme.parse().unwrap();
            Token::new_literal_real(lexeme, saved_src_line, saved_src_col, parsed_num)
        };
        self.tokens.push(token);
    }

    fn process_word(&mut self) {
        let saved_src_line = self.src_line;
        let saved_src_col = self.src_col - 1; // Stepping back by one char.

        while self.peek().is_alphanumeric() || self.peek() == '_' { 
            self.advance(); 
        }

        let lexeme = self.extract_lexeme(self.start_pos, self.pos);
        if self.is_keyword(&lexeme) {
            let token_type = *self.keywords.get(&lexeme).unwrap();
            self.add_token(token_type, saved_src_line, saved_src_col);
            return;
        }

        let lexeme_sid = self.strpool.intern(&lexeme);
        let token = Token::new_literal_id(lexeme, saved_src_line, saved_src_col, lexeme_sid);
        self.tokens.push(token);
    }

    fn process_end_of_file(&mut self) {
        /*
        It is possible that code file ends without exiting
        from nesting scopes ('if', 'for', declarations, etc).
        To ensure that all scopes are closed we add
        additional ExitScope tokens according to the current nesting level.
        */
        for _ in 0..self.nest_lvl {
            self.add_token(TokenType::ExitScope, self.src_line, self.src_col);
        }
        self.add_token(TokenType::EndOfFile, self.src_line, self.src_col);
    }

    fn add_token(&mut self, token_type: TokenType, src_line: usize, src_col: usize) {
        let t = Token::new_no_lexeme(token_type, src_line, src_col);
        self.tokens.push(t);
    }

    fn report_error(&mut self, err_msg: &str) {
        print!("Line {}:{}. {}", self.src_line, self.pos, err_msg);
        self.has_err = true;
    }

    pub fn print_tokens(&self) {
        let (w1,w2, w3, w4) = (4, 4, 16, 16);
        println!("\n{:w1$}{:w2$}{:w3$}{:w4$}", "Ln", "Col", "Token", "Lexeme");
        for t in &self.tokens {
            let name = format!("{}", t.token_type);
            println!("{:<w1$}{:<w2$}{:<w3$}{:<w4$}", t.src_line + 1, t.src_col + 1, name, t.lexeme);
        }
    }
}