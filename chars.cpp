#define character_non_whitespace 0b1
#define character_bracket        0b10
#define character_sym_start      0b100
#define character_sym_body       0b1000
#define character_digit          0b10000

#define WS 0
#define CH character_non_whitespace
#define BR character_bracket
#define SS character_sym_start 
#define SB character_sym_body
#define DG character_digit

char character_table[] = {
 /*   0  NUL (null)                   */ WS,
 /*   1  SOH (start of heading)       */ WS,
 /*   2  STX (start of text)          */ WS,
 /*   3  ETX (end of text)            */ WS,
 /*   4  EOT (end of transmission)    */ WS,
 /*   5  ENQ (enquiry)                */ WS,
 /*   6  ACK (acknowledge)            */ WS,
 /*   7  BEL (bell)                   */ WS,
 /*   8  BS  (backspace)              */ WS,
 /*   9  TAB (horizontal tab)         */ WS,
 /*  10  LF  (NL line feed, new line) */ WS,
 /*  11  VT  (vertical tab)           */ WS,
 /*  12  FF  (NP form feed, new page) */ WS,
 /*  13  CR  (carriage return)        */ WS,
 /*  14  SO  (shift out)              */ WS,
 /*  15  SI  (shift in)               */ WS,
 /*  16  DLE (data link escape)       */ WS,
 /*  17  DC1 (device control 1)       */ WS,
 /*  18  DC2 (device control 2)       */ WS,
 /*  19  DC3 (device control 3)       */ WS,
 /*  20  DC4 (device control 4)       */ WS,
 /*  21  NAK (negative acknowledge)   */ WS,
 /*  22  SYN (synchronous idle)       */ WS,
 /*  23  ETB (end of trans. block)    */ WS,
 /*  24  CAN (cancel)                 */ WS,
 /*  25  EM  (end of medium)          */ WS,
 /*  26  SUB (substitute)             */ WS,
 /*  27  ESC (escape)                 */ WS,
 /*  28  FS  (file separator)         */ WS,
 /*  29  GS  (group separator)        */ WS,
 /*  30  RS  (record separator)       */ WS,
 /*  31  US  (unit separator)         */ WS,
 /*  32  SPACE                        */ WS,
 /*  33  !                            */ CH | SS,
 /*  34  "                            */ CH,
 /*  35  #                            */ CH | SS,
 /*  36  $                            */ CH | SS,
 /*  37  %                            */ CH | SS,
 /*  38  &                            */ CH | SS,
 /*  39  '                            */ CH,
 /*  40  (                            */ CH | BR,
 /*  41  )                            */ CH | BR,
 /*  42  *                            */ CH | SS,
 /*  43  +                            */ CH | SS,
 /*  44  ,                            */ CH,
 /*  45  -                            */ CH | SS,
 /*  46  .                            */ CH | SB,
 /*  47  /                            */ CH | SS,
 /*  48  0                            */ CH | DG,
 /*  49  1                            */ CH | DG,
 /*  50  2                            */ CH | DG,
 /*  51  3                            */ CH | DG,
 /*  52  4                            */ CH | DG,
 /*  53  5                            */ CH | DG,
 /*  54  6                            */ CH | DG,
 /*  55  7                            */ CH | DG,
 /*  56  8                            */ CH | DG,
 /*  57  9                            */ CH | DG,
 /*  58  :                            */ CH | SS,
 /*  59  ;                            */ CH,
 /*  60  <                            */ CH | SS,
 /*  61  =                            */ CH | SS,
 /*  62  >                            */ CH | SS,
 /*  63  ?                            */ CH | SS,
 /*  64  @                            */ CH | SS,
 /*  65  A                            */ CH | SS,
 /*  66  B                            */ CH | SS,
 /*  67  C                            */ CH | SS,
 /*  68  D                            */ CH | SS,
 /*  69  E                            */ CH | SS,
 /*  70  F                            */ CH | SS,
 /*  71  G                            */ CH | SS,
 /*  72  H                            */ CH | SS,
 /*  73  I                            */ CH | SS,
 /*  74  J                            */ CH | SS,
 /*  75  K                            */ CH | SS,
 /*  76  L                            */ CH | SS,
 /*  77  M                            */ CH | SS,
 /*  78  N                            */ CH | SS,
 /*  79  O                            */ CH | SS,
 /*  80  P                            */ CH | SS,
 /*  81  Q                            */ CH | SS,
 /*  82  R                            */ CH | SS,
 /*  83  S                            */ CH | SS,
 /*  84  T                            */ CH | SS,
 /*  85  U                            */ CH | SS,
 /*  86  V                            */ CH | SS,
 /*  87  W                            */ CH | SS,
 /*  88  X                            */ CH | SS,
 /*  89  Y                            */ CH | SS,
 /*  90  Z                            */ CH | SS,
 /*  91  [                            */ CH | BR,
 /*  92  \                            */ CH, // TODO: handle backslash
 /*  93  ]                            */ CH | BR,
 /*  94  ^                            */ CH | SS,
 /*  95  _                            */ CH | SS,
 /*  96  `                            */ CH | SS,
 /*  97  a                            */ CH | SS,
 /*  98  b                            */ CH | SS,
 /*  99  c                            */ CH | SS,
 /* 100  d                            */ CH | SS,
 /* 101  e                            */ CH | SS,
 /* 102  f                            */ CH | SS,
 /* 103  g                            */ CH | SS,
 /* 104  h                            */ CH | SS,
 /* 105  i                            */ CH | SS,
 /* 106  j                            */ CH | SS,
 /* 107  k                            */ CH | SS,
 /* 108  l                            */ CH | SS,
 /* 109  m                            */ CH | SS,
 /* 110  n                            */ CH | SS,
 /* 111  o                            */ CH | SS,
 /* 112  p                            */ CH | SS,
 /* 113  q                            */ CH | SS,
 /* 114  r                            */ CH | SS,
 /* 115  s                            */ CH | SS,
 /* 116  t                            */ CH | SS,
 /* 117  u                            */ CH | SS,
 /* 118  v                            */ CH | SS,
 /* 119  w                            */ CH | SS,
 /* 120  x                            */ CH | SS,
 /* 121  y                            */ CH | SS,
 /* 122  z                            */ CH | SS,
 /* 123  {                            */ CH | BR,
 /* 124  |                            */ CH,
 /* 125  }                            */ CH | BR,
 /* 126  ~                            */ CH | SS,
 /* 127  DEL                          */ WS
};

#undef WS
#undef CH
#undef BR
#undef SS
#undef SB
#undef DG