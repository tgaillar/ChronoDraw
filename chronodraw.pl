#!/bin/sh
#! -*-perl-*-
# line 4
#
# Make this script be run by /bin/sh, which in
# turn will run Perl on it... (see man perlrun(1))
# Fail otherwise!
#
eval 'exec perl -x $0 ${1+"$@"}; echo "ERROR, failed to launch PERL script $0" 1>&2; exit 2'
  if 0;

#
# Copyright 2010-2018 Thibaud GAILLARD (thibaud dot gaillard at gmail dot com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#
# Use any script-attached or local SVG.pm, if any. Rely otherwise on the PERL installation...
# (SVG.pm not installed as a default)
#
BEGIN {
  my ($spath, $sfile) = ($0 =~ /^(.*)\/([^\/]*)$/);
#  printf ("#DEBUG# path   = %s\n#DEBUG# script = %s\n", $spath, $sfile);
  if ((-r "$spath/SVG.pm") && (-d "$spath/SVG")) {
    push @INC , "$spath/";
    push @INC , "$spath/SVG";
  }
  elsif ((-r "$spath/SVG/SVG.pm") && (-d "$spath/SVG/SVG")) {
    push @INC , "$spath/SVG/";
    push @INC , "$spath/SVG/SVG";
  }
  elsif ((-r "$spath/SVG/lib/SVG.pm") && (-d "$spath/SVG/lib/SVG")) {
    push @INC , "$spath/SVG/lib";
    push @INC , "$spath/SVG/lib/SVG";
  }
  elsif ((-r "./SVG.pm") && (-d "./SVG")) {
    push @INC , "./";
    push @INC , "./SVG";
  }
  elsif ((-r "./SVG/SVG.pm") && (-d "./SVG/SVG")) {
    push @INC , "./SVG/";
    push @INC , "./SVG/SVG";
  }
  elsif ((-r "./SVG/lib/SVG.pm") && (-d "./SVG/lib/SVG")) {
    push @INC , "./SVG/lib";
    push @INC , "./SVG/lib/SVG";
  }
}

# Be strong about PERL syntax
use strict;
##use warnings;

# Let us parse options using perl library
use Getopt::Long;

# Get support from Struct Class
use Class::Struct;

# Get support from SVG
use SVG (indent => "  ");


# Get support from "switch"-like directive...
use feature "switch";
no if ($] >= 5.018), 'warnings' => 'experimental';
# ...without the warnings that emerged in 5.18 onwards
##use experimental qw(smartmatch);

# Global variables
my $warning = 0;
my $error   = 0;

my ($DEBUG, $HELP);

# Global settings
my $DSVG;	   # visually debug generated SVG

my $DPI    =  72;  # dot per inch (inkscape is 90 as per SVG standard!)
my $SCALE  = 100;  # overall scale factor (units)
my $MARGIN =   0;  # overall margin (t/b/l/r, units) / 1cm = 28.35px

my $WIDTH  = undef;# Overall width when defined
my $HEIGHT = undef;# Overall height when defined
my $RATIO  = 1.00; # Overall h/v cycle ratio
my $CYCLE  = undef;# Overall width of a cycle column
my $SIGROW = undef;# Overall height of a signal row

my $SEDGE  = 0.10; # signal rise/fall width ratio over 1 cycle (1.0)
my $FSIZES = 0.30; # font size (signal)
my $FSIZEV = 0.20; # font size (value)
my $FSIZET = 0.20; # font size (timing)
my $SWIDTH = 0.03; # signal stroke width
my $VWIDTH = 0.01; # vertical bar stroke width
my $HWIDTH = $SWIDTH*3/4; # hiatus stroke width
my $SNAMEO = 0.10; # signal name offset
my $SNAMET = 0.05; # signal name tolerance

my $FNAME    = 'Impact';        # font name used for texts
my $FHWRATIO = 2*(1 - 2.5/100); # font vertical/horizontal ratio / char (estimated)
my $FVORATIO = 1/3;             # font vertical offset ratio to reach baseline (estimated)

my $DTL = 0.00; # cycle left  offset
my $DTR = 0.00; # cycle right offset

my $SM = 0.20; # signal vertical margin (top & bottom, %)
my $TM = 0.10; # timing vertical margin (top & bottom, %)
my $HM = 0.10; # hiatus vertical margin (top & bottom, %)
my $TT = $SEDGE; # Text horizontal tolerance when left&right constrained

my $SIGNALH = 0.7;        # overall height of a single wave row
my $TIMINGH = $SIGNALH/2; # overall height of a single timing row

my $HIATUSW = $SEDGE*1.50;        # hiatus overall width (before blending)
my $HIATUSH = $SIGNALH*(1-2*$HM); # hiatus overall height
my $HIATUSO = $HIATUSW*0.75;      # hiatus text offset from center (before blending)
my @HIATUSB = (1, 2);             # hiatus length blending parameters (ratio, unratio)

my $re_ss_dash = '^[LZHW].';

my $re_color = '^#[0-Aa-fA-F]{6}$';
my $WHITE  = "#FFFFFF"; # Full White
my $GRAY   = "#D3D3D3"; # Light Gray
my $BLACK  = "#000000"; # Full Black
my $RED    = "#FF0000"; # Full Red
my $GREEN  = "#00FF00"; # Full Green
my $GBLUE  = "#00FFFF"; # Full Green/Blue (Turquoise?)

#
# Timing Diagram
#

struct (TIMING => [
  scale    => '$', # real
  margin   => '$', # real
  wcratio  => '$', # real (in %)
  siglmax  => '$', # integer
  sx       => '$', # real
  sy       => '$', # real
  sigedge  => '$', # real (in %)
  cycles   => '$', # integer
  dtl      => '$', # real
  dtr      => '$', # real
  cyclesep => '@', # array of char
  signals  => '@', # array of strings
  signal   => '%', # -> %SIGNAL
]);

struct (SIGNAL => [
  name   => '$', # string
  value  => '$', # string
  time   => '$', # real
  offset => '$', # real (in %)
  pvalue => '$', # string
  clock  => '$', # integer (# phases per cycle if positive)
  duty   => '$', # real (in %)
  noline => '$', # boolean
  logic  => '$', # logic (init)
  wave   => '@', # -> @WAVE
  tagidx => '%', # hash index to @WAVE
  maxlvl => '$', # integer (highest timref extent)
  minlvl => '$', # integer (lowest  timref extent)
  hiatus => '@', # -> @HIATUS
  yh     => '$', # real (vertical elevation high)
  y      => '$', # real (vertical elevation)
  yl     => '$', # real (vertical elevation low)
]);

struct (WAVE => [
  cycle  => '$', # integer
  time   => '$', # real
  jitter => '$', # string
  value  => '$', # char/string
  logic  => '$', # boolean
  align  => '$', # integer (-1/left, 0/center, +1/right)
  timref => '@', # -> @TIMREF
  timtag => '$', # boolean (show a timref tag mark)
]);

struct (TIMREF => [
  sigref  => '$', # string
  evtref  => '$', # integer
  offref  => '$', # integer
  tagref  => '$', # string
  text    => '$', # string
  align   => '$', # integer (-1/left, 0/center, +1/right)
  arrows  => '$', # integer (-1/out, 0/none, +1/in) wrt current signal
  arrowr  => '$', # integer (-1/out, 0/none, +1/in) wrt reference signal
  level   => '$', # integer (-n/below, 0/on, +n/above) at nth level
]);

struct (HIATUS => [
  cycle  => '$',
  time   => '$',
]);

struct (WAVEPT => [
  s    => '$', # char
  v    => '$', # string
  j    => '$', # string
  a    => '$', # (-1, 0, 1)
  #
  plpt => '@', # polyline/polygon point list (x,y)
  xlpt => '@', # polyline/polygon point list (x,y)
  ltpt => '@', # left  text point (x,y)
  rtpt => '@', # right text point (x,y)
]);

# Negated clock hash
my %CLOCKN = (
  '0' => '1',
  '1' => '0',
  'L' => 'H',
  'H' => 'L',
);

# Low clock boolean hash
my %CLOCKL = (
  '0' => 1,
  '1' => 0,
  'L' => 1,
  'H' => 0,
);

# Command-line/User-code accessible variables
my %VARIABLE = (
  dpi	 => \$DPI,	# Dot per inch
  scale	 => \$SCALE,	# Scale
  margin => \$MARGIN,	# Signal name left margin
  ratio  => \$RATIO,	# Cycle / Interline ratio (h/v)
  width  => \$WIDTH,	# Overall diagram width
  height => \$HEIGHT,	# Overall diagram height
  sigrow => \$SIGROW,	# Interline height
  cycle  => \$CYCLE,	# Cycle width

  fszsn  => \$FSIZES,	# Font size for signal names
  fszsv  => \$FSIZEV,	# Font size for signal text value
  fsztr  => \$FSIZET,	# Font size for timing reference text
  dsvg   => \$DSVG,	# Debug SVG (within drawing)
);
my %VARLINESET;

#
# Impact font reverse-engineered (what a kl*dge!)
#
my $CHARFSIZE = 48.86; # Font-size
my $CHARWREF  = 'e';   # Reference char
my %CHARWIDTH = (	# Origin to leftmost point of following 'e'
  'a' =>  26.0523, 'b' =>  26.8157, 'c' =>  25.5752, 'd' =>  26.8157, 'e' =>  26.4340, 'f' =>  15.5550, 'g' =>  26.8157, 'h' =>  27.0066, 'i' =>  14.7916, 'j' =>  15.0779,
  'k' =>  24.8117, 'l' =>  14.7916, 'm' =>  39.1262, 'n' =>  27.0066, 'o' =>  26.4340, 'p' =>  26.8157, 'q' =>  26.7203, 'r' =>  18.8951, 's' =>  24.4300, 't' =>  16.3185,
  'u' =>  27.0066, 'v' =>  22.8077, 'w' =>  34.1638, 'x' =>  22.6168, 'y' =>  23.3803, 'z' =>  18.6088,
  'A' =>  26.2432, 'B' =>  28.4380, 'C' =>  28.5335, 'D' =>  28.4380, 'E' =>  21.7580, 'F' =>  20.8991, 'G' =>  28.3426, 'H' =>  28.5335, 'I' =>  15.5550, 'J' =>  17.6545,
  'K' =>  27.6746, 'L' =>  20.0402, 'M' =>  36.4541, 'N' =>  27.8655, 'O' =>  28.1518, 'P' =>  25.9569, 'Q' =>  28.1518, 'R' =>  27.7700, 'S' =>  26.7203, 'T' =>  21.9488,
  'U' =>  28.1518, 'V' =>  27.0066, 'W' =>  41.2256, 'X' =>  25.0026, 'Y' =>  22.3305, 'Z' =>  20.8037,
  '1' =>  20.0402, '2' =>  25.9569, '3' =>  27.3883, '4' =>  25.8614, '5' =>  27.6746, '6' =>  27.8655, '7' =>  20.6128, '8' =>  27.5792, '9' =>  27.8655, '0' =>  27.6746,
  '!' =>  14.6007, '@' =>  39.3170, '#' =>  32.0644, '$' =>  28.1518, '^' =>  25.0980, '&' =>  29.5832, '%' =>  35.3090, '*' =>  15.1733, '(' =>  16.7956, ')' =>  16.7956,
  '_' =>  28.4380, '+' =>  27.4838, '-' =>  15.8413, '=' =>  27.4838, ' ' =>  10.0201, ',' =>   9.6384, '.' =>  10.4973, ';' =>  11.3561, ':' =>  11.3561, '<' =>  27.4838,
  '>' =>  27.4838, '/' =>  20.8037, '?' =>  27.1020, '"' =>  19.4677, '\''=>  10.4973, '`' =>  17.7499, '[' =>  15.2687, ']' =>  15.2687, '{' =>  19.4677, '}' =>  19.4677,
  '°' =>  18.4179, '\\'=>  20.8037, '|' =>  14.6962, '£' =>  27.5792, '´' =>  17.7499,
);
my %CHARLEFT  = (	# Origin to leftmost point
  'a' =>   1.2883, 'b' =>   1.7177, 'c' =>   1.4314, 'd' =>   1.4314, 'e' =>   1.4314, 'f' =>   0.0954, 'g' =>   1.4314, 'h' =>   1.7177, 'i' =>   1.7177, 'j' =>  -0.1431,
  'k' =>   1.7177, 'l' =>   1.7177, 'm' =>   1.7177, 'n' =>   1.7177, 'o' =>   1.4314, 'p' =>   1.7177, 'q' =>   1.4314, 'r' =>   1.7177, 's' =>   1.0020, 't' =>   0.1431,
  'u' =>   1.5746, 'v' =>  -0.2863, 'w' =>  -0.1431, 'x' =>   0.0000, 'y' =>  -0.2863, 'z' =>   0.2863,
  'A' =>  -0.2863, 'B' =>   2.0040, 'C' =>   1.7177, 'D' =>   2.0040, 'E' =>   2.0040, 'F' =>   2.0040, 'G' =>   1.7177, 'H' =>   2.0040, 'I' =>   2.0040, 'J' =>   0.4294,
  'K' =>   2.0040, 'L' =>   2.0040, 'M' =>   2.0040, 'N' =>   2.0040, 'O' =>   1.7177, 'P' =>   2.0040, 'Q' =>   1.7177, 'R' =>   2.0040, 'S' =>   1.1690, 'T' =>   0.2863,
  'U' =>   1.8609, 'V' =>  -0.2863, 'W' =>   0.0000, 'X' =>   0.0000, 'Y' =>  -0.2863, 'Z' =>   0.2863,
  '1' =>   0.2863, '2' =>   1.2883, '3' =>   1.4314, '4' =>   0.2863, '5' =>   1.5746, '6' =>   1.7177, '7' =>   0.2863, '8' =>   1.5746, '9' =>   1.7177, '0' =>   1.7177,
  '!' =>   1.7177, '@' =>   0.8589, '#' =>   0.8589, '$' =>   1.2883, '^' =>   0.2863, '&' =>   0.8589, '%' =>   0.8589, '*' =>   0.7157, '(' =>   2.0040, ')' =>   0.8589,
  '_' =>  -0.2863, '+' =>   1.5269, '-' =>   0.8589, '=' =>   1.5507, ' ' =>   0.0000, ',' =>   0.8589, '.' =>   0.8589, ';' =>   1.7177, ':' =>   1.7177, '<' =>   1.5507,
  '>' =>   1.5507, '/' =>   0.2863, '?' =>   2.0040, '"' =>   0.8589, '\''=>   0.8589, '`' =>   0.0000, '[' =>   2.0040, ']' =>   0.8589, '{' =>   0.8589, '}' =>   0.8589,
  '°' =>   0.8589, '\\'=>   0.2863, '|' =>   4.0319, '£' =>   1.7177, '´' =>   3.6025,
);
my %CHARIGHT  = (	# Origin to rightmost point
  'a' =>  23.0224, 'b' =>  23.9290, 'c' =>  23.0224, 'd' =>  23.6427, 'e' =>  23.5473, 'f' =>  14.0043, 'g' =>  23.6188, 'h' =>  24.0006, 'i' =>  11.6424, 'j' =>  11.9287,
  'k' =>  23.6427, 'l' =>  11.6424, 'm' =>  36.0724, 'n' =>  23.9529, 'o' =>  23.5234, 'p' =>  23.9051, 'q' =>  23.5711, 'r' =>  17.0581, 's' =>  22.1397, 't' =>  14.8870,
  'u' =>  23.7859, 'v' =>  21.6387, 'w' =>  32.8517, 'x' =>  21.6148, 'y' =>  21.9011, 'z' =>  16.7241,
  'A' =>  25.0980, 'B' =>  25.3843, 'C' =>  25.4559, 'D' =>  25.2650, 'E' =>  19.4199, 'F' =>  19.0144, 'G' =>  25.0503, 'H' =>  25.0980, 'I' =>  12.0480, 'J' =>  14.2906,
  'K' =>  26.4817, 'L' =>  18.1555, 'M' =>  32.9948, 'N' =>  24.4300, 'O' =>  24.9549, 'P' =>  23.6427, 'Q' =>  24.9787, 'R' =>  24.4300, 'S' =>  24.1199, 'T' =>  22.2351,
  'U' =>  24.8356, 'V' =>  25.8614, 'W' =>  39.7942, 'X' =>  24.1437, 'Y' =>  23.4041, 'Z' =>  18.7996,
  '1' =>  16.5809, '2' =>  23.3564, '3' =>  24.1676, '4' =>  24.1199, '5' =>  24.6447, '6' =>  24.8594, '7' =>  18.7042, '8' =>  24.5254, '9' =>  24.8594, '0' =>  24.4539,
  '!' =>  12.3104, '@' =>  37.0506, '#' =>  29.7502, '$' =>  25.4320, '^' =>  23.3087, '&' =>  28.1518, '%' =>  32.9948, '*' =>  13.0023, '(' =>  14.4337, ')' =>  13.2886,
  '_' =>  27.2690, '+' =>  24.5254, '-' =>  13.5272, '=' =>  24.5016, ' ' =>   0.0000, ',' =>   7.3242, '.' =>   8.1354, ';' =>   8.9942, ':' =>   8.9942, '<' =>  24.5016,
  '>' =>  24.5016, '/' =>  19.0382, '?' =>  24.7640, '"' =>  17.2012, '\''=>   8.1592, '`' =>  12.6921, '[' =>  12.9069, ']' =>  11.7617, '{' =>  17.1773, '}' =>  17.1773,
  '°' =>  16.0799, '\\'=>  19.0382, '|' =>   9.2090, '£' =>  25.0026, '´' =>  16.2946,
);

sub TextWidth ($@) {
  my ($text, $fsize) = @_;

  my $wref = $CHARWIDTH{$CHARWREF} - $CHARLEFT{$CHARWREF};
  debug ("<TEXTWIDTH> \"%s\" @ %f ('%s' = %s @ %s)", $text, $fsize, $CHARWREF, $wref, $CHARFSIZE);

  my @text = split (//, $text);
  debug ("    - text  = [\"%s\"]", join ("\", \"", @text));
  my @width;

  my $left  = $CHARLEFT{$text[0]};
  my $right = $CHARWIDTH{$text[-1]} - $CHARIGHT{$text[-1]} - $CHARLEFT{$CHARWREF};

  my $width = 0 - $left - $right;
  foreach my $c (@text) {
    my $w = $CHARWIDTH{$c} - $CHARLEFT{$CHARWREF};
    push (@width, $w);
    $width += $w;
  }

  debug ("    - width = [\"%s\"]", join ("\", \"", @width));

  if ($fsize <= 0) {
    $fsize = 1.0;
  }

  my ($uleft, $uwidth, $uright) = ( $left/$CHARFSIZE, $width/$CHARFSIZE, $right/$CHARFSIZE);
  my ($fleft, $fwidth, $fright) = ($uleft*$fsize,    $uwidth*$fsize,    $uright*$fsize    );
  debug ("    => %s = %s / %s @ %f => %s", $uwidth, $width, $CHARFSIZE, $fsize, $fwidth);

  return ($fleft, $fwidth, $fright);
}

#
# Useful REGEXPs
#
my $reSIGname   = '(\w+)';
my $reSIGtext   = '(?:\s*/"([^"]*)")?';
my $reSIGclocki = '(?:\s*~(\d+)(?:@(\d+)%)?)?'; # ($phases, $dutcyc)
my $reSIGclocke = '(?:\s*(~)(\d+)?(?:@(\d+)%)?)?'; # ($phases, $dutcyc)
my $reSIGvalue  = '(?:([01XZLHW])|"(<?)([^>"]*)(>?)")'; # ($logicval, $ltvjust, $textval, $rtvjust)
my $reSIGoffset = '(?:\s*([-+]\d+(?:[.]\d+)?)%)?'; # ($offset)
my $reSIGjitter = '(?:\s*:(?:([-]\d+(?:[.]\d+)?)%[*](\d+))?' .
                         '(?:([+]\d+(?:[.]\d+)?)%[*](\d+))?)?'; # ($noffj, $ncntj, $poffj, $pcntj)
my $reSIGtimpos = '(/+|\\\\+)'; # ($vpos)
my $reSIGtimref = '([-<>])(?:(\w+)(?:@(\w+))?(?:([-+]\d+))?)([-<>])'; # ($leftend, $refname, $reftag, $refoffset, $rightend)
my $reSIGtimtxt = '"(<)?(?|([^"]*)(>)"|([^"]*[^>])"|")'; # ($leftalign, $text, $rightalign)
my $reSIGtiming = "(?:$reSIGtimpos$reSIGtimref$reSIGtimtxt)";
my $reSIGtag    = '(?:/(\w+))?';

my $re_empty    = '^\s*$';
my $re_comment  = '^([^#]*)#\s*(.*)$'; # ($text, $comment)
my $re_variable = '^\s*\$(\w+)\s*=\s*(?:(\d+(?:\.\d+)?)\s*(px|pt|pc|in|mm|cm)?|"([^"]+)"|([-+]\d+(?:\.\d+)?)%)\s*$'; # ($varname, $varnum, $varunit, $vartext, $varprop)
my $re_boundary = '^\s*[[]([-+]?\d+(?:[.]\d+)?)%:([-+]?\d+(?:[.]\d+)?)%[]]\s*$'; # ($cycleol, $cycleor)
my $re_init     = "^\\s*$reSIGname($reSIGtext)$reSIGclocki\\s*([=:])\\s*$reSIGvalue?$reSIGoffset\\s*\$";
my $re_cycle    = '^\s*([.\|!]+)\s*$'; # (@cycle)
my $re_event    = "^\\s*$reSIGname$reSIGtag$reSIGclocke\\s*=\\s*$reSIGvalue?$reSIGoffset$reSIGjitter\\s*(($reSIGtiming\\s*)*)\$";
my $re_timref   = "^\\s*$reSIGname([-+]\\d+)?\\s*(($reSIGtiming\\s*)*)\$";
my $re_hiatus   = "^\\s*~(?:$reSIGname\\s*)?$reSIGoffset\\s*\$";
my $re_subtime  = '^\s*[{}](\s|[{})*\s*$';

#
# Command options:
#  - DEBUG, output debugging information
#  - HELP, give help about script
#
my %OPTIONS = (
  "debug"     => \$DEBUG,
  "help"      => \$HELP,
);

# Augment basic options with internal variables and parse them all
foreach my $vname (keys %VARIABLE) {
  $OPTIONS{$vname . "=s"} = sub {ParseVARIABLE (@_)};
}
GetOptions (%OPTIONS);

# Print usage if arguments are inconsistent
if ($HELP) {
  print STDERR "usage: $0 [-help] [-debug]\n";
  exit (1);
}

#
# Here is the real stuff...
#

my $timing = TimingCreate ();

ParseTMG ($timing);
DumpTiming ($timing) if $DEBUG;

exit ($error) if ($error);

RenderTiming ($timing);

exit (0);

#
# End of main
#

#
# Support routines
#

sub warning ($@) {
  my ($format, @varargs) = @_;
  printf STDERR ("**WARNING:$.** " . $format . "\n", @varargs);
  $warning++;
}
sub error ($@) {
  my ($format, @varargs) = @_;
  printf STDERR ("**ERROR:$.** " . $format . "\n", @varargs);
  $error++;
}

sub debug ($@) {
  my ($format, @varargs) = @_;
  return if (!$DEBUG);
  printf STDERR ("**DEBUG:$.** " . join ("\n**DEBUG:$.** ", (split ("\n", $format))) . "\n", @varargs);
}

#
# Timing Diagram
#

sub TimingCreate (@) { #$$$$$$) {
  my ($scale, $margin, $ratio, $sx, $sy, $sedge) = @_;
  my $timing = new TIMING (
    scale    => (defined $scale)  ? $scale  : 1.0,
    margin   => (defined $margin) ? $margin : 0.0,
    wcratio  => (defined $ratio)  ? $ratio  : 1.0,
    siglmax  => 0,
    sx       => (defined $sx)     ? $sx     : 0.0,
    sy       => (defined $sy)     ? $sy     : 0.0,
    sigedge  => (defined $sedge)  ? $sedge  : 0.0,
    cycles   => 0,
    dtl      => 0,
    dtr      => 0,
    cyclesep => [],
    signals  => [],
    signal   => {},
    );

  return ($timing);
}

sub ParseTMG ($) {
  my ($timing) = @_;

  my $cycle = -1;

  while (<>) {

    # Clean up
    chomp;
#    debug ("<LINE> %s", $_);

    # Catch & reduce comments
    if (/$re_comment/o) {
      my ($text, $comment) = ($1, $2);
      debug ("<COMMENT> comment = \"%s\"", $comment);
      $_ = $text;
    }

    # Skip empty lines
    if (/$re_empty/o) {
      next;
    }

    # Manage variable setting
    if (/$re_variable/o) {
      my ($vname, $vnum, $vunit, $vtext, $vprop) = (lc($1), $2, $3, $4, $5);
      ParseVARIABLE ($vname, undef, $vnum, $vunit, $vtext, $vprop);
      next;
    }

    # Manage cycle L/R boundaries

    if (/$re_boundary/o) {
      my ($cycleol, $cycleor) = ($1, $2);

      debug ("<BOUNDARY> [%s%%:%s%%]", $cycleol, $cycleor);

      # Left boundary must be negative or null, no more than a cycle...
      if (($cycleol > 0.0) || ($cycleol < -100.0)) {
	error ("cycle left boundary shall be negative or null, no more than a cycle (%s%%)", $cycleol);
      } else {
	$DTL = -$cycleol/100;
      }

      # Right boundary must be positive or null, no more than a cycle...
      if (($cycleor < 0.0) || ($cycleol > 100.0)) {
	error ("cycle right boundary shall be positive or null, no more than a cycle (%s%%)", $cycleor);
      } else {
	$DTR = $cycleor/100;
      }

      next;
    }

    # Manage signal init
    if (($cycle < 0) && /$re_init/oi) {
      my ($sname, $stextp, $stext, $scphases, $scduty, $sassign, $svlogic, $svtljust, $svtext, $svtrjust, $soffset) = ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11);
      debug ("<INIT> %s = %s (shown as \"%s\")", $sname, ($svlogic ? $svlogic : $svtext), ($stext ? $stext : $sname));

      # Verify signal does not already exist
      if (exists ${$timing->signal}{$sname}) {
	error ("attempt to declare already existing signal (%s)", $sname);
	next;
      }
      # Verify offset is kept within a cycle boundary
      if ((defined $soffset) && !(($scduty > -100) && ($scduty < 100))) {
	error ("attempt to declare signal with an invalid offset (%s / %d%)", $sname, $soffset);
	next;
      }
      # Verify duty cycle is ... decent!
      if ((defined $scduty) && !(($scduty > 0) && ($scduty < 100))) {
	error ("attempt to declare clock signal with an invalid duty cycle (%s / %d%)", $sname, $scduty);
	next;
      }
      # Then create it
      {
	my $signal = new SIGNAL (
	  name   => $stextp ? $stext : $sname,
	  value  => undef,
	  time   => undef,
	  offset => (defined $soffset) ? ($soffset / 100.0) : 0.0,
	  clock  => (defined $scphases) ? $scphases : 0,
	  duty   => (defined $scduty) ? ($scduty / 100) : 0.5,
	  pvalue => (defined $svlogic) ? $svlogic : ((defined $svtext) ? $svtext : undef),
	  noline => ($sassign eq ':'),
	  logic  => (defined $svlogic),
	  wave   => [],
	  tagidx => {},
	  minlvl => 0,
	  maxlvl => 0,
	  hiatus => [],
	  );
	  $signal->value($signal->pvalue);

	# Make the clock/signal exist before the largest left window
	if (($signal->clock > 0) && ($signal->logic) && (exists $CLOCKN{$signal->pvalue})) {
	  CycleClockMaybe ($signal, -2);
	  CycleClockMaybe ($signal, -1);
	} else {
	  my $wave = new WAVE (
	    cycle => -2,
	    time  => -2.0,
	    value => $signal->pvalue,
	    logic => $signal->logic,
	    align => $svtljust ? ($svtrjust ? 0 : -1) : ($svtrjust ? 1 : 0),
	      );
	  push (@{$signal->wave}, $wave);
	}

	# Register signal in timing diagram's tables
	${$timing->signal}{$sname} = $signal;
	push (@{$timing->signals}, $sname);

	# Update the max signal name length
	my ($tl, $tw, $tr) = TextWidth ($signal->name);
	if ($timing->siglmax < ($tl+$tw+$tr)) {
	  $timing->siglmax ($tl+$tw+$tr);
	}
      }

      next;
    }

    # Manage cycles
    if (/$re_cycle/oi) {
      my ($cycles) = ($1);

      foreach my $c (split (//, $cycles)) {
	debug ("<CYCLE> %s", $c);

	# Next cycle
	$cycle++;

	# Add vertical bar information if asked to
	push (@{$timing->cyclesep}, $c);

	# Handle clocks (as a special case out of init, do not run clocks on the very first cycle so we can force them if needed)
	if ($cycle > 0) {
	  foreach my $signal (@{$timing->signals}) {
	    my $s = ${$timing->signal}{$signal};
	    if ($s->clock > 0) {
	      CycleClockMaybe ($s, $cycle-1);
	    }
	  }
	}

      }

      next;
    }

    # Manage signal event
    if (/$re_event/oi) {
      my ($sname, $stag, $sclock, $scphase, $scduty, $svlogic, $svtljust, $svtext, $svtrjust, $soffset, $noffj, $ncntj, $poffj, $pcntj, $timrefs) = ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15);
      my @timref = split (/\s+(?=[\/\\])/, $timrefs);
      debug ("<EVENT>\n" .
	     " - signal = %s\n" .
	     " - tag    = %s\n" .
	     " - clock  = %s\n" .
	     " - phase  = %s\n" .
	     " - duty   = %s\n" .
	     " - value  = %s%s%s%s\n" .
	     " - offset = %s\n" .
	     " - jitter = %s%%*%s / %s%%*%s\n" .
	     " - timref = %s | %s\n",
	     $sname, $stag, $sclock, $scphase, $scduty, $svlogic, $svtljust, $svtext, $svtrjust, $soffset, $noffj, $ncntj, $poffj, $pcntj, $timrefs, "[" . join ("] [", @timref) . "]");

      # Catch attempts to provide full event info during init
      if ($cycle < 0) {
	error ("attempt to create signal event during init phase (\"%s\")", $sname);
	next;
      }

      # Check signal has been declared...
      if (! (exists ${$timing->signal}{$sname})) {
	error ("unknown signal (\"%s\")", $sname);
	next;
      }

      # Verify offset is kept within a cycle boundary
      if (   (defined $soffset)
	  && (($soffset <= -100) || (100 <= $soffset))) {
	error ("attempt to assign signal an invalid offset (%s / %d%%)", $sname, $soffset);
	next;
      }
      # Verify negative jitter is kept within a cycle boundary
      if (   (defined $noffj)
	  && (($noffj < -100) || (0 <= $noffj) || ($ncntj <= 0))) {
	error ("attempt to assign signal an invalid negative jitter offset or count (%s / %d%% / %d)", $sname, $noffj, $ncntj);
	next;
      }
      # Verify positive jitter is kept within a cycle boundary
      if (   (defined $poffj)
	  && (($poffj <= 0) || (100 < $poffj) || ($pcntj <= 0))) {
	error ("attempt to assign signal an invalid positive jitter offset or count (%s / %d%% / %d)", $sname, $poffj, $pcntj);
	next;
      }
      # Verify duty cycle is ... decent!
      if (   (defined $scduty)
	  && (($scduty <= 0) || (100 <= $scduty))) {
	error ("attempt to assign clock signal an invalid duty cycle (%s / %d%%)", $sname, $scduty);
	next;
      }
      # ... then add a new event
      {
	my $signal = ${$timing->signal}{$sname};

	my $time  = $cycle + ((defined $soffset) ? ($soffset / 100.0) : $signal->offset);
	my $value = (defined $svlogic) ? $svlogic : ((defined $svtext) ? $svtext : undef);
	my $align = (defined $svlogic) ? 0 : ($svtljust ? ($svtrjust ? 0 : -1) : ($svtrjust ? 1 : 0));

	my $refwaveidx;

	# Redefine a clock if asked to
	my $ckoffset = 0;
	if (defined $sclock) {

	  # Use the given phases or reuse previous one, if any
	  if (defined $scphase) {
	    $signal->clock ($scphase);
	  } else {
	    if ($signal->clock != 0) {
	      $signal->clock (abs($signal->clock));
	    } else {
	      error ("trying to implicitly reactivate clock for signal \"%s\"", $sname);
	      next;
	    }
	  }

	  # Use the given duty cycle or reuse previous one, if any
	  $signal->duty ((defined $scduty) ? ($scduty / 100) : 0.5);

	  # Finally verify clock value is cyclable!
	  if (exists $CLOCKN{$value}) {
	    $signal->pvalue ($CLOCKN{$value});
	    $signal->time (undef);
	    CycleClockMaybe ($signal, $cycle);
	    $ckoffset = $signal->clock - 1;
	  } else {
	    warning ("attempt to assign clock signal a non-clockable value (%s / %s), turning to signal", $sname, $value);
	    $sclock = undef;
	  }

	}

	# Deactivate clock (if it ever was) and register event
	if (!(defined $sclock)) {
	  $signal->clock (-abs($signal->clock));

	  # Create jitter events before reference, if any
	  if (defined ($noffj)) {
	    for (my $i=0; $i<$ncntj; $i++) {
	      my $timej = $time + $noffj*($ncntj-$i)/$ncntj/100;
	      my $wave  = new WAVE (
		cycle => $cycle,
		time  => $timej,
		jitter => (($i == 0) ? 'b' : 'm'),
		value => $value,
		logic => (defined $svlogic),
		align => $align,
		  );
	      push (@{$signal->wave}, $wave);
	      $signal->time ($timej);
	    }
	  }

	  # Create reference event
	  my $wave = new WAVE (
	    cycle => $cycle,
	    time  => $time,
	    jitter => defined ($noffj) ? (defined ($poffj) ? 'm' : 'e') : (defined ($poffj) ?'b' : undef),
	    value => $value,
	    logic => (defined $svlogic),
	    align => $align,
	      );
	  push (@{$signal->wave}, $wave);
	  $signal->time ($time);
	  $refwaveidx = $#{$signal->wave};

	  # Create jitter events after reference, if any
	  if (defined ($poffj)) {
	    for (my $i=0; $i<$pcntj; $i++) {
	      my $timej = $time + $poffj*($i+1)/$pcntj/100;
	      my $wavej  = new WAVE (
		cycle => $cycle,
		time  => $timej,
		jitter => (($i == ($pcntj-1)) ? 'e' : 'm'),
		value => $value,
		logic => (defined $svlogic),
		align => $align,
		  );
	      push (@{$signal->wave}, $wavej);
	      $signal->time ($timej);
	    }
	  }

	  # Finally adjust the signal for what it is
	  $signal->pvalue($signal->value);
	  $signal->value($value);
	}

	# Create a tag if asked too (allowed only once), appropriately adjusting for clocks
	if (defined $stag) {
	  if (exists ${$signal->tagidx}{$stag}) {
	    error ("tag \"%s\" redefined for signal \"%s\"", $stag, $sname);
	  } else {
	    ${$signal->tagidx}{$stag} = $refwaveidx - $ckoffset;
	  }
	}

	# Parse timing references, if any
	my $wave = ${$signal->wave}[$refwaveidx - $ckoffset];
	ParseTIMREF ($timing, $cycle, $signal, $wave, @timref);
      }

      next;
    }

    if (/$re_timref/oi) {
      my ($sname, $eoffset, $timrefs) = ($1, $2, $3);

      # Sanity check
      if (!(defined $eoffset)) {
	$eoffset = 0;
      }

      # Check signal has been declared...
      my $signal = (exists ${$timing->signal}{$sname}) ? ${$timing->signal}{$sname} : undef;
      if (!(defined $signal)) {
	error ("unknown signal (\"%s\")", $sname);
	next;
      }

      # Now we perform a few operations if referring to a clock
      #  - cycle the clock in this cycle (if not done already, allow it only once)
      #  - allow positive offset only is less than the number of phases
      #  - correct the offest by the number of phases, so we get a human-friendly reference
      if ($signal->clock > 0) {
	if ($eoffset >= $signal->clock) {
	  error ("bad timing ref, clock signal offset shall be less than clock phases (%d >= %d)", $sname, $eoffset, $signal->clock);
	  next;
	}
	CycleClockMaybe ($signal, $cycle);
	$eoffset -= $signal->clock - 1;
      } else {
	if ($eoffset > 0) {
	  error ("bad timing ref, signal offset shall be negative or null (vs %d)", $sname, $eoffset);
	  next;

	}
      }

      # Parse the timing references, if any
      my @timref = split (/\s+(?=[\/\\])/, $timrefs);
      debug ("<TIMREF>\n" .
	     " - signal = %s\n" .
	     " - offset = %s\n" .
	     " - timref = %s | %s\n",
	     $sname, $eoffset, $timrefs, "[" . join ("] [", @timref) . "]");

      my $wave = ${$signal->wave}[-1+$eoffset];
      if (defined $wave) {
	ParseTIMREF ($timing, $cycle, $signal, $wave, @timref);
      } else {
	error ("attempt to reference a signal with no wave");
      }

      next;
    }


    # Manage signal hiatus
    if (/$re_hiatus/oi) {
      my ($sname, $hoffset) = ($1, $2);
      debug ("<HIATUS>\n" .
	     " - signal = %s\n" .
	     " - offset = %s\n",
	     ($sname ? $sname : "<all>"), $hoffset);

      # Catch attempts to provide full event info during init
      if ($cycle < 0) {
	error ("attempt to create signal hiatus during init phase (\"%s\")", $sname);
	next;
      }

      # Check signal has been declared...
      if ($sname && (! (exists ${$timing->signal}{$sname}))) {
	error ("unknown signal (\"%s\")", $sname);
	next;
      }

      # ... then add a new hiatus
      {
	my $time = $cycle + ((defined $hoffset) ? ($hoffset / 100.0) : 0);
	foreach my $s ($sname ? ($sname) : @{$timing->signals}) {
	  my $signal = ${$timing->signal}{$s};
	  if (! $signal->noline) {
	    my $hiatus = new HIATUS (
	      cycle => $cycle,
	      time  => $time);
	    push (@{$signal->hiatus}, $hiatus);
	  }
	}
      }

      next;
    }

    # Catch any unrecognized statement
    error ("Unrecognized directive \"%s\"", $_);

  }

  # Create a (few) lasts event to reach end-of-cycle if needed
  foreach my $s (values %{$timing->signal}) {
    if (!(defined $s->time) || ($s->time < $cycle+2)) {
      debug ("<LAST>\n" .
	     " - signal = %s\n" .
	     " - time = %s\n",
	     $s->name, $s->time);
      if ($s->clock > 0) {
	CycleClockMaybe ($s, $cycle+0);
	CycleClockMaybe ($s, $cycle+1);
	CycleClockMaybe ($s, $cycle+2);
      } else {
	my $lw = ${$s->wave}[-1];
	push (@{$s->wave}, new WAVE (cycle => $cycle+2, time => $cycle+2, value => '=', logic => '=', align => undef));
	$s->time ($cycle+2);
      }
    }
  }

  # Update & return parsed data
  $timing->cycles($cycle);
  return ($timing);

}

sub CycleClockMaybe ($$) {
  my ($signal, $cycle) = @_;

  # We can only cycle a clock with defined values
  my $time   = $signal->time;
  my $value  = $signal->value;
  my $pvalue = $signal->pvalue;
  my $phases = $signal->clock;
  my $duty   = $signal->duty;
  my $offset = $signal->offset;

  my $period = 2.0 / $phases;
  my $widthh = $period * $duty;
  my $widthl = $period - $widthh;

  # If no event ever existed before, create the (pseudo) previous one
  if (!(defined $time)) {

    # At the very begining, create an event with the same value if we have an even number of phases, use opposite otherwise
    if (($cycle < 0) && (exists $CLOCKN{$pvalue})) {
      $value = (($cycle*$phases % 2) == 0) ? $pvalue : $CLOCKN{$pvalue};
    } else {
      $value = $pvalue;
    }

    # Now compute time of (pseudo) previous event
    $time = $cycle + $offset - ($CLOCKL{$value} ? $widthl : $widthh);

    # Do that only when on init, cuz we later unset/set $time later to manage "voids" (FIXME: what a kludge...)
    if ($cycle < 0) {
      debug (" - phase %d: %s @ %.3f", $phases, $value, $time);
      push (@{$signal->wave}, new WAVE (cycle => $cycle-1, time => $time, value => $value, logic => 1, align => 0));
    }
  }

  if (exists $CLOCKN{$value}) {

    # Check this clock has not been cycled already
    my $lwave = ${$signal->wave}[-1];
    if (($lwave->cycle) == $cycle) {
      debug ("<CLOCK> skipping %s clock cycling at cycle %d, done already", $signal->name, $cycle);
      return;
    } else {
      debug ("<CLOCK> stepping %s by %d phases @ cycle %d", $signal->name, $signal->clock, $cycle);
    }

    # Step over each phase and add new events
    for (my $phase=0; $phase < $phases; $phase++) {
      $time = $time + ($CLOCKL{$value} ? $widthl : $widthh);
      $pvalue = $value;
      $value  = $CLOCKN{$value};
      debug (" - phase %d: %s @ %.3f", $phase+1, $value, $time);
      push (@{$signal->wave}, new WAVE (cycle => $cycle, time => $time, value => $value, logic => 1, align => 0));
    }
    $signal->pvalue($pvalue);
    $signal->time ($time);
    $signal->value($value);
  }
}

sub ParseVARIABLE ($$@) {
  my ($varname, $optval, $varnum, $varunit, $vartext, $varprop) = @_;

  if (defined $optval) {
    my $vline = '$' . $varname . "=" . $optval;
    ($varname, $varnum, $varunit, $vartext, $varprop) = ($vline =~ /$re_variable/o);
  }
  $varname = lc ($varname);
  debug ("<VARIABLE> %s = %s%s%s%s", $varname, $varnum, $varunit, '"' . $vartext . '"', $varprop . "%");

  # Check variable exists
  if (!(exists $VARIABLE{$varname})) {
    error ("attempt to reference unknown variable \"%s\"", $varname);
  }

  # Variables cannot be set more than once...
  elsif (exists $VARLINESET{$varname}) {
    my $line = $VARLINESET{$varname};
    if ($line) {
      warning ("attempt to redefine variable (\"%s\", first set at line %d)", $varname, $line);
    } else {
      warning ("ignoring variable definition (\"%s\", first set on command line)", $varname);
    }
  }

  else {
    $VARLINESET{$varname} = $.;

    # Text value directly get affected
    if (defined $vartext) {
      ${$VARIABLE{$varname}} = $vartext;
      debug ("   - assign \"%s\"", $vartext);
    }

    # Text value directly get affected
    elsif (defined $varprop) {
      if (defined ${$VARIABLE{$varname}}) {
	my $offset = $varprop / 100.0;
	my $value  = ${$VARIABLE{$varname}} * (1 + $offset);
	debug ("   - assign (%s %s %s%%) = %s", ${$VARIABLE{$varname}}, ($offset < 0) ? "-" : "+",  $offset, $value);
	${$VARIABLE{$varname}} = $value;
      } else {
	error ("attempt to apply an offset to a variable with an undefined value (%s%% to %s)", $varprop, $varname);
      }

    }

    # Numerical value with no unit directly get affected too
    elsif (!(defined $varunit)) {
      ${$VARIABLE{$varname}} = $varnum;
      debug ("   - assign %s ", $varnum);
    }

    # Otherwise handle & convert unit to pixels
    else {
      my $value;
      given ($varunit) {
	when (/px/)       { $value = $varnum;               break; }
	when (/pt/)       { $value = $varnum * $DPI / 72;   break; }
	when (/pc/)       { $value = $varnum * $DPI / 6;    break; }
	when (/in/)       { $value = $varnum * $DPI;        break; }
	when (/mm/)       { $value = $varnum * $DPI / 25.4; break; }
	when (/cm/)       { $value = $varnum * $DPI / 2.54; break; }
	default { }
      }
      ${$VARIABLE{$varname}} = $value;
      debug ("   - assign %s px ", $value);
    }
  }

}

sub ParseTIMREF ($$$@) {
  my ($timing, $cycle, $signal, $wave, @timref) = @_;

  # ... and process timing references, if any
  foreach my $t (@timref) {
    my ($vpos, $leftend, $refname, $reftag, $refoffset, $rightend, $rightalign, $text, $leftalign) = ($t =~ /$reSIGtiming/);
    debug ("   <TIMREF>\n" .
	   "    - vpos   = %s\n" .
	   "    - sigref = %s \"%s\" \"%s\" \"%s\" %s\n" .
	   "    - text   = %s \"%s\" %s\n",
	   $vpos, $leftend, $refname, $reftag, $refoffset, $rightend, $rightalign, $text, $leftalign);

    # Make sure we were not asked for an impossible arrow set
    if (($leftend eq '>') && ($rightend ne '<') || ($rightend eq '<') && ($leftend ne '>')) {
      error ("invalid timing arrow set (\"$leftend...$rightend\")");
    }

    # Check the referenced signal exists
    my $refsig = (exists ${$timing->signal}{$refname}) ? ${$timing->signal}{$refname} : undef;
    if (!(defined $refsig)) {
      error ("bad timing ref, unknown reference signal (\"%s\")", $refname);
    }

    # Update the signal level information
    my $vlevel = (($vpos =~ /\/+/) ? 1 : -1) * length ($vpos);
    if ($vlevel > $signal->maxlvl) {
      $signal->maxlvl ($vlevel);
    }
    if ($vlevel < -$signal->minlvl) {
      $signal->minlvl (-$vlevel);
    }

    # Adjust offset if referring to a (active) clock, only in case no tag was given
    if (!(defined $reftag) && ($refsig->clock > 0)) {
      CycleClockMaybe ($refsig, $cycle);
      $refoffset -= $refsig->clock - 1;
    }

    # And create & populate a timing reference
    my $timref = new TIMREF (
      sigref => $refname,
      evtref => $#{$refsig->wave},
      offref => $refoffset,
      tagref => $reftag,
      text   => $text,
      align  => ($leftalign ne '') ? (($rightalign ne '') ? 0 : -1) : (($rightalign ne '') ? 1 : 0),
      arrowr => ($leftend  eq '>') ? -1 : (($leftend  eq '-') ? 0 : 1),
      arrows => ($rightend eq '<') ? -1 : (($rightend eq '-') ? 0 : 1),
      level  => $vlevel);
    push (@{$wave->timref}, $timref);
    debug ("\n" .
	   "    - sigref = %s\n" .
	   "    - evtref = %s\n" .
	   "    - offref = %s\n" .
	   "    - tagref = %s\n" .
	   "    - text   = %s\n" .
	   "    - align  = %s (\"%s\" & \"%s\")\n" .
	   "    - arrowr = %d\n" .
	   "    - arrows = %d\n" .
	   "    - level  = %d\n",
	   $timref->sigref, $timref->evtref, $timref->offref, $timref->tagref, $timref->text, $timref->align, $rightalign, $leftalign, $timref->arrowr, $timref->arrows, $timref->level);
  }

}

sub DumpTiming ($) {
  my ($timing) = @_;

  debug ("<DUMP>");
  debug ("<DUMP> Timing sequence has %d cycles:", $timing->cycles);
  for (my $cycle = 0; $cycle <= $timing->cycles; $cycle++) {
    debug ("<DUMP>    %2d => %s", $cycle, ${$timing->cyclesep}[$cycle]);
  }
  foreach my $signal (@{$timing->signals}) {
    my $s = ${$timing->signal}{$signal};
    debug ("<DUMP>");
    debug ("<DUMP>  * %s (shown as \"%s\"" . ($s->clock ? ", clock with %d phases per cycle" : ""). ")", $signal, $s->name, $s->clock);
    foreach my $w (@{$s->wave}) {
      my $align = $w->logic ? "" : (($w->align < 0) ? "/left" : (($w->align > 0) ? "/right" : "/center"));
      debug ("<DUMP>    %.3f @ %d => %s%s / %s", $w->time, $w->cycle, , $w->logic ? $w->value : '"' . $w->value . '"', $align, $w->jitter);
    }
  }

}

sub RenderTiming ($) {
  my ($timing) = @_;

  debug ("<RENDER>... ");

  # Evaluate each signal/wave vertical position
  my $y = 0;
  foreach my $signal (@{$timing->signals}) {
    my $s = ${$timing->signal}{$signal};
    $s->yh ($y);
    $y += $TIMINGH*($s->maxlvl);
    $s->y ($y);
    $y += $SIGNALH + $TIMINGH*($s->minlvl);
    $s->yl ($y);
  }

  # Evaluate diagram size (with a slight margin on the signal name...)
  my ($w, $h, $sx, $sy) = ($timing->cycles, $y, $SNAMEO + $timing->siglmax*(1+$SNAMET)*$FSIZES, 0);

  # First adjust RATIO according to SIGNALH, in case it is not unity....
  debug ("<ADJUST> NONE");
  $RATIO = $RATIO * $SIGNALH;
  debug ("   - RATIO = %s", $RATIO);

  # Apply width and/or height constraints, if any...
#    warning ("variable value \"%s\" ignored when sizing for \"%s\"", "", "") if (defined $);
  if (defined $CYCLE) {
    debug ("<SIZING> CYCLE = %s", $CYCLE);
    if (defined $SIGROW) {
      debug ("<SIZING> SIGROW = %s", $SIGROW);
      $SCALE = $SIGROW / $SIGNALH;
      debug ("   - SCALE = %s", $SCALE);
      $RATIO = $CYCLE / $SCALE;
      debug ("   - RATIO = %s", $RATIO);
    } elsif (defined $HEIGHT) {
      $SCALE = ($HEIGHT - 2*$MARGIN) / ($sy + $h);
      debug ("   - SCALE = %s", $SCALE);
      $RATIO = $CYCLE / $SCALE;
      debug ("   - RATIO = %s", $RATIO);
    } else {
      $SCALE = $CYCLE / $RATIO;
      debug ("   - SCALE = %s", $SCALE);
    }
  }
  elsif (defined $SIGROW) {
    debug ("<SIZING> SIGROW = %s", $SIGROW);
    $SCALE = $SIGROW / $SIGNALH;
    debug ("   - SCALE = %s", $SCALE);
    if (defined $WIDTH) {
      debug ("<SIZING> WIDTH = %s", $WIDTH);
      $RATIO = ((($WIDTH - 2*$MARGIN) / $SCALE) - $sx) / ($DTL + $w +$DTR);
      debug ("   - RATIO = %s", $RATIO);
    }
  }
  elsif (defined $HEIGHT) {
    debug ("<SIZING> HEIGHT = %s", $HEIGHT);
    $SCALE = ($HEIGHT - 2*$MARGIN) / ($sy + $h);
    debug ("   - SCALE = %s", $SCALE);
    if (defined $WIDTH) {
      debug ("<SIZING> WIDTH = %s", $WIDTH);
      $RATIO = ((($WIDTH - 2*$MARGIN) / $SCALE) - $sx) / ($DTL + $w +$DTR);
      debug ("   - RATIO = %s", $RATIO);
    }
  }
  elsif (defined $WIDTH) {
    debug ("<SIZING> WIDTH = %s", $WIDTH);
    $SCALE = ($WIDTH  - 2*$MARGIN) / ($sx + $RATIO*($DTL + $w +$DTR));
    debug ("   - SCALE = %s", $SCALE);
  }

  # First populate the timing structure
  $timing->scale    ($SCALE);
  $timing->margin   ($MARGIN);
  $timing->wcratio  ($RATIO);
  $timing->sx       ($sx);
  $timing->sy       ($sy);
  $timing->dtl      ($DTL);
  $timing->dtr      ($DTR);
  $timing->sigedge  ($SEDGE);

  # Create an SVG space & populate it
  {
    my ($swm, $shm) = RenderWH ($timing, $w, $h);
    my $svg = new SVG (width => $swm, height => $shm);

    # Create common definitions (filters)
    RenderCommon ($svg);

    # Create a white background (we need a fake timing table with neutral scale/ratio)
    my $tident = TimingCreate ();
    RenderPolygon ($svg, $tident, $WHITE, 0, 1, (0,0), ($swm, 0), ($swm, $shm), (0,$shm));

    # Create an horizontal ground line in gray (logic signals only)
    foreach my $signal (@{$timing->signals}) {
      my $s = ${$timing->signal}{$signal};
      debug ("<RENDER HBAR> signal %s... (%s)", $s->name, $s->logic ? "logic" : " text");
      if ($s->logic) {
	my $w  = $SWIDTH/4;
	my $dx = UnratioX ($timing, $w/2);
	my $xl = 0               - $timing->dtl + $dx;
	my $xr = $timing->cycles + $timing->dtr - $dx;
	my $yb = $s->y + $SIGNALH - $SIGNALH*$SM/2;
	RenderPolyline ($svg, $timing, $GRAY, $w, 1, ($xl,$yb), ($xr,$yb));
      }
    }


    # Process vertical cycle-separators, except first (unless DTL non-null) and last (unless DTR non null)
    my $dtlom = ($timing->dtl > 0.0) ? 1 : 0;
    my $dtrom = ($timing->dtr > 0.0) ? 1 : 0;
    for (my $cycle=1-$dtlom; $cycle < $timing->cycles+$dtrom; $cycle++) {
      RenderVbar ($svg, $timing, $cycle, ${$timing->cyclesep}[$cycle]);
    }

    # Process each timing ref in a signal, wave by wave
    foreach my $signal (@{$timing->signals}) {
      my $s = ${$timing->signal}{$signal};
      my $ys = $s->y;
      debug ("<RENDER TIMREF> signal %s... ", $s->name);
      foreach my $w (@{$s->wave}) {
	my $time  = $w->time;
	my $cycle = $w->cycle;
	foreach my $t (@{$w->timref}) {
	  my $sr = ${$timing->signal}{$t->sigref};
	  my $yr = $sr->y;
	  RenderTimref ($svg, $timing, $t, $cycle, $time, $ys, $yr);

	  # We shall have a timref tag mark here (done later when rendering signal)
	  $w->timtag(1);
	}
      }
    }

    # Process each signal
    foreach my $signal (@{$timing->signals}) {
      my $s = ${$timing->signal}{$signal};
      my $y = $s->y;
      my $yh = $y + ($SIGNALH *      $SM);
      my $yl = $y + ($SIGNALH * (1 - $SM));
      debug ("<RENDER TIMING> signal %s @ %+.1f", $s->name, $y);
      RenderSignal ($svg, $timing, $s, $yh, $yl);

      # Render hiatus, if any
      foreach my $h (@{$s->hiatus}) {
	RenderHiatus ($svg, $timing, "", "", $HWIDTH, $h->time, $y+$SIGNALH/2, $HIATUSW, $HIATUSH);
      }
      #
    }

    # Process vertical cycle-separators, only first/last when dtl/dtr is null
    # (and adjust them horizontally so no signal will extend out...)
    my (@cycles, @xwoffset);
    my $xwoffset = UnratioX ($timing, $VWIDTH/2);
    if ($timing->dtl == 0.0) {
      push (@cycles, 0);
      push (@xwoffset, +$xwoffset);
    }
    if ($timing->dtr == 0.0) {
      push (@cycles, $timing->cycles);
      push (@xwoffset, -$xwoffset);
    }
    foreach my $cycle (@cycles) {
      my $xwo = shift (@xwoffset);
      RenderVbar ($svg, $timing, $cycle+$xwo, ${$timing->cyclesep}[$cycle]);
    }

    print $svg->xmlify ();
  }
}

sub RenderVbar ($$$$) {
  my ($svg, $timing, $x, $sep) = @_;

  debug ("<RENDER VBAR> \"%s\" @ %s... ", $sep, $x);
  # First identify the starting & ending point(s)...
  my $h = ${$timing->signal}{${$timing->signals}[-1]}->yl;
  my @ypair;
  if ($sep eq '|') {
    push (@ypair, 0, $h);
  } elsif ($sep eq '!') {
    my $show = 0;
    foreach my $signal (@{$timing->signals}) {
      my $s = ${$timing->signal}{$signal};
      if ((!$show) && (!$s->noline)) {
	push (@ypair, $s->yh);
	$show = 1;
      } elsif ($show && $s->noline) {
	push (@ypair, $s->yl);
	$show = 0;
      }
    }
    if ($show) {
      push (@ypair, $h);
    }
  }

  # ... Then render each pair (adjusting by half a width if top or bottom point)
  my ($yh, $yl);
  while (@ypair) {
    ($yh, $yl, @ypair) = @ypair;
    $yh += $VWIDTH/2 if ($yh ==  0);
    $yl -= $VWIDTH/2 if ($yl == $h);
    RenderPolyline ($svg, $timing, "| ", $VWIDTH, 1, ($x,$yh), ($x,$yl));
  }

}

sub RenderTimref ($$$$$$) {
  my ($svg, $timing, $timref, $cs, $ts, $ys, $yr) = @_;

  # First parse reference signal's wave for the closest event
  my $sigref = ${$timing->signal}{$timref->sigref};
  my @wr = @{$sigref->wave};
  my $ir = $timref->evtref;
  if ($timref->tagref ne '') {
    if (exists ${$sigref->tagidx}{$timref->tagref}) {
      $ir = ${$sigref->tagidx}{$timref->tagref};
    } else {
      error ("could not find reference event \"%s\" in signal wave \"%s\"", $timref->tagref, $timref->sigref);
      return;
    }
  } 
  if (($ir < 0) || (($ir+$timref->offref) < 0) || (($ir+$timref->offref) > $#wr)) {
    error ("could not find event with offset %d in signal wave \"%s\"", $timref->offref, $timref->sigref);
    return;
  }
  my $wi = $ir+$timref->offref;
  my $tr = $wr[$wi]->time;

  # We shall have a timref tag mark here (done later when rendering signal)
  $wr[$wi]->timtag(1);

  # Now compute elevations...
  my ($ysh, $ysl, $ysm, $yrh, $yrl);
  if ($timref->level > 0) {
    $ysh = $ys - ($timref->level * $TIMINGH) + ($TIMINGH *      $TM);
    $ysm = $ys - ($timref->level * $TIMINGH) + ($TIMINGH /      2  );
#   $ysl = $ys -                   $TIMINGH  + ($TIMINGH * (1 - $TM));
    $ysl = $ys                               + ($SIGNALH *      $SM);
  } else {
#   $ysh = $ys + $SIGNALH                                     + ($TIMINGH *      $TM);
    $ysh = $ys + $SIGNALH                                     - ($SIGNALH *      $SM);
    $ysm = $ys + $SIGNALH - ((1 + $timref->level) * $TIMINGH) + ($TIMINGH /      2   );
    $ysl = $ys + $SIGNALH - ((1 + $timref->level) * $TIMINGH) + ($TIMINGH * (1 - $TM));
  }
  if ($yr < $ys) {
    $yrh = $ysm;
#   $yrl = $yr + $SIGNALH + ($TIMINGH *      $TM);
    $yrl = $yr            + ($SIGNALH * (1 - $SM));
  } elsif ($yr > $ys) {
    $yrh = $ysm;
#   $yrl = $yr            - ($TIMINGH *      $TM);
    $yrl = $yr            + ($SIGNALH *      $SM);
  } else {
    # As a special case, signals with a timing reference to themselves ... share the same elevation!
    $yrh = $ysh;
    $yrl = $ysl;
  }

  # And render vertical bars, ...
  RenderPolyline ($svg, $timing, "| ", $VWIDTH, 1, ($tr, $yrh), ($tr, $yrl));
  RenderPolyline ($svg, $timing, "| ", $VWIDTH, 1, ($ts, $ysh), ($ts, $ysl));

  # ... horizontal bars, possibly with arrows... (we shift the arrows by a line width so to adjust the arrow end next to the vertical bar) ...
  my $da = $TIMINGH / (($tr < $ts) ? 5 : -5);
  my $dax = UnratioX ($timing, $da);
  my $day = $da;
  my $daa = UnratioX ($timing, ($tr < $ts) ? $VWIDTH : -$VWIDTH);
  my $dar = ($timref->arrowr) ? $daa : 0;
  my $das = ($timref->arrows) ? $daa : 0;
  my $dae = 0;

  #
  if (($timref->arrowr < 0) && ($timref->arrows < 0)) {
    $dae = 1.5*$dax;
    RenderPolyline ($svg, $timing, "| ",   $VWIDTH, 1, ($tr-$dar, $ysm), ($tr-$dae, $ysm));
    RenderPolygon  ($svg, $timing, $BLACK, $VWIDTH, 1, ($tr-$daa, $ysm), ($tr-$dax, $ysm+$day/2), ($tr-$dax, $ysm-$day/2));
    RenderPolyline ($svg, $timing, "| ",   $VWIDTH, 1, ($ts+$dar, $ysm), ($ts+$dae, $ysm));
    RenderPolygon  ($svg, $timing, $BLACK, $VWIDTH, 1, ($ts+$daa, $ysm), ($ts+$dax, $ysm+$day/2), ($ts+$dax, $ysm-$day/2));
  } else {
    RenderPolyline ($svg, $timing, "| ", $VWIDTH, 1, ($tr+$dar, $ysm), ($ts-$das, $ysm));
    if ($timref->arrowr) {
      RenderPolygon ($svg, $timing, $BLACK, $VWIDTH, 1, ($tr+$daa, $ysm), ($tr+$dax, $ysm+$day/2), ($tr+$dax, $ysm-$day/2));
    }
    if ($timref->arrows) {
      RenderPolygon ($svg, $timing, $BLACK, $VWIDTH, 1, ($ts-$daa, $ysm), ($ts-$dax, $ysm+$day/2), ($ts-$dax, $ysm-$day/2));
    }
  }

  # ... and timing text, if any!
  if ($timref->text) {
    my ($tx, $ta, $tw);
    # Justify text right, to the outer left of timing reference
    if ($timref->align < 0) {
      $tx = (($tr < $ts) ? $tr - $dae - $dax/2 : $ts + $dae + $dax/2);
      $ta = +1;
      $tw = 0;
    }
    # Justify text left, to the outer right of timing reference
    elsif ($timref->align > 0) {
      $tx = (($tr < $ts) ? $ts + $dae + $dax/2 : $tr - $dae - $dax/2);
      $ta = -1;
      $tw = 0;
    }
    # Center text within timing reference (but adjust for the arrows, if any)
    else {
      my $txr = $tr + (($timref->arrowr > 0) ? $dax : 0);
      my $txs = $ts - (($timref->arrows > 0) ? $dax : 0);
      $tx = ($txr + $txs) / 2;
      $ta = 0;
      $tw = abs ($txr - $txs);
    }
    RenderText ($svg, $timing, ($tx, $ysm), $timref->text, $ta, $FSIZET, $tw, 0, $WHITE)
  }

}

sub RenderSignal ($$$$$) {
  my ($svg, $timing, $signal, $yh, $yl) = @_;

  my $noline = $signal->noline;
  my $cycles = $timing->cycles;

  my $dx = $timing->sigedge;
  my $hx = $dx / 2;
  my $qx = $hx / 2;
  my $ym = ($yl + $yh) / 2.0;

  my $lpt = new WAVEPT ();
  my $rpt = new WAVEPT ();
  my $pt  = undef;

  # Show the signal name
  RenderText ($svg, $timing, UnratioX ($timing, -$SNAMEO) - $timing->dtl, $ym, $signal->name, +1, $FSIZES, 0);

  # Render the signal wave by wave
  foreach my $w (@{$signal->wave}) {
    my $c = $w->cycle;
    my $l = $w->logic;
    my $v = $w->value;
    my $a = $w->align;
    my $t = $w->time;
    my $j = $w->jitter;
    my $s;
    my $tv = ($l ? "" : $v);
    my $tt = (defined $w->timtag);

    $s = (defined $v) ? ($l ? $v : 'T') : ' ';
    debug ("  <%s SIGNAL> %s @ %.3f (%s)", !(defined $pt) ? "INIT  " : "RENDER", $s, $t, $l ? "logic" : '"' . $v . '"');

    # First initialize the wave, after which rendering can be started
    if (!(defined $pt)) {
      InitWavePoint  ($lpt, $s, $tv, $a, $j);
      FirstWavePoint ($lpt, $t, $yh, $ym, $yl);
      $pt = $lpt;
      next;
    }

    # Then compute new point and render wave
    InitWavePoint ($rpt, $s, $tv, $a, $j);
    my $h = $signal->hiatus;
    RenderWave ($svg, $timing, $lpt, $rpt, $t, $yh, $ym, $yl, $hx, $qx, $noline, $h);

    # Prepare left point for next wave (we swap $lpt & $rpt because rpt contains data for the next run)
    $pt = $lpt; $lpt = $rpt; $rpt = $pt;
  }

  # Render the signal tags one by one, if any
  foreach my $w (@{$signal->wave}) {
    RenderWaveTag ($svg, $timing, $w->time, $ym, $hx, $noline, $w->timtag);
  }

}

sub InitWavePoint ($$$$$) {
  my ($pt, $s, $v, $a, $j) = @_;

  $pt->s($s);
  $pt->v($v);
  $pt->a($a);
  $pt->j($j);
  @{$pt->plpt} = ();
  @{$pt->ltpt} = ();
  @{$pt->rtpt} = ();
}

sub FirstWavePoint ($$$$$) {
  my ($pt, $t, $yh, $ym, $yl) = @_;

  # Referring to high/medium/low points
  my (@pth, @ptm, @ptl, @npt);

  # Catch all possible cases
  given ($pt->s) {
    when (/^[1H]/)	{ @pth = ($t, $yh);                   @npt = (@pth      ); break; }
    when (/^Z/)		{ @ptm = ($t, $ym);                   @npt = (@ptm      ); break; }
    when (/^[0L]/)	{ @ptl = ($t, $yl);                   @npt = (@ptl      ); break; }
    when (/^[XWT]/)	{ @pth = ($t, $yh); @ptl = ($t, $yl); @npt = (@pth, @ptl); break; }
    default { }
  }

  @{$pt->plpt} = @npt;
  @{$pt->rtpt}  = ($t, $ym);
}

sub RenderWave ($$$$$$$$$$$) {
  my ($svg, $timing, $ppt, $pt, $t, $yh, $ym, $yl, $dhx, $dqx, $noline, $hiatus) = @_;

  my ($hx, $qx) = $noline ? (0, 0) : ($dhx, $dqx);

  # Referring to low/medium/high points
  my (@ptl, @ptm, @pth);

  # Referring to (closing?) line/polygon/segment points
  my (@clpt, @cppt);

  # Referring to next/left/right line points ("next" normally is "left", except when polygons...)
  my (@nlpt, @ltpt, @rtpt);

  # Special stuff for handling jitter
  my (@cspt, @xlpt);

  # Determine the ending/starting points, we catch all possible cases (49 total, 'T' & '=' skipped)
  my $ss = $ppt->s . $pt->s;

  # If we have jitter, adjust (in advance) current value to previous one (we reuse/rotate the points)
  if (defined ($pt->j)) {
    $ss = $ss . $pt->j;
    if ($pt->j ne 'e') {
      $pt->s($ppt->s);
    }
  }

  given ($ss) {

    # Into void
    when (/^[1H] /)      { @pth = ($t, $yh);                                                 @clpt = (@pth);             @nlpt = ();     @ltpt = (@pth); @rtpt = ();     break; }
    when (/^Z /)         { @ptm = ($t, $ym);                                                 @clpt = (@ptm);             @nlpt = ();     @ltpt = (@ptm); @rtpt = ();     break; }
    when (/^[0L] /)      { @ptl = ($t, $yl);                                                 @clpt = (@ptl);             @nlpt = ();     @ltpt = (@ptl); @rtpt = ();     break; }
    when (/^[XWT] /)     { @ptm = ($t, $ym); @pth = ($t-$hx, $yh); @ptl = ($t-$hx, $yl);     @cppt = (@ptl, @ptm, @pth); @nlpt = ();     @ltpt = (@ptm); @rtpt = ();     break; }

    # Out of void
    when (/^ [1H]/)      { @pth = ($t, $yh);                                                 @clpt = ();     @nlpt = (@pth);             @ltpt = ();     @rtpt = (@pth); break; }
    when (/^ Z/)         { @ptm = ($t, $ym);                                                 @clpt = ();     @nlpt = (@ptm);             @ltpt = ();     @rtpt = (@ptm); break; }
    when (/^ [0L]/)      { @ptl = ($t, $yl);                                                 @clpt = ();     @nlpt = (@ptl);             @ltpt = ();     @rtpt = (@ptl); break; }
    when (/^ [XWT]/)     { @ptm = ($t, $ym); @pth = ($t+$hx, $yh); @ptl = ($t+$hx, $yl);     @cppt = ();     @nlpt = (@pth, @ptm, @ptl); @ltpt = ();     @rtpt = (@pth); break; }

    # Constant (9 = 4+1+4)
    when (/^[1H][1H]/)   { @pth = ($t, $yh);                                                 @clpt = (@pth); @nlpt = (@pth);             @ltpt = (@pth); @rtpt = (@pth); break; }
    when (/^ZZ/)         { @ptm = ($t, $ym);                                                 @clpt = (@ptm); @nlpt = (@ptm);             @ltpt = (@ptm); @rtpt = (@ptm); break; }
    when (/^[0L][0L]/)   { @ptl = ($t, $yl);                                                 @clpt = (@ptl); @nlpt = (@ptl);             @ltpt = (@ptl); @rtpt = (@ptl); break; }

    # Any but unknown/text to unknown/text (10 = 4+2+4, 'T' skipped)
    when (/^[1H][XWT]/)  { @pth = ($t-$hx, $yh); @ptl = ($t+$hx, $yl);                       @clpt = (@pth); @nlpt = (@pth, @ptl);       @ltpt = (@pth); @rtpt = (@ptl); break; }
    when (/^Z[XWT]/)     { @ptm = ($t-$qx, $ym); @pth = ($t+$qx, $yh); @ptl = ($t+$qx, $yl); @clpt = (@ptm); @nlpt = (@pth, @ptm, @ptl); @ltpt = (@ptm); @rtpt = (@pth); break; }
    when (/^[0L][XWT]/)  { @ptl = ($t-$hx, $yl); @pth = ($t+$hx, $yh);                       @clpt = (@ptl); @nlpt = (@pth, @ptl);       @ltpt = (@ptl); @rtpt = (@pth); break; }

    # Fall (8 = 4+2+2)
    when (/^[1H][0L]/)   { @pth = ($t-$hx, $yh); @ptl = ($t+$hx, $yl);                       @clpt = (@pth); @nlpt = (@pth, @ptl);       @ltpt = (@pth); @rtpt = (@ptl); break; }
    when (/^[1H]Z/)      { @pth = ($t-$qx, $yh); @ptm = ($t+$qx, $ym);                       @clpt = (@pth); @nlpt = (@pth, @ptm);       @ltpt = (@pth); @rtpt = (@ptm); break; }
    when (/^Z[0L]/)      { @ptm = ($t-$qx, $ym); @ptl = ($t+$qx, $yl);                       @clpt = (@ptm); @nlpt = (@ptm, @ptl);       @ltpt = (@ptm); @rtpt = (@ptl); break; }

    # Rise (8 = 2+2+4)
    when (/^[0L][1H]/)   { @ptl = ($t-$hx, $yl); @pth = ($t+$hx, $yh);                       @clpt = (@ptl); @nlpt = (@ptl, @pth);       @ltpt = (@ptl); @rtpt = (@pth); break; }
    when (/^[0L]Z/)      { @ptl = ($t-$qx, $yl); @ptm = ($t+$qx, $ym);                       @clpt = (@ptl); @nlpt = (@ptl, @ptm);       @ltpt = (@ptl); @rtpt = (@ptm); break; }
    when (/^Z[1H]/)      { @ptm = ($t-$qx, $ym); @pth = ($t+$qx, $yh);                       @clpt = (@ptm); @nlpt = (@ptm, @pth);       @ltpt = (@ptm); @rtpt = (@pth); break; }

    # Unknown/text to any but unknown/text (10 = 4+2+4, 'T' skipped)
    when (/^[XWT][1H]/)  { @ptl = ($t-$hx, $yl); @pth = ($t+$hx, $yh);                       @cppt = (@ptl, @pth);       @nlpt = (@pth); @ltpt = (@ptl); @rtpt = (@pth); break; }
    when (/^[XWT]Z/)     { @ptm = ($t+$qx, $ym); @pth = ($t-$qx, $yh); @ptl = ($t-$qx, $yl); @cppt = (@ptl, @ptm, @pth); @nlpt = (@ptm); @ltpt = (@pth); @rtpt = (@ptm); break; }
    when (/^[XWT][0L]/)  { @pth = ($t-$hx, $yh); @ptl = ($t+$hx, $yl);                       @cppt = (@ptl, @pth);       @nlpt = (@ptl); @ltpt = (@pth); @rtpt = (@ptl); break; }

    # Unknown/text to unknown/text (4, 'T' skipped)
    when (/^[XWT][XWT]/) { @ptm = ($t,     $ym); @pth = ($t-$hx, $yh); @ptl = ($t-$hx, $yl); @cppt = (@ptl, @ptm, @pth);                 @ltpt = (@pth);
                           @pth = ($t+$hx, $yh); @ptl = ($t+$hx, $yl);                       @nlpt = (@pth, @ptm, @ptl);                 @rtpt = (@pth); break; }

    # Tail
    when (/^[1H]=/)      { @pth = ($t, $yh);                                                 @clpt = (@pth      ); @ltpt = (@pth); break; }
    when (/^Z=/)         { @ptm = ($t, $ym);                                                 @clpt = (@ptm      ); @ltpt = (@ptm); break; }
    when (/^[0L]=/)      { @ptl = ($t, $yl);                                                 @clpt = (@ptl      ); @ltpt = (@ptl); break; }
    when (/^[XWT]=/)     { @pth = ($t, $yh); @ptl = ($t, $yl);                               @cppt = (@ptl, @pth); @ltpt = (@pth); break; }

    # Normally never reached...
    default { }
  }

  # Reprocess the point(s) list(s) in special case of "jitter" events (begin/middle/end)
  given ($ss) {

    when (   /^[0LZ][H1]b/
	  || /^[0L]Zb/
	  || /^[1HZ][L0]b/
	  || /^[1H]Zb/     ) { @cspt = @nlpt; @xlpt = @nlpt[2,3];    @nlpt = @{$ppt->plpt}; @clpt = (); break; }
    when (   /^[0LZ][H1]m/
	  || /^[0L]Zm/
	  || /^[1HZ][L0]m/
	  || /^[1H]Zm/     ) { @cspt = @nlpt; @xlpt = @{$ppt->xlpt}; @nlpt = @{$ppt->plpt}; @clpt = (); break; }
    when (   /^[0LZ][H1]e/
	  || /^[0L]Ze/
	  || /^[1HZ][L0]e/
	  || /^[1H]Ze/     ) { @cspt = @nlpt; @xlpt = ();            @nlpt = @{$ppt->xlpt};             break; }

    # Normally never reached...
    default { }
  }

  # Render polyline(s) and/or polygon if "allowed"
  if (!$noline) {
    if    (@cspt) { my $sx = substr ($ss, 1, 1) . "x";
		    RenderPolyline ($svg, $timing, $sx, $SWIDTH, 0, @cspt               ); }
    if    (@clpt) { RenderPolyline ($svg, $timing, $ss, $SWIDTH, 0, @{$ppt->plpt}, @clpt); }
    elsif (@cppt) { RenderPolygon  ($svg, $timing, $ss, $SWIDTH, 0, @{$ppt->plpt}, @cppt); }
  }
  @{$pt->plpt} = @nlpt;
  @{$pt->xlpt} = @xlpt;
  debug ("   <RENDER WAVE> %2s: nlpt = (%s)", $ss, DumpPoints (@nlpt));

  # Force text to be verticaly-centered...
  @ltpt[1] = $ym;
  @rtpt[1] = $ym;
  @{$pt->ltpt}  = @ltpt;
  @{$pt->rtpt}  = @rtpt;

  # Render text for this wave (maybe)
  if ($ppt->v ne "") {

    # Clip left and right text points
    my ($cnt, @cpt) = ClipPathLR (0-$timing->dtl, $timing->cycles+$timing->dtr, 4, @{$ppt->rtpt}, @{$pt->ltpt});
    my @ptl = @cpt[0,1]; # left  point
    my @ptr = @cpt[2,3]; # right point

    # Identify any hiatus that may lie in-between and insert each of them with left & right margins...
    my @plist;
    my $hxo = BlendRatioUnratioX ($timing, $HIATUSO, @HIATUSB);
    foreach my $h (@{$hiatus}) {
      my $ht = $h->time;
      if (($ptl[0] < $ht) && ($ht < $ptr[0])) {
	push (@plist, $ht-$hxo, $ym, $ht+$hxo, $ym);
      }
    }
    push (@plist, @ptr);

    # Iterate over the points list (contains any identified hiatus with margins)
    while (@plist) {

      @ptr = (shift (@plist), shift (@plist));

      # Perform text alignment and render it
      my $a = $ppt->a;
      my $w = ($ptr[0] - $ptl[0]);
      my @p;
      if    ($a < 0) { @p = @ptl; }
      elsif ($a > 0) { @p = @ptr; }
      else           { @p = (($ptl[0] + $ptr[0])/2, $ptr[1]); }
      RenderText ($svg, $timing, @p, $ppt->v, $a, $FSIZEV, $w, $noline ? 0 : $TT);

      @ptl = (shift (@plist), shift (@plist));
    }
  }

}

sub RenderWaveTag ($$$$$$$) {
  my ($svg, $timing, $x, $y, $dx, $noline, $timtag) = @_;

  # Render a timref tag, if any
  if ($timtag && !$noline) {
    my @ttl = ($x - $dx, $y);
    my @ttr = ($x + $dx, $y);
    RenderPolyline ($svg, $timing, 0, $VWIDTH, 0, @ttl, @ttr);
  }

}

sub RenderCommon ($) {
  my ($svg) = @_;

##  <svg xmlns='http://www.w3.org/2000/svg' font-size='24'>
##    <filter id='f' x='0' y='0' width='100%' height='100%'>
##      <feFlood flood-color='yellow' result='bg'/>
##      <feMerge>
##        <feMergeNode in='bg'/>
##        <feMergeNode in='SourceGraphic'/>
##      </feMerge>
##    </filter>
##    <text x='20' y='100' filter='url(#f)'>Hello there</text>
##  </svg>

  my $bgText = $svg->filter (
    id     => 'bgText',
    filterUnits => 'objectBoundingBox',
    x      => '0%',
    y      => '0%',
    width  => '100%',
    height => '100%',
  );

  my $flood = $bgText->fe ('-type' => 'flood', 'flood-color' => ($DSVG ? $GBLUE : $WHITE), 'result' => 'bg');
  my $merge = $bgText->fe ('-type' => 'merge');
  $merge->fe ('-type' => 'mergenode', in => 'bg');
  $merge->fe ('-type' => 'mergenode', in => 'SourceGraphic');

}

sub RenderPolyline ($$$$$@) {
  my ($svg, $timing, $ss, $lw, $nc, @pt) = @_;

  debug ("    <RENDER POLYLINE> %2s: %s", $ss, DumpPoints (@pt));

  my $dw = RenderDX ($timing, $lw);
  my $da = "$dw, " . $dw*2;
  {
    # Clip polyline, unless asked not to... (offset limits inward by 1/2 line-width!)
    my $hlwx = UnratioX ($timing, $lw)/2;
    my ($cnt, @cpt) = $nc ? (0, @pt) : ClipPolylineLR (0-$timing->dtl+$hlwx, $timing->cycles+$timing->dtr-$hlwx, @pt);

    # If we got a count list, then we have a segment list: render each one separately
    if ($cnt) {
      my $i = 0;

      # Each index in the count list is the start of a segment
      foreach my $c (split (/,/, $cnt)) {
	my ($f, $t) = ($i, $i+$c-1);
	debug ("      <POLYLINE> %d: %s", $c, DumpPoints (@cpt[$f .. $t]));
	$svg->polyline (
	  points => join (",", RenderXY ($timing, @cpt[$f .. $t])),
	  style => {
	    'fill'              => 'none', # none or #rrggbb
	    'stroke'            => ($ss =~ /$re_color/o) ? $ss : $BLACK, # none or #rrggbb
	    'stroke-width'      => $dw,
	    'stroke-dasharray'  => (($ss =~ /$re_ss_dash/o) ? "$da" : 'none'),
	    'stroke-dashoffset' => '0',
	    'stroke-miterlimit' => $dw,
	    'stroke-linecap'    => 'round',
	    'stroke-linejoin'   => 'round',
	  } );
	$i += $c;
      }
    }

    # If we got no count list, make sure we have a path and render it straight
    elsif (@cpt) {
      debug ("      <POLYLINE> %s", DumpPoints (@cpt));
      $svg->polyline (
	points => join (",", RenderXY ($timing, @cpt)),
	style => {
	  'fill'              => 'none', # none or #rrggbb
	  'stroke'            => ($ss =~ /$re_color/o) ? $ss : $BLACK, # none or #rrggbb
	  'stroke-width'      => $dw,
	  'stroke-dasharray'  => (($ss =~ /$re_ss_dash/o) ? "$da" : 'none'),
	  'stroke-dashoffset' => '0',
	  'stroke-miterlimit' => $dw,
	  'stroke-linecap'    => 'round',
	  'stroke-linejoin'   => 'round',
	} );
    }

    # Too bad, clipping yielded nothing...
    else {
      debug ("      <POLYLINE> 0: []");
    }
  }
}

sub RenderPolygon ($$$$$@) {
  my ($svg, $timing, $ss, $lw, $nc, @pt) = @_;

  debug ("    <RENDER POLYGON> %2s: %s", $ss, DumpPoints (@pt));

  my $dw  = RenderDX ($timing, $lw);
  my $da = "$dw, " . $dw*2;
  {
    # Clip polygon, unless asked not to...
    my $hlwx = UnratioX ($timing, $lw)/2;
    my ($cnt, @cpt) = $nc ? (0, @pt) : ClipPolygonLR (0-$timing->dtl+$hlwx, $timing->cycles+$timing->dtr-$hlwx, @pt);

    # If we got a count list, then we have a polygon and a segment list: render each one separately...
    if ($cnt) {
      my $i = 0;

      # First render a "naked" polygon with the whole path (fill but no stroke)
      debug ("      <POLYGON> FILL %s", DumpPoints (@cpt));
      $svg->polygon (
	points => join (",", RenderXY ($timing, @cpt)),
	style => {
	  'fill'   => ($ss =~ /$re_color/o) ? $ss : (($ss =~ /[XW]./) ? $GRAY : $WHITE), # none or #rrggbb
	  'stroke' => 'none',	# none or #rrggbb
	}
	  );

      # Each index in the count list is the start of a segment
      foreach my $c (split (/,/, $cnt)) {
	my ($f, $t) = ($i, $i+$c-1);
	debug ("      <POLYGON> STROKE %d: %s", $c, DumpPoints (@cpt[$f .. $t]));
	$svg->polyline (
	  points => join (",", RenderXY ($timing, @cpt[$f .. $t])),
	  style => {
	    'fill'              => 'none',	 # none or #rrggbb
	    'stroke'            => ($ss =~ /$re_color/o) ? $ss : $BLACK, # none or #rrggbb
	    'stroke-width'      => $dw,
	    'stroke-dasharray'  => (($ss =~ /$re_ss_dash/o) ? "$da" : 'none'),
	    'stroke-dashoffset' => '0',
	    'stroke-miterlimit' => $dw,
	    'stroke-linecap'    => 'round',
	    'stroke-linejoin'   => 'round',
	  } );
	$i += $c;
      }
    }

    # If we got no count list, make sure we have a path and render a full polygon (fill & stroke)
    elsif (@cpt) {
      debug ("      <POLYGON> FILL-STROKE %s", DumpPoints (@cpt));
      $svg->polygon (
	points => join (",", RenderXY ($timing, @cpt)),
	style => {
	  'fill'              => ($ss =~ /$re_color/o) ? $ss : (($ss =~ /[XW]./) ? $GRAY : $WHITE), # none or #rrggbb
	  'stroke'            => ($lw ? (($ss =~ /$re_color/o) ? $ss : $BLACK) : 'none'), # none or #rrggbb
	  'stroke-width'      => $dw,
	  'stroke-dasharray'  => (($ss =~ /$re_ss_dash/o) ? "$da" : 'none'),
	  'stroke-dashoffset' => '0',
	  'stroke-miterlimit' => $dw,
	  'stroke-linecap'    => 'round',
	  'stroke-linejoin'   => 'round',
	}
	  );
    }

    # Too bad, clipping yielded nothing...
    else {
      debug ("      <POLYGON> 0: []");
    }
  }
}

sub RenderText ($$$$$$$$@) {
  my ($svg, $timing, $x, $y, $text, $justif, $fs, $width, $tol, $bg) = @_;

  my @anchor = ('start', 'middle', 'end');

  my ($rx, $ry) = RenderXY ($timing, $x, $y);
  my $fsize = RenderDX ($timing, $fs);

  debug ("    <RENDER TEXT> \"%s\" @ [%.3f %.3f], align @ %s", $text, $x, $y, $anchor[$justif+1]);

  # Estimate the text bounding box
  my ($tl, $tw, $tr) = TextWidth ($text, $fs);
  my ($tleft, $twidth, $tright) = (UnratioX ($timing, $tl), UnratioX ($timing, $tw), UnratioX ($timing, $tr));
  my ($dwl, $dwr);
  if ($justif < 0) {
    ($dwl, $dwr) = (-$tleft, $tleft+$twidth);
  }
  if ($justif == 0) {
    ($dwl, $dwr) = ($twidth/2, $twidth/2);
  }
  if ($justif > 0) {
    ($dwl, $dwr) = ($twidth+$tright, -$tright);
  }
  my ($txl, $tyh, $txr, $tyl) = ($x-$dwl, $y-$fs/2, $x+$dwr, $y+$fs/2);

  # Make sure we have enough room to render the text
  if (($width > 0) && ($width < $twidth*(1-$tol))) {
    warning ("not enough width to render text \"%s\" @ (%s,%s) [%s < %s-%s%%]", $text, $x, $y, $width, $twidth, 100*$tol);
  } else {

    # Add an opaque background if asked to
    if ($bg =~ /$re_color/o) {
      RenderPolygon ($svg, $timing, ($DSVG ? $GREEN : $bg), 0, 1, ($txl, $tyh), ($txr, $tyh), ($txr, $tyl), ($txl, $tyl));
    }

    # Render text, shifting origin "down" by 1/3 of the font size (approx.)
    $svg->text (
      x => $rx,
      y => $ry+$fsize*$FVORATIO,
      style => {
        'font-family' => $FNAME,
        'font-size'   => $fsize,
        'text-anchor' => $anchor[$justif+1],
      },
#      filter => 'url(#bgText)',
    )->cdata($text);

  }

  # If debugging SVG, show (estimated) text box and text alignment markers
  if ($DSVG) {
    my $tmw = $fs/100;

    RenderPolyline ($svg, $timing, $RED, $tmw, 1, ($txl, $tyh), ($txr, $tyh), ($txr, $tyl), ($txl, $tyl), ($txl, $tyh));

    my ($dx, $tmdx, $tmdy) = (($justif == 0) ? $width/2 : 0, UnratioX($timing, $fs/10), $fs/10);
    if ($justif <= 0) {
      RenderPolygon ($svg, $timing, $RED, 0, 1, ($x-$dx,$y-$tmdy), ($x-$dx,$y+$tmdy), ($x-$dx+$tmdx,$y));
    }
    if ($justif == 0) {
      RenderPolyline ($svg, $timing, $RED, $tmw, 1, ($x-$tmdx,$y), ($x+$tmdx,$y));
      RenderPolyline ($svg, $timing, $RED, $tmw, 1, ($x,$y-$tmdy), ($x,$y+$tmdy));
    }
    if ($justif >= 0) {
      RenderPolygon ($svg, $timing, $RED, 0, 1, ($x+$dx,$y-$tmdy), ($x+$dx,$y+$tmdy), ($x+$dx-$tmdx,$y));
    }
  }
}

sub RenderHiatus ($$$$$) {
  my ($svg, $timing, $fc, $sc, $lw, $x, $y, $w, $h) = @_;

  debug ("    <RENDER HIATUS> %d:%d", $x, $y);

  {
    # Compute full-width, half-width, half-height & quad-height
    my $bw = BlendRatioUnratioX ($timing, $w, @HIATUSB);
    my ($fw, $hw, $hh, $qh) = ($bw, $bw/2, $h/2, $h/4);
##    my ($xl, $xr)      = ($x-$lw, $x+$lw);

    # Compute points for left & right side of the hiatus (as 2 joined cubic bezier curves)
    my @hl = RenderXY ($timing,
		       $x-$hw,$y-$hh, $x-$fw,$y-$qh, $x-$hw,$y,
		       $x    ,$y+$qh, $x-$hw,$y+$hh, $x-$hw,$y+$hh);
    my @hr = RenderXY ($timing,
		       $x+$hw,$y+$hh, $x+$fw,$y+$qh, $x+$hw,$y,
		       $x    ,$y-$qh, $x+$hw,$y-$hh, $x+$hw,$y-$hh);

    # Compute hiatus path & side polylines (being as much efficient as can be!)
    my $hlp = sprintf ("%s,%s C %s,%s %s,%s %s,%s %s,%s %s,%s %s,%s", @hl[0,1], @hl);
    my $hrp = sprintf ("%s,%s C %s,%s %s,%s %s,%s %s,%s %s,%s %s,%s", @hr[0,1], @hr);

    my $hl = "M " . $hlp;
    my $hr = "M " . $hrp;
    my $hp = "M " . $hlp . " L " . $hrp . " Z";

    # First render a "naked" figure with the whole path (fill but no stroke)
    debug ("      <PATH> FILL %s", $hp);
    $svg->path (
      d => $hp,
      style => {
	'fill' => ($fc =~ /$re_color/o) ? $fc : $WHITE,
	'stroke' => ($fc =~ /$re_color/o) ? $fc : $WHITE, # none or #rrggbb
	'stroke-width' => RenderDX ($timing, $lw*2.5),
	#'stroke' => 'none',	# none or #rrggbb
      } );

    # Then render the left & right waves (stroke but not fill)
    debug ("      <PATH> STROKE %s", $hl);
    $svg->path (
      d => $hl,
      style => {
	'fill' => 'none',	 # none or #rrggbb
	'stroke' => ($sc =~ /$re_color/o) ? $sc : $BLACK, # none or #rrggbb
	'stroke-width' => RenderDX ($timing, $lw),
	'stroke-dasharray' => 'none',
	'stroke-linecap' => 'round',
	'stroke-linejoin' => 'round',
      } );
    debug ("      <PATH> STROKE %s", $hr);
    $svg->path (
      d => $hr,
      style => {
	'fill' => 'none',	 # none or #rrggbb
	'stroke' => ($sc =~ /$re_color/o) ? $sc : $BLACK, # none or #rrggbb
	'stroke-width' => RenderDX ($timing, $lw),
	'stroke-dasharray' => 'none',
	'stroke-linecap' => 'round',
	'stroke-linejoin' => 'round',
      } );
  }
}

sub RenderXY ($@) {
  my ($t, $x, $y, @pt) = @_;

  my $rx = $t->margin + ($t->sx + ($x+$t->dtl)*$t->wcratio) * $t->scale;
  my $ry = $t->margin + ($t->sy +  $y                     ) * $t->scale;

  return ($rx, $ry, @pt ? RenderXY ($t, @pt) : ());
}

sub RenderDX ($$) {
  my ($t, $dx) = @_;
  return ($dx * $t->scale);
}

sub RenderDY ($$) {
  my ($t, $dy) = @_;
  return ($dy * $t->scale);
}

sub RenderWH ($$$) {
  my ($t, $w, $h) = @_;

  my ($rw, $rh) = RenderXY ($t, $w+$t->dtr, $h);

  return ($rw + $t->margin, $rh + $t->margin);
}

sub UnscaleX ($$) {
  my ($t, $x) = @_;
  return ($x / $t->scale);
}

sub UnscaleY ($$) {
  my ($t, $y) = @_;
  return ($y / $t->scale);
}

sub UnratioX ($$) {
  my ($t, $x) = @_;
  return ($x / $t->wcratio);
}

sub UnratioY ($$) {
  my ($t, $y) = @_;
  return ($y);
}

sub BlendRatioUnratioX ($$$$) {
  my ($t, $x, $rf, $uf) = @_;
  return ($x * ($rf + $uf/$t->wcratio)/($rf + $uf));
}

sub ClipPathLR ($$@) {
  my ($lx, $rx, $minpt, @pt) = @_;

  debug ("        <CLIP> path in range <%s %s>: %s", $lx, $rx, DumpPoints (@pt));

  # Not enough points yields nothing...
  return (0) if (@pt < $minpt);

  # Now process and check each segment against the L/R boundaries
  my @cpt = ();
  my @cnt = ();
  my $cnt = 0;
  while (@pt >= 4) {
    my ($x1, $y1, $x2, $y2, @rest) = @pt;
    my (@lpt, @rpt);

    debug ("        <CLIP>   segment %s", DumpPoints ($x1, $y1, $x2, $y2));

    # Next iteration is done with all but the first point, except if currently on last pair
    @pt = ($x2, $y2, @rest);

    # Simply skip out of bounds segment, including 1 bound-only touching
    next if (   (($x1 <= $lx) && ($x2 <  $lx))
	     || (($x1 <  $lx) && ($x2 <= $lx))
	     || (($x1 >  $rx) && ($x2 >= $rx))
	     || (($x1 >= $rx) && ($x2 >  $rx))
	    );

    # Clip left, interpolating segment that crosses the left boundary
    if (($x1 < $lx) || ($x2 < $lx)) {
      @lpt = ($lx, InterpolateY ($lx, $x1, $y1, $x2, $y2));
      debug ("        <CLIP>     interpolateLEFT  %s", DumpPoints (@lpt));
    }

    # Clip right, interpolating segment that crosses the right boundary
    if (($x1 > $rx) || ($x2 > $rx)) {
      @rpt = ($rx, InterpolateY ($rx, $x1, $y1, $x2, $y2));
      debug ("        <CLIP>     interpolateRIGHT %s", DumpPoints (@rpt));
    }

    # Clipping yields 2 new points (a polyline on its own)...
    if (@lpt && @rpt) {
      push (@cpt, ($x1 < $x2) ? (@lpt, @rpt) : (@rpt, @lpt));
      $cnt +=4; push (@cnt, $cnt); $cnt = 0;
      debug ("        <CLIP>       SINGLE: %s", DumpPoints (($x1 < $x2) ? (@lpt, @rpt) : (@rpt, @lpt)));
      next;
    }

    # ...or 1 new point...
    elsif (@lpt || @rpt) {
      # ...which is the start of the segment (add to the polyline)...
      if ( (($x1 < $x2) && @lpt) || (($x1 > $x2) && @rpt) ) {
	push (@cpt, @lpt, @rpt);
	$cnt +=2;
	debug ("        <CLIP>       START:  %s", DumpPoints (@lpt, @rpt));
      }
      # ...or the end of the segment (end this polyline)...
      else {
	push (@cpt, ($x1, $y1, @lpt, @rpt));
	$cnt +=4; push (@cnt, $cnt); $cnt = 0;
	debug ("        <CLIP>       FINISH: %s", DumpPoints ($x1, $y1, @lpt, @rpt));
	next;
      }
    }

    # or the original point (add to the polyline)
    else {
      push (@cpt, $x1, $y1);
      $cnt +=2;
      debug ("        <CLIP>       POINT:  %s", DumpPoints ($x1, $y1));
    }

    # Finish the line if no more points (out-of-bounds pt2 caught before...)
    if (!@rest) {
      push (@cpt, $x2, $y2);
      $cnt +=2;
      debug ("        <CLIP>       FINISH: %s", DumpPoints ($x2, $y2));
    }

  }

  # Return the list(s), prefixed by a non-null comma-separated list
  # of polyline(s) width (in points) if more than one...
  if (@cnt) {
    push (@cnt, $cnt) if ($cnt);
    $cnt = join (",", @cnt);
  } else {
    $cnt = 0;
  }
  return ($cnt, @cpt);

}

sub ClipPolylineLR ($$@) {
  my ($lx, $rx, @pt) = @_;

  # Return original point list if not clipped
  return ClipPathLR ($lx, $rx, 4, @pt);

}

sub ClipPolygonLR ($$@) {
  my ($lx, $rx, @pt) = @_;

  # Return original point list if not clipped
  return ClipPathLR ($lx, $rx, 6, @pt, $pt[0], $pt[1]);

}

sub InterpolateY ($$$$$) {
  my ($x, $x1, $y1, $x2, $y2) = @_;

  return ($x1 == $x2) ? (($y1 + $y2) / 2) : ($y1 + (($x - $x1) * ($y2 - $y1)) / ($x2 - $x1));
}

sub DumpPoints ($$@) {
  my ($x, $y, @pt) = @_;

  my $pt = "[$x $y]";
  return  (@pt ? "$pt " . DumpPoints (@pt) : "$pt") if $DEBUG;
}
